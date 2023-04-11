# Prepare data for movement analysis
# Setup -------------------------------------------------------------------
## Load packages -----------------------------------------------------------
library(vultureUtils)
library(sf)
library(tidyverse)
library(move)
library(feather)
library(readxl)

## Download data from movebank
# download data from movebank (just a subset of the times for now)
# base::load("movebankCredentials/pw.Rda")
# MB.LoginObject <- move::movebankLogin(username = "kaijagahm", password = pw)
# rm(pw)

# dat <- vultureUtils::downloadVultures(loginObject = MB.LoginObject, removeDup = T, dfConvert = T, quiet = T, dateTimeStartUTC = "2021-12-01 00:00", dateTimeEndUTC = "2023-03-31 11:59")
# write_feather(dat, "data/dat.feather")
# dat <- read_feather("data/dat.feather")
# ## fix trackId
# dat <- dat %>%
#   mutate(trackId = as.character(trackId),
#          trackId = case_when(trackId == "E03" ~ "E03w",
#                              TRUE ~ trackId))
# 
# # Add the Nili_id
# ww <- read_excel("data/whoswho_vultures_20230315_new.xlsx", sheet = "all gps tags")[,1:35] %>%
#   dplyr::select(Nili_id, Movebank_id) %>%
#   distinct()
# 
# all(dat$trackId %in% ww$Movebank_id) # true
# 
# dat2 <- left_join(dat, ww, by = c("trackId" = "Movebank_id"))
# 
# ## annotate the data with periods to remove
# toRemove <- read_excel("data/whoswho_vultures_20230315_new.xlsx", sheet = "periods_to_remove")
# 
# toRemove <- toRemove %>%
#   dplyr::select(Nili_id,
#                 "trackId" = Movebank_id,
#                 remove_start,
#                 remove_end,
#                 reason) %>%
#   mutate(across(c(remove_start, remove_end), .fns = function(x){
#     lubridate::ymd(x)
#   })) %>%
#   dplyr::filter(!is.na(remove_end))
# 
# toRemove_long <- toRemove %>%
#   group_by(Nili_id, reason) %>%
#   # sequence of daily dates for each corresponding start, end elements
#   dplyr::mutate(dateOnly = map2(remove_start, remove_end, seq, by = "1 day")) %>%
#   # unnest the list column
#   unnest(cols = c(dateOnly)) %>% 
#   # remove any duplicate rows
#   distinct() %>%
#   dplyr::select(-c(remove_start, remove_end)) %>%
#   rename("status" = reason)
# 
# # Join to the original data
# datAnnot <- dat2 %>%
#   left_join(toRemove_long, by = c("Nili_id", "dateOnly")) %>%
#   mutate(status = replace_na(status, "valid"))
# nrow(datAnnot) == nrow(dat2) #T
# 
# # Clean the data
# ## Region masking, downsampling, removal of speed outliers, setting altitude outliers to NA, etc.
# mask <- sf::st_read("data/CutOffRegion.kml")
# datAnnotCleaned <- vultureUtils::cleanData(dataset = datAnnot, mask = mask, inMaskThreshold = 0.33, removeVars = F, idCol = "Nili_id", downsample = T)
# save(datAnnotCleaned, file = "data/datAnnotCleaned.Rda")

## Load data ---------------------------------------------------------------
load("data/datAnnotCleaned.Rda")

# Fix time zone so dates make sense ---------------------------------------
## Overwrite the dateOnly column from the new times
datAnnotCleaned <- datAnnotCleaned %>%
  mutate(timestampIsrael = lubridate::with_tz(timestamp, tzone = "Israel"),
         dateOnly = lubridate::date(timestampIsrael))

# Split into seasons ------------------------------------------------------
datAnnotCleaned <- datAnnotCleaned %>%
  mutate(month = lubridate::month(timestampIsrael),
         year = lubridate::year(timestampIsrael),
         season = case_when(month %in% 7:11 ~ "nb",
                            month %in% c(12, 1:6) ~ "b"),
         seasonUnique = case_when(season == "nb" ~ paste(year, season, sep = "_"),
                                  season == "b" & month == 12 ~ 
                                    paste(year+1, season, sep = "_"),
                                  TRUE ~ paste(year, season, sep = "_")))

# Separate the seasons -----------------------------
seasons <- datAnnotCleaned %>%
  group_by(seasonUnique) %>%
  group_split(.keep = T)
seasonNames <- map_chr(seasons, ~.x$seasonUnique[1])

# Restrict to southern individuals ----------------------------------------
# Based on previous investigations for the 2022 breeding and non-breeding seasons, have found that a good cutoff for southern vs. non-southern is 3550000 (in ITM)
## Transform to SF object, so we can get centroids
seasonsSF <- map(seasons, ~.x %>%
                   sf::st_as_sf(coords = c("location_long", "location_lat"), remove = F) %>%
                   sf::st_set_crs("WGS84") %>%
                   sf::st_transform(32636))

## Get centroids, so we can see who's "southern" for that season.
centroids <- map(seasonsSF, ~.x %>%
                   group_by(Nili_id) %>%
                   summarize(geometry = st_union(geometry)) %>%
                   st_centroid())

## Examine a histogram of centroid latitudes 
walk(centroids, ~hist(st_coordinates(.x)[,2])) # looks like 3550000 is generally a good cutoff point here.

## Get southern individuals for each season, so we can filter the data
southernIndivs <- map(centroids, ~.x %>%
                        filter(st_coordinates(.)[,2] < 3550000) %>%
                        pull(Nili_id))

## Remove individuals not in the south
seasons <- map2(.x = seasons, .y = southernIndivs, ~.x %>% filter(Nili_id %in% .y))

# Save copies for soc analysis --------------------------------------------
# We don't want to remove individuals with too few ppd or too few days tracked for the *social* analysis, since those things will be accounted for with SRI and all individuals make up important parts of the social fabric. So, before I filter for ppd and for days tracked, going to save a copy to use for social analysis. 
seasons_forSoc <- seasons
roosts_seasons_forSoc <- purrr::map(seasons_forSoc, ~vultureUtils::get_roosts_df(df = .x, id = "Nili_id")) 
roosts_seasons_forSoc <- roosts_seasons_forSoc %>%
  map(., ~st_as_sf(.x, crs = "WGS84", coords = c("location_long", "location_lat"), remove = F))

# Export the data
save(seasons_forSoc, file = "data/seasons_forSoc.Rda") # XXX re-do this one
save(roosts_seasons_forSoc, file = "data/roosts_seasons_forSoc.Rda")

# Include only individuals with enough points per day ------------------------
# How many points per day are we looking at?
ppd <- map_dfr(seasons, ~.x %>%
             st_drop_geometry() %>%
             group_by(seasonUnique, Nili_id, dateOnly) %>%
             summarize(n = n()))

ppd %>%
  ggplot(aes(x = n))+
  geom_histogram()+
  facet_wrap(~seasonUnique)+
  theme_classic() # XXX start here--how to identify too few ppd?

## Theoretically, should only have 72 points per day--12 hours, 6 points per hour. That should vary a bit. I don't really understand why so many individuals/days have more than 100 points per day, but let's proceed for now.
# Orr suggests to just restrict to individuals with >30 points per day.

## Remove days on which individuals don't have at least 30 points.
seasons <- map(seasons, ~.x %>%
                 group_by(Nili_id, dateOnly) %>%
                 filter(n() >= 30) %>%
                 ungroup())

# Include only individuals with enough days tracked -----------------------
## Must be tracked for at least 1/3 of the number of days in the season.
durs <- map_dbl(seasons, ~length(unique(.x$dateOnly)))

indivsToKeep <- map2(.x = seasons, .y = durs, ~.x %>%
                       st_drop_geometry() %>%
                       group_by(Nili_id) %>%
                       summarize(nDaysTracked = length(unique(dateOnly)),
                                 propDaysTracked = nDaysTracked/.y) %>%
                       filter(propDaysTracked >= 0.33) %>%
                       pull(Nili_id))

## remove the individuals not tracked for long enough
seasons <- map2(.x = seasons, .y = indivsToKeep, ~.x %>% filter(Nili_id %in% .y))

# Get roosts for each season ----------------------------------------------
roosts_seasons <- purrr::map(seasons, ~vultureUtils::get_roosts_df(df = .x, id = "Nili_id")) 

roosts_seasons <- roosts_seasons %>%
  map(., ~st_as_sf(.x, crs = "WGS84", coords = c("location_long", "location_lat")))

# Get elevation rasters ---------------------------------------------------
elevs_z09_seasons <- map(seasons, ~elevatr::get_elev_raster(.x %>% st_transform(crs = "WGS84"), z = 9))

groundElev_z09 <- map2(elevs_z09_seasons, seasons, ~raster::extract(x = .x, y = .y %>% st_transform(crs = "WGS84")))

seasons <- map2(.x = seasons, .y = groundElev_z09, 
                ~.x %>% mutate(height_above_ground = .y,
                               height_above_ground = case_when(height_above_ground <0 ~ 0,
                                                               TRUE ~ height_above_ground)))

# Export the data
save(seasons, file = "data/seasons.Rda")
save(roosts_seasons, file = "data/roosts_seasons.Rda")
