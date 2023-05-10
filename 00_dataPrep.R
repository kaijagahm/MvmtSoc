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
base::load("movebankCredentials/pw.Rda")
MB.LoginObject <- move::movebankLogin(username = "kaijagahm", password = pw)
rm(pw)

# dat <- vultureUtils::downloadVultures(loginObject = MB.LoginObject, removeDup = T, dfConvert = T, quiet = T, dateTimeStartUTC = "2020-09-01 00:00", dateTimeEndUTC = "2022-11-30 11:59")
# write_feather(dat, "data/dat.feather")
dat <- read_feather("data/dat.feather")
## fix trackId
dat <- dat %>%
  mutate(trackId = as.character(trackId),
         trackId = case_when(trackId == "E03" ~ "E03w",
                             TRUE ~ trackId))

# Add the Nili_id
ww <- read_excel("data/whoswho_vultures_20230315_new.xlsx", sheet = "all gps tags")[,1:35] %>%
  dplyr::select(Nili_id, Movebank_id) %>%
  distinct()

all(dat$trackId %in% ww$Movebank_id) # true

dat2 <- left_join(dat, ww, by = c("trackId" = "Movebank_id"))

## annotate the data with periods to remove
toRemove <- read_excel("data/whoswho_vultures_20230315_new.xlsx", sheet = "periods_to_remove")

toRemove <- toRemove %>%
  dplyr::select(Nili_id,
                remove_start,
                remove_end,
                reason) %>%
  mutate(across(c(remove_start, remove_end), .fns = function(x){
    lubridate::ymd(x)
  })) %>%
  dplyr::filter(!is.na(remove_end))

toRemove_long <- toRemove %>%
  group_by(Nili_id, reason) %>%
  # sequence of daily dates for each corresponding start, end elements
  dplyr::mutate(dateOnly = map2(remove_start, remove_end, seq, by = "1 day")) %>%
  # unnest the list column
  unnest(cols = c(dateOnly)) %>%
  # remove any duplicate rows
  distinct() %>%
  dplyr::select(-c(remove_start, remove_end)) %>%
  rename("status" = reason)

# Join to the original data
datAnnot <- dat2 %>%
  left_join(toRemove_long, by = c("Nili_id", "dateOnly")) %>%
  mutate(status = replace_na(status, "valid"))
nrow(datAnnot) == nrow(dat2) #T

# Attach age and sex information
as <- read_excel("data/whoswho_vultures_20230315_new.xlsx", sheet = "all gps tags")[,1:35] %>%
  dplyr::select(Nili_id, birth_year, sex) %>%
  distinct()

datAnnot2 <- datAnnot %>%
  dplyr::select(-c("sex")) %>%
  left_join(as, by = "Nili_id")
nrow(datAnnot2) == nrow(datAnnot)

datAnnot <- datAnnot2 # a hack so the rest of the code will work

# Clean the data
## Region masking, downsampling, removal of speed outliers, setting altitude outliers to NA, etc.
mask <- sf::st_read("data/CutOffRegion.kml")
datAnnotCleaned <- vultureUtils::cleanData(dataset = datAnnot, mask = mask, inMaskThreshold = 0.33, removeVars = F, idCol = "Nili_id", downsample = F)
save(datAnnotCleaned, file = "data/datAnnotCleaned.Rda")

## Load data ---------------------------------------------------------------
load("data/datAnnotCleaned.Rda")

# Fix time zone so dates make sense ---------------------------------------
## Overwrite the dateOnly column from the new times
datAnnotCleaned <- datAnnotCleaned %>%
  mutate(timestampIsrael = lubridate::with_tz(timestamp, tzone = "Israel"),
         dateOnly = lubridate::date(timestampIsrael),
         month = lubridate::month(dateOnly),
         year = lubridate::year(dateOnly))

# Split into Marta's 3 seasons --------------------------------------------
s1 <- datAnnotCleaned %>%
  mutate(start_breeding = ifelse(month %in% c(1:5),
                                 paste(as.character(year-1), "-", "12", "-", "15", sep = ""),
                                 paste(as.character(year), "-", "12", "-", "15", sep = "")),
         start_summer = ifelse(month == 12,
                               paste(as.character(year + 1), "-", "05", "-", "15", sep = ""),
                               paste(as.character(year), "-", "05", "-", "15", sep = "")),
         start_fall = as.Date(paste(as.character(year), "-", "09", "-", "15", sep = "")),
         start_breeding = as.Date(start_breeding),
         start_summer = as.Date(start_summer),
         season = case_when(
           dateOnly >= start_breeding & dateOnly < start_summer ~ "breeding",
           dateOnly >= start_summer & dateOnly <= start_fall ~ "summer",
           dateOnly >= start_fall & dateOnly < start_breeding ~ "fall"))

s2 <- s1 %>%
  mutate(age = year - birth_year,
         sex = case_when(sex == "f/m" ~ "u",
                         TRUE ~ sex)) %>%
  mutate(age = ifelse(season == "breeding" & month %in% c(1, 12), age + 1, age)) %>%
  group_by(Nili_id, year, month) %>%
  mutate(age = ifelse(month == 2, min(age) + 1, age)) %>%
  mutate(age_group = case_when(age == 0 ~ "juv",
                               age >= 1 & age <= 4 ~ "sub",
                               age >= 5 ~ "adult",
                               TRUE ~ NA)) %>%
  mutate(seasonUnique = case_when(season %in% c("fall", "summer") ~ paste(year, season, sep = "_"),
                                  season == "breeding" & month == 12 ~ paste(year + 1, season, sep = "_"),
                                  season == "breeding" & month != 12 ~ paste(year, season, sep = "_"))) %>%
  # explicitly set the factor levels in temporal order
  mutate(seasonUnique = factor(seasonUnique, levels = c("2020_summer", "2020_fall", "2021_breeding", "2021_summer", "2021_fall", "2022_breeding", "2022_summer", "2022_fall")),
         season = factor(season, levels = c("breeding", "summer", "fall")))

# remove columns that were used in the season calculation
s3 <- s2 %>%
  dplyr::select(-c(month, start_breeding, start_summer, start_fall))

# Separate the seasons -----------------------------
seasons <- s3 %>%
  group_by(seasonUnique) %>%
  group_split(.keep = T)
# extract the season names in case we need a separate vector at some point.
seasonNames <- map_chr(seasons, ~as.character(.x$seasonUnique[1])) # ok good, these are in the right order

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
# The next step will be to remove individuals with too few points per day or too few days tracked. But we don't want those indivs removed for the *social* analysis, since those things will be accounted for with SRI and all individuals make up important parts of the social fabric. So, before I filter for ppd and for days tracked, going to save a copy to use for social analysis. 
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
             group_by(seasonUnique, season, Nili_id, dateOnly) %>%
             summarize(n = n()))

## Theoretically, should only have 72 points per day--12 hours, 6 points per hour. That should vary a bit. I don't really understand why so many individuals/days have more than 100 points per day, but let's proceed for now: going to downsample that to 10 minue intervals anyway, so it won't matter.

# Orr suggests to just restrict to vulture*days with >30 points per day. Let's see what that would look like.
ppd %>%
  mutate(Status = ifelse(n < 30, "removed", "kept")) %>%
  ggplot(aes(x = n))+
  geom_histogram(aes(fill = Status))+
  facet_wrap(~seasonUnique, scales ="free")+
  geom_vline(aes(xintercept = 30), col = "red")+
  scale_fill_manual(values = c("darkgray", "red"))+
  theme_classic()+
  ylab("# vulture*days")+
  xlab("# points per vulture*day")

ppd <- ppd %>%
  ungroup() %>%
  mutate(removed = ifelse(n < 30, T, F))

# what proportion of vulture*days are removed overall?
(propRemoved <- ppd %>%
    group_by(seasonUnique, season) %>%
    summarize(nTot = n(),
              nRemoved = sum(removed),
              propRemoved = nRemoved/nTot))
propRemoved %>%
  ggplot(aes(x = seasonUnique, y = propRemoved, col = season))+
  geom_point(size = 10)+
  theme_classic()+
  scale_color_manual(values = c("blue", "red", "darkorange"))

# I'm a bit concerned about this because it removes more data for the breeding season than for the non-breeding seasons (likely because less sunlight during those seasons)

# what about by individual over each season?
ppd %>%
  group_by(seasonUnique, season, Nili_id) %>%
  summarize(propRemoved = sum(removed)/n()) %>%
  ggplot(aes(x = seasonUnique, y = propRemoved))+
  geom_boxplot(aes(group = seasonUnique, fill = season))+
  geom_jitter(width = 0.2, alpha = 0.3, size = 2)+
  scale_fill_manual(name = "Season", values = c("blue", "red", "darkorange"))+
  theme_classic()+ # tells the same story
  ylab("Proportion of days removed")+
  xlab("")

# Remove nighttime points -------------------------------------------------
seasons <- map(seasons, ~{
  times <- suncalc::getSunlightTimes(date = unique(lubridate::date(.x$timestamp)),
                                     lat = 31.434306, lon = 34.991889,
                                     keep = c("sunrise", "sunset")) %>%
    dplyr::select("dateOnly" = date, sunrise, sunset)
  
  .x <- .x %>%
    dplyr::mutate(dateOnly = lubridate::ymd(dateOnly)) %>%
    dplyr::left_join(times, by = "dateOnly") %>%
    dplyr::mutate(daylight = ifelse(timestamp >= sunrise & timestamp <= sunset, "day", "night")) %>%
    dplyr::select(-c(sunrise, sunset)) %>%
    dplyr::filter(daylight == "day")
})

# re-calculate ppd
ppd <- map_dfr(seasons, ~.x %>%
                 st_drop_geometry() %>%
                 group_by(seasonUnique, season, Nili_id, dateOnly) %>%
                 summarize(n = n()))

ppd %>% # visualize ppd
  mutate(Status = ifelse(n < 30, "removed", "kept")) %>%
  ggplot(aes(x = n))+
  geom_histogram(aes(fill = Status))+
  facet_wrap(~seasonUnique, scales ="free")+
  geom_vline(aes(xintercept = 30), col = "red")+
  scale_fill_manual(values = c("darkgray", "red"))+
  theme_classic()+
  ylab("# vulture*days")+
  xlab("# points per vulture*day")

# Now most individuals have fewer than 100 points per day. I still see some really high ppd values for 2020 summer, which is probably because there were more burst sampling occasions in that season. Those will be removed when we downsample.

## Remove days on which individuals don't have at least 30 points.
# XXX DID NOT RUN THIS PART!
# seasons <- map(seasons, ~.x %>%
#                  group_by(Nili_id, dateOnly) %>%
#                  filter(n() >= 30) %>%
#                  ungroup())

# Include only individuals with enough days tracked -----------------------
## Must be tracked for at least 1/4 of the number of days in the season.
durs <- map_dbl(seasons, ~length(unique(.x$dateOnly)))

indivsToKeep <- map2(.x = seasons, .y = durs, ~.x %>%
                       st_drop_geometry() %>%
                       group_by(Nili_id) %>%
                       summarize(nDaysTracked = length(unique(dateOnly)),
                                 propDaysTracked = nDaysTracked/.y) %>%
                       filter(propDaysTracked >= 0.25) %>%
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

# Downsample the data and save the downsampled data
subsample <- function(df, idCol = "Nili_id", timestampCol = "timestamp", mins = 10){
  sub <- df %>%
    arrange(idCol, timestampCol) %>%
    group_by(.data[[idCol]]) %>%
    mutate(tc = cut(.data[[timestampCol]], breaks = paste(as.character(mins), "min", sep = " "))) %>%
    group_by(.data[[idCol]], "d" = lubridate::date(.data[[timestampCol]]), tc) %>%
    slice(1) %>%
    ungroup() %>%
    dplyr::select(-c("d", "tc"))
  return(sub)
}

# create downsampled data
seasons_10min <- map(seasons, ~subsample(df = .x, idCol = "Nili_id", timestampCol = "timestamp", mins = 10), .progress = T) # XXX start here--put this in the data prep section.
seasons_20min <- map(seasons, ~subsample(df = .x, idCol = "Nili_id", timestampCol = "timestamp", mins = 20), .progress = T)
seasons_30min <- map(seasons, ~subsample(df = .x, idCol = "Nili_id", timestampCol = "timestamp", mins = 30), .progress = T)
seasons_60min <- map(seasons, ~subsample(df = .x, idCol = "Nili_id", timestampCol = "timestamp", mins = 60), .progress = T)
seasons_120min <- map(seasons, ~subsample(df = .x, idCol = "Nili_id", timestampCol = "timestamp", mins = 120), .progress = T)

save(seasons_10min, file = "data/seasons_10min.Rda")
save(seasons_20min, file = "data/seasons_20min.Rda")
save(seasons_30min, file = "data/seasons_30min.Rda")
save(seasons_60min, file = "data/seasons_60min.Rda")
save(seasons_120min, file = "data/seasons_120min.Rda")
