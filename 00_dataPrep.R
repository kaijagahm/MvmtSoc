# Prepare data for movement analysis
# Setup -------------------------------------------------------------------
## Load packages -----------------------------------------------------------
library(vultureUtils)
library(sf)
library(tidyverse)

## Load data ---------------------------------------------------------------
load("data/datAnnotCleaned.Rda")
roostPolygons <- sf::st_read("data/roosts25_cutOffRegion.kml") %>%
  sf::st_set_crs("WGS84") %>%
  sf::st_transform(32636)

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
         seasonUnique = case_when(season == "nb" ~ paste(season, year, sep = "_"),
                                  season == "b" & month == 12 ~ 
                                    paste(season, year+1, sep = "_"),
                                  TRUE ~ paste(season, year, sep = "_")))

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
                   group_by(trackId) %>%
                   summarize(geometry = st_union(geometry)) %>%
                   st_centroid())

## Get southern individuals for each season, so we can filter the data
southernIndivs <- map(centroids, ~.x %>%
                        filter(st_coordinates(.)[,2] < 3550000) %>%
                        pull(trackId))

## Remove individuals not in the south
seasons <- map2(.x = seasons, .y = southernIndivs, ~.x %>% filter(trackId %in% .y))

# Include only individuals with enough points per day ------------------------
## Theoretically, should only have 72 points per day--12 hours, 6 points per hour. That should vary a bit. I don't really understand why so many individuals/days have more than 100 points per day, but let's proceed for now.
# 72/3 = 24. For simplicity, let's restrict to individual/days with at least 25 points per day.

## Remove days on which individuals don't have at least 25 points.
seasons <- map(seasons, ~.x %>%
                 group_by(trackId, dateOnly) %>%
                 filter(n() >= 25) %>%
                 ungroup())

# Include only individuals with enough days tracked -----------------------
## Must be tracked for at least 1/3 of the number of days in the season.
durs <- map_dbl(seasons, ~length(unique(.x$dateOnly)))

indivsToKeep <- map2(.x = seasons, .y = durs, ~.x %>%
                       st_drop_geometry() %>%
                       group_by(trackId) %>%
                       summarize(nDaysTracked = length(unique(dateOnly)),
                                 propDaysTracked = nDaysTracked/.y) %>%
                       filter(propDaysTracked >= 0.33) %>%
                       pull(trackId))

## remove the individuals not tracked for long enough
seasons <- map2(.x = seasons, .y = indivsToKeep, ~.x %>% filter(trackId %in% .y))


# Get number of days tracked for each individual --------------------------
daysTracked_seasons <- map2(seasons, durs, ~.x %>%
                             st_drop_geometry() %>%
                             group_by(trackId) %>%
                             summarize(daysTracked = length(unique(dateOnly)),
                                       propDaysTracked = daysTracked/.y))

# Get roosts for each season ----------------------------------------------
roosts_seasons <- purrr::map(seasons, ~vultureUtils::get_roosts_df(df = .x, id = "trackId")) 

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
save(daysTracked_seasons, file = "data/daysTracked_seasons.Rda")
save(roosts_seasons, file = "data/roosts_seasons.Rda")
