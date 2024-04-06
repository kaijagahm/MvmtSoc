# Script for calculating movement metrics

# Setup -------------------------------------------------------------------
## Load packages -----------------------------------------------------------
library(vultureUtils)
library(sf)
library(tidyverse)
library(igraph)
library(mapview) # for quick maps
library(adehabitatHR)
library(sp)
library(elevatr) # for getting elevation information
library(raster) # for getting elevation information
library(parallel) # for running long distance calculations in parallel
library(future) # for parallel computing
library(ctmm)
load("data/derived/cc.Rda")

distviz <- function(data, varname, seasoncol){
  p <- data %>%
    ggplot(aes(x = .data[[varname]],
               fill = factor(.data[[seasoncol]]),
               color = factor(.data[[seasoncol]])))+
    geom_histogram()+
    facet_wrap(~.data[[seasoncol]])+
    theme_classic()+
    theme(legend.position = "none")+
    xlab("")+
    ylab("County")+
    ggtitle(varname)
  print(p)
}

## Load data ---------------------------------------------------------------
base::load("data/dataPrep/downsampled_10min.Rda") # data rarefied to 10 minute intervals. Going to use that for everything.
base::load("data/dataPrep/season_names.Rda")
base::load("data/dataPrep/roosts.Rda")
roosts <- map(roosts, ~.x %>% group_by(Nili_id) %>% 
                        mutate(daysTracked = length(unique(roost_date))) %>% ungroup())
roostPolygons <- sf::st_read("data/raw/roosts50_kde95_cutOffRegion.kml")

durs <- map_dbl(downsampled_10min, ~{
  dates <- lubridate::ymd(.x$dateOnly)
  dur <- difftime(max(dates), min(dates), units = "days")
})

# Get number of days tracked for each individual --------------------------
daysTracked_seasons <- map2(downsampled_10min, durs, ~.x %>%
                              st_drop_geometry() %>%
                              group_by(Nili_id) %>%
                              summarize(daysTracked = length(unique(dateOnly)),
                                        propDaysTracked = daysTracked/.y))
names(daysTracked_seasons) <- season_names
save(daysTracked_seasons, file = "data/calcMovement/daysTracked_seasons.Rda")
load("data/calcMovement/daysTracked_seasons.Rda")
dts <- purrr::list_rbind(daysTracked_seasons, names_to = "season")

# VVV Visualize days tracked
distviz(dts, "propDaysTracked", "season")

# Convert to sf -----------------------------------------------------------
downsampled_10min_sf <- map(downsampled_10min, ~.x %>% sf::st_as_sf(coords = c("location_long", "location_lat"), crs = "WGS84"))
save(downsampled_10min_sf, file = "data/calcMovement/downsampled_10min_sf.Rda")
load("data/calcMovement/downsampled_10min_sf.Rda")

# Movement Behavior --------------------------------------------------------
## SPACE USE ---------------------------------------------------------------
load("data/akde/stats_w_50_df.Rda")
load("data/akde/stats_w_95_df.Rda")
akde_stats <- bind_rows(stats_w_50_df, stats_w_95_df)
row.names(akde_stats) <- NULL

areas <- akde_stats %>%
  dplyr::select("Nili_id" = ID,
                est, level, seasonUnique) %>%
  pivot_wider(id_cols = c(Nili_id, seasonUnique), names_from = level, names_prefix = "level_",
              values_from = "est") %>%
  rename("homeRange" = level_0.95,
         "coreArea" = level_0.5) %>%
  mutate(coreAreaFidelity = coreArea/homeRange)
areas_list <- areas %>%
  group_by(seasonUnique) %>%
  group_split()

# VVV Visualize core areas, home ranges, and ratios
distviz(areas, "homeRange", "seasonUnique")
distviz(areas, "coreArea", "seasonUnique")
distviz(areas, "coreAreaFidelity", "seasonUnique")

## ROOST SITE USE ----------------------------------------------------------
# Make sure that each roost point intersects only one (or 0) roost (multi)polygon(s)
table(unlist(map(roosts, ~as.numeric(lengths(st_intersects(.x, roostPolygons)))))) # should be only 0 or 1. Good!

# Get ID's of which polygon each roost location falls into
roostIDs <- map(roosts, ~as.numeric(st_intersects(x = .x, y = roostPolygons)))

roosts <- map2(roosts, roostIDs, ~.x %>% 
                         mutate(roostID = as.character(.y)) %>%
                         mutate(roostID = case_when(is.na(roostID) ~ paste(Nili_id, roost_date, sep = "_"), 
                                                    TRUE ~ roostID)))
### Roost Switches ----------------------------------------------------------
## Calculate sequences of roost locations
roostSequences <- map(roosts, ~.x %>%
                        st_drop_geometry() %>%
                        dplyr::select(Nili_id, roost_date, roostID) %>%
                        arrange(Nili_id, roost_date) %>%
                        group_by(Nili_id) %>%
                        mutate(switch = case_when(roostID == lag(roostID) ~ F,
                                                  TRUE ~ T)))

## Calculate whether, on each night, they switch or stay at the same roost
roostSwitches <- map(roostSequences, ~.x %>%
                       group_by(Nili_id) %>%
                       summarize(nSwitch = sum(switch),
                                 nStay = sum(!switch),
                                 nights = n(),
                                 propSwitch = nSwitch/nights) %>%
                       dplyr::select(Nili_id, propSwitch))

### Roost Diversity -------------------------------------------------------
## (Shannon index)
shannon <- map(roosts, ~.x %>%
                 st_drop_geometry() %>%
                 mutate(roostID = as.character(roostID),
                        roostID = case_when(is.na(roostID) ~ 
                                              paste(Nili_id, roost_date, sep = "_"),
                                            TRUE ~ roostID)) %>%
                 group_by(Nili_id, roostID) %>%
                 summarize(n = n()) %>%
                 ungroup() %>%
                 group_by(Nili_id) %>%
                 dplyr::select(Nili_id, roostID, n) %>%
                 mutate(nNights = sum(n),
                        uniqueRoosts = length(unique(roostID))) %>%
                 mutate(prop = n/nNights,
                        lnProp = log(prop),
                        item = prop*lnProp) %>%
                 group_by(Nili_id) %>%
                 summarize(shannon = -sum(item),
                           uniqueRoosts = uniqueRoosts[1]))

## MOVEMENT ----------------------------------------------------------------
### Daily Flight Duration ---------------------------------------------------
dfd <- map(downsampled_10min_sf, ~.x %>%
             st_drop_geometry() %>%
             group_by(Nili_id, dateOnly) %>%
             arrange(timestamp, .by_group = T) %>%
             mutate(flight = ifelse(ground_speed > 5, T, F),
                    lagTimestamp = lag(timestamp),
                    lagFlight = lag(flight)) %>%
             mutate(dur = case_when(flight & lagFlight ~ as.numeric(difftime(timestamp, lagTimestamp, units = "secs")),
                                    flight & !lagFlight ~ 0,
                                    !flight & lagFlight ~ as.numeric(difftime(timestamp, lagTimestamp, units = "secs")),
                                    is.na(flight)|is.na(lagFlight) ~ 0)) %>%
             group_by(Nili_id, dateOnly) %>%
             summarize(totalFlightDuration = as.numeric(sum(dur, na.rm = T))))

dfdSumm <- map(dfd, ~.x %>%
                 group_by(Nili_id) %>%
                 summarize(meanDFD = mean(totalFlightDuration, na.rm = T)))

names(dfdSumm) <- season_names
dfddf <- purrr::list_rbind(dfdSumm, names_to = "season") %>%
  mutate(meanDFD = meanDFD/60/60)
distviz(dfddf, "meanDFD", "season")

### Daily Distance Traveled/Mean Displacement -------------------------------

# set up parallel backend with 4 sessions (change this to match your machine)
future::plan(future::multicore, workers = 10)

# function to calculate displacements within a group
calc_displacements <- function(group) {
  start_point <- dplyr::first(group$geometry)
  group %>%
    dplyr::mutate(
      disp_from_start = as.vector(sf::st_distance(geometry, start_point)),
      dist_to_prev = as.vector(sf::st_distance(geometry, dplyr::lag(geometry, default = dplyr::first(geometry)), by_element = T))
    )
}

# function to calculate metrics for each individual and date
calc_metrics <- function(data){
  # split the data by Nili_id and dateOnly
  groups <- data %>%
    dplyr::group_by(Nili_id, dateOnly) %>%
    group_split()

  # run the distance calculations in parallel using furrr::future_map()
  disp_data <- furrr::future_map_dfr(groups, calc_displacements)

  # group the data by Nili_id and dateOnly, and calculate the metrics
  result <- disp_data %>%
    sf::st_drop_geometry() %>%
    dplyr::group_by(Nili_id, dateOnly) %>%
    arrange(timestamp, .by_group = T) %>%
    mutate(csDist = cumsum(replace_na(dist_to_prev, 0))) %>%
    dplyr::summarise(
      dmd = max(disp_from_start, na.rm = T),
      ddt = sum(dist_to_prev, na.rm = T),
      distToMaxPt = csDist[disp_from_start == max(disp_from_start, na.rm = T)],
      tort_dmd = distToMaxPt/dmd) # YIKES

  return(result)
}

# apply the calc_metrics() function to each season in the list using purrr::map()
dailyMovementList_seasons <- map(downsampled_10min_sf, calc_metrics, .progress = T)
save(dailyMovementList_seasons, file = "data/calcMovement/dailyMovementList_seasons.Rda")
load("data/calcMovement/dailyMovementList_seasons.Rda") 

mnMvmt <- map(dailyMovementList_seasons, ~.x %>%
                group_by(Nili_id) %>%
                summarize(meanDMD = mean(dmd, na.rm = T),
                          meanDDT = mean(ddt, na.rm = T),
                          mnTort = mean(tort_dmd, na.rm = T)))

mnMvmt[[1]] %>% is.na() %>% colSums() # yessssssssssssssss

names(mnMvmt) <- season_names
mm <- purrr::list_rbind(mnMvmt, names_to = "season")
distviz(mm, "meanDMD", "season")
distviz(mm, "meanDDT", "season") # huh, those distributions do seem to differ more...
distviz(mm, "mnTort", "season")

# Would be good to do this by season same as above

### Altitude -------------------------------------------------
# daily altitude stats
# Note that I've already cleaned outlier altitudes in 00_dataPrep.R. 
# Now just need to restrict to ground_speed > 5m/s
dailyAltitudes <- map(downsampled_10min_sf, ~.x %>%
                        st_drop_geometry() %>%
                        filter(ground_speed >= 5) %>% # we only care about the altitude of in-flight points.
                        group_by(Nili_id, dateOnly) %>%
                        summarize(meanAltitude = mean(height_above_ground, na.rm  =T)) %>%
                        mutate(meanAltitude = case_when(meanAltitude < 0 ~ NA,
                                                        TRUE ~ meanAltitude)))
names(dailyAltitudes) <- season_names

dailyAltitudes %>%
  purrr::list_rbind(names_to = "season") %>%
  ggplot(aes(x = meanAltitude, group = interaction(Nili_id, season), col = season))+
  geom_density(alpha = 0.5)+
  theme_classic()+
  ylab("")+
  xlab("Mean daily flight altitude (m)")+
  facet_wrap(~season)

dailyAltitudesSumm <- map(dailyAltitudes, ~.x %>%
                            group_by(Nili_id) %>%
                            summarize(mnDailyMnAlt = mean(meanAltitude, na.rm = T)))

## ALL (compile movement metrics) --------------------------------------------
movementBehavior <- map2(areas_list, dailyAltitudesSumm, ~left_join(.x, .y, by = "Nili_id")) %>%
  map2(., roostSwitches, ~left_join(.x, .y, by = "Nili_id")) %>%
  map2(., shannon, ~left_join(.x, .y, by = "Nili_id")) %>%
  map2(., dfdSumm, ~left_join(.x, .y, by = "Nili_id")) %>%
  map2(., mnMvmt, ~left_join(.x, .y, by = "Nili_id")) %>%
  map2(., daysTracked_seasons, ~left_join(.x, .y, by = "Nili_id")) %>%
  map2(., .y = season_names, ~.x %>% mutate(seasonUnique = .y) %>% relocate(seasonUnique, .after = "Nili_id"))

## Add age and sex
ageSex <- map(downsampled_10min_sf, ~.x %>% st_drop_geometry() %>% 
                dplyr::select(Nili_id, birth_year, sex) %>% 
                distinct())

movementBehavior <- map2(movementBehavior, ageSex, ~left_join(.x, .y, by = "Nili_id"))

# Visualize ---------------------------------------------------------------
mbdf <- purrr::list_rbind(movementBehavior)
distviz(mbdf, varname = "coreArea", seasoncol = "seasonUnique")
distviz(mbdf, varname = "homeRange", seasoncol = "seasonUnique")
distviz(mbdf, varname = "coreAreaFidelity", seasoncol = "seasonUnique")
distviz(mbdf, varname = "mnDailyMnAlt", seasoncol = "seasonUnique")
distviz(mbdf, varname = "propSwitch", seasoncol = "seasonUnique")
distviz(mbdf, varname = "shannon", seasoncol = "seasonUnique")
distviz(mbdf, varname = "uniqueRoosts", seasoncol = "seasonUnique")
distviz(mbdf, varname = "meanDFD", seasoncol = "seasonUnique")
distviz(mbdf, varname = "meanDMD", seasoncol = "seasonUnique")
distviz(mbdf, varname = "meanDDT", seasoncol = "seasonUnique")
distviz(mbdf, varname = "mnTort", seasoncol = "seasonUnique")
# I think these all look reasonable

## Save raw movement data ------------------------------------------------------
save(movementBehavior, file = "data/calcMovement/movementBehavior.Rda")

# tidy up
rm(ageSex)
rm(areas)
rm(cc)
rm(coreAreas_indivs)
rm(dailyAltitudes)
rm(dailyAltitudesSumm)
rm(dailyMovementList_seasons)
rm(datasetAssignments)
rm(daysTracked_seasons)
rm(dfd)
rm(dfddf)
rm(dfdSumm)
rm(downsampled_10min)
rm(downsampled_10min_sf)
rm(dts)
rm(homeRanges_indivs)
rm(hrList_indivs)
rm(hrList_indivs_SP)
rm(ikuda_df)
rm(indivs)
rm(indsKUDAreas)
rm(kuds_indivs)
rm(mbdf)
rm(mm)
rm(mnMvmt)
rm(movementBehavior)
rm(roostIDs)
rm(roostPolygons)
rm(roosts)
rm(roostSequences)
rm(roostSwitches)
rm(shannon)
