# Script for calculating movement metrics

# Setup -------------------------------------------------------------------
## Load packages -----------------------------------------------------------
library(vultureUtils)
library(sf)
library(tidyverse)
library(igraph)
library(mapview) # for quick maps
library(MASS) # for KDE
library(adehabitatHR)
library(sp)
library(ggfortify)
library(factoextra)
library(elevatr) # for getting elevation information
library(raster) # for getting elevation information
library(viridis)
library(naniar)
library(parallel) # for running long distance calculations in parallel
library(corrplot)
library(future) # for parallel computing

## Load data ---------------------------------------------------------------
base::load("data/seasons_10min.Rda") # data rarefied to 10 minute intervals. Going to use that for everything.
base::load("data/roosts_seasons.Rda")
seasonNames <- map_chr(seasons_10min, ~.x$seasonUnique[1])
roostPolygons <- sf::st_read("data/roosts50_kde95_cutOffRegion.kml")

durs <- map_dbl(seasons_10min, ~{
  dates <- lubridate::ymd(.x$dateOnly)
  dur <- difftime(max(dates), min(dates), units = "days")
})

# Get number of days tracked for each individual --------------------------
daysTracked_seasons <- map2(seasons_10min, durs, ~.x %>%
                              st_drop_geometry() %>%
                              group_by(Nili_id) %>%
                              summarize(daysTracked = length(unique(dateOnly)),
                                        propDaysTracked = daysTracked/.y))

# Movement Behavior --------------------------------------------------------
## HOME RANGE ---------------------------------------------------------------
## Individual home ranges
hrList_indivs <- map(seasons_10min, ~.x %>%
                       dplyr::select(Nili_id) %>%
                       st_transform(32636) %>%
                       mutate(x = st_coordinates(.)[,1], 
                              y = st_coordinates(.)[,2]) %>%
                       st_drop_geometry() %>%
                       group_by(Nili_id) %>%
                       group_split(.keep = T))
save(hrList_indivs, file = "data/hrList_indivs.Rda") # for use in 01.5_KDERarefaction.R
indivs <- map(hrList_indivs, ~map_chr(.x, ~.x$Nili_id[1]))
hrList_indivs_SP <- map(hrList_indivs, ~map(.x, ~SpatialPoints(.x[,c("x", "y")]))) # returns a list of lists, where each element is a spatial points data frame for one individual over the course of the whole season.

kuds_indivs <- map(hrList_indivs_SP, ~map(.x, ~{
  if(nrow(.x@coords) >= 5){k <- kernelUD(.x, h = "href", grid = 100, extent = 1)}
  else{k <- NULL} # changed back to the href value here because Noa thinks that's better, but will need to discuss with Noa and Marta.
  return(k)}, .progress = T))

### Core Area (50%) ---------------------------------------------------------
coreAreas_indivs <- map(kuds_indivs, ~map_dbl(.x, ~{
  if(!is.null(.x)){
    area <- kernel.area(.x, percent = 50)}
  else{area <- NA}
  return(area)
}))

### Home Range (95%) ---------------------------------------------------------
homeRanges_indivs <- map(kuds_indivs, ~map_dbl(.x, ~{
  if(!is.null(.x)){
    area <- kernel.area(.x, percent = 95)}
  else{area <- NA}
  return(area)
}))

### Assemble them
areas <- map2(coreAreas_indivs, homeRanges_indivs, 
              ~bind_cols("coreArea" = .x, "homeRange" = .y))
indsKUDAreas <- map2(indivs, areas, ~bind_cols("Nili_id" = .x, .y))

### Core Area Fidelity (50%/95%) --------------------------------------------
indsKUDAreas <- map(indsKUDAreas, ~.x %>% 
                      mutate(coreAreaFidelity = coreArea/homeRange))

## ROOST SITE USE ----------------------------------------------------------
# Make sure that each roost point intersects only one (or 0) roost (multi)polygon(s)
table(unlist(map(roosts_seasons, ~as.numeric(lengths(st_intersects(.x, roostPolygons)))))) 

# Get ID's of which polygon each roost location falls into
roostIDs <- map(roosts_seasons, ~as.numeric(st_intersects(x = .x, y = roostPolygons)))

roosts_seasons <- map2(roosts_seasons, roostIDs, ~.x %>% 
                         mutate(roostID = as.character(.y)) %>%
                         mutate(roostID = case_when(is.na(roostID) ~ paste(Nili_id, roost_date, sep = "_"), 
                                                    TRUE ~ roostID)))

### Roost Switches ----------------------------------------------------------
## Calculate sequences of roost locations
roostSequences <- map(roosts_seasons, ~.x %>%
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
shannon <- map(roosts_seasons, ~.x %>%
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

## Roost ranking
ranking <- map(roosts_seasons, ~.x %>%
                 st_drop_geometry() %>%
                 mutate(roostID = as.character(roostID),
                        roostID = case_when(is.na(roostID) ~ 
                                              paste(Nili_id, roost_date, sep = "_"),
                                            TRUE ~ roostID)) %>%
                 dplyr::select(Nili_id, roostID, roost_date) %>%
                 distinct() %>%
                 group_by(Nili_id) %>%
                 mutate(nNights = n()) %>%
                 ungroup() %>%
                 dplyr::select(-roost_date) %>%
                 group_by(Nili_id, roostID, nNights) %>%
                 summarize(nightsUsed = n()) %>%
                 mutate(propUsed = nightsUsed/nNights) %>%
                 ungroup() %>%
                 group_by(Nili_id) %>%
                 arrange(desc(nightsUsed), .by_group = T) %>%
                 mutate(rank = 1:n()))

mostUsedRoostProp <- map(ranking, ~.x %>%
                           group_by(Nili_id) %>%
                           filter(rank == 1) %>%
                           dplyr::select(Nili_id, "mostUsedRoostProp" = propUsed))

## MOVEMENT ----------------------------------------------------------------
### Daily Flight Duration ---------------------------------------------------
dfd <- map(seasons_10min, ~.x %>%  # using the rarefied data, for consistency. Shouldn't really affect the estimates much.
             st_drop_geometry() %>%
             group_by(Nili_id) %>%
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
                 summarize(meanDFD = mean(totalFlightDuration, na.rm = T),
                           minDFD = min(totalFlightDuration, na.rm = T),
                           maxDFD = max(totalFlightDuration, na.rm = T),
                           medDFD = median(totalFlightDuration, na.rm = T)))

### Daily Distance Traveled/Mean Displacement -------------------------------

# set up parallel backend with 4 sessions (change this to match your machine)
future::plan(multicore, workers = 4)

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
    dplyr::group_split(Nili_id, dateOnly)

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
dailyMovementList_seasons <- purrr::map(seasons_10min, calc_metrics, .progress = T)
# XXX START HERE
save(dailyMovementList_seasons, file = "data/dailyMovementList_seasons.Rda")
load("data/dailyMovementList_seasons.Rda") 

mnMvmt <- map(dailyMovementList_seasons, ~.x %>%
                group_by(Nili_id) %>%
                summarize(meanDMD = mean(dmd, na.rm = T),
                          meanDDT = mean(ddt, na.rm = T),
                          mnTort = mean(tort_dmd, na.rm = T)))

mnMvmt[[1]] %>% is.na() %>% colSums() # yessssssssssssssss

### Altitude -------------------------------------------------
# daily altitude stats
# Note that I've already cleaned outlier altitudes in 00_dataPrep.R. 
# Now just need to restrict to ground_speed > 5m/s
dailyAltitudes <- map(seasons_10min, ~.x %>%
                        st_drop_geometry() %>%
                        filter(ground_speed >= 5) %>%
                        group_by(Nili_id, dateOnly) %>%
                        summarize(meanAltitude = mean(height_above_ground, na.rm  =T),
                                  maxAltitude = max(height_above_ground, na.rm = T),
                                  medAltitude = median(height_above_ground, na.rm = T),
                                  propOver1km = sum(height_above_ground > 1000)/n()))

dailyAltitudesSumm <- map(dailyAltitudes, ~.x %>%
                            group_by(Nili_id) %>%
                            summarize(mnDailyMaxAlt = mean(maxAltitude, na.rm = T),
                                      mnDailyMnAlt = mean(meanAltitude, na.rm = T),
                                      mnDailyPropOver1km = mean(propOver1km, na.rm = T),
                                      mnDailyMedAlt = mean(medAltitude, na.rm = T)))

## ALL (compile movement metrics) --------------------------------------------
movementBehavior <- map2(indsKUDAreas, dailyAltitudesSumm, ~left_join(.x, .y, by = "Nili_id")) %>%
  map2(., roostSwitches, ~left_join(.x, .y, by = "Nili_id")) %>%
  map2(., shannon, ~left_join(.x, .y, by = "Nili_id")) %>%
  map2(., mostUsedRoostProp, ~left_join(.x, .y, by = "Nili_id")) %>%
  map2(., dfdSumm, ~left_join(.x, .y, by = "Nili_id")) %>%
  map2(., mnMvmt, ~left_join(.x, .y, by = "Nili_id")) %>%
  map2(., daysTracked_seasons, ~left_join(.x, .y, by = "Nili_id")) %>%
  #map(., na.omit) %>%
  map2(., .y = seasonNames, ~.x %>% mutate(seasonUnique = .y) %>% relocate(seasonUnique, .after = "Nili_id"))

## Add age and sex
ageSex <- map(seasons_10min, ~.x %>% st_drop_geometry() %>% 
                dplyr::select(Nili_id, birth_year, sex) %>% 
                distinct())


movementBehavior <- map2(movementBehavior, ageSex, ~left_join(.x, .y, by = "Nili_id"))


## Scale vars
movementBehaviorScaled <- map(movementBehavior, ~.x %>%
                                mutate(across(-c(Nili_id, seasonUnique, birth_year, sex), function(x){
                                  as.numeric(as.vector(scale(x)))
                                })))

## Save raw movement data ------------------------------------------------------
save(movementBehavior, file = "data/movementBehavior.Rda")
save(movementBehaviorScaled, file = "data/movementBehaviorScaled.Rda")
