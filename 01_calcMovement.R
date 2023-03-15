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

## Load data ---------------------------------------------------------------
base::load("data/seasons.Rda")
base::load("data/roosts_seasons.Rda")
seasonNames <- map_chr(seasons, ~.x$seasonUnique[1])
base::load("data/daysTracked_seasons.Rda")
base::load("data/elevs_z09_seasons.Rda")
roostPolygons <- sf::st_read("data/roosts25_cutOffRegion.kml")

# Movement Behavior --------------------------------------------------------
## HOME RANGE ---------------------------------------------------------------
## Individual home ranges
hrList_indivs <- map(seasons, ~.x %>%
                       dplyr::select(trackId) %>%
                       st_transform(32636) %>%
                       mutate(x = st_coordinates(.)[,1], 
                              y = st_coordinates(.)[,2]) %>%
                       st_drop_geometry() %>%
                       group_by(trackId) %>%
                       group_split(.keep = T))
indivs <- map(hrList_indivs, ~map_chr(.x, ~.x$trackId[1]))
hrList_indivs_SP <- map(hrList_indivs, ~map(.x, ~SpatialPoints(.x[,c("x", "y")]))) # returns a list of lists, where each element is a spatial points data frame for one individual over the course of the whole season.

kuds_indivs <- map(hrList_indivs_SP, ~map(.x, ~{
  if(nrow(.x@coords) >= 5){k <- kernelUD(.x, h = "href", grid = 100, extent = 1)}
  else{k <- NULL}
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
indsKUDAreas <- map2(indivs, areas, ~bind_cols("trackId" = .x, .y))
indsKUDAreas <- map2(indsKUDAreas, daysTracked_seasons, ~left_join(.x, .y, by = "trackId"))

### Core Area Fidelity (50%/95%) --------------------------------------------
indsKUDAreas <- map(indsKUDAreas, ~.x %>% 
                      mutate(coreAreaFidelity = coreArea/homeRange) %>%
                      mutate(coreArea_corrected = coreArea/propDaysTracked,
                             homeRange_corrected = homeRange/propDaysTracked))

### 3D Home Range: Altitude -------------------------------------------------
# daily altitude stats
dailyAltitudes <- map(seasons, ~.x %>%
                        st_drop_geometry() %>%
                        group_by(trackId, dateOnly) %>%
                        summarize(meanAltitude = mean(height_above_ground, na.rm  =T),
                                  maxAltitude = max(height_above_ground, na.rm = T),
                                  medAltitude = median(height_above_ground, na.rm = T),
                                  propOver1km = sum(height_above_ground > 1000)/n()))

dailyAltitudesSumm <- map(dailyAltitudes, ~.x %>%
                            group_by(trackId) %>%
                            summarize(mnDailyMaxAlt = mean(maxAltitude, na.rm = T),
                                      mnDailyMnAlt = mean(meanAltitude, na.rm = T),
                                      mnDailyPropOver1km = mean(propOver1km, na.rm = T),
                                      mnDailyMedAlt = mean(medAltitude, na.rm = T)))

## ROOST SITE USE ----------------------------------------------------------
## Make roost locations fall into polygons
roostIDs <- map(roosts_seasons, ~as.numeric(st_intersects(x = .x, y = roostPolygons)))
roosts_seasons <- map2(roosts_seasons, roostIDs, ~.x %>% 
                         mutate(roostID = as.character(.y)) %>%
                         mutate(roostID = case_when(is.na(roostID) ~ paste(trackId, dateOnly, sep = "_"), 
                                                    TRUE ~ roostID)))

### Roost Switches ----------------------------------------------------------
## Calculate sequences of roost locations
roostSequences <- map(roosts_seasons, ~.x %>%
                        st_drop_geometry() %>%
                        dplyr::select(trackId, dateOnly, roostID) %>%
                        arrange(trackId, dateOnly) %>%
                        group_by(trackId) %>%
                        mutate(switch = case_when(roostID == lag(roostID) ~ F,
                                                  TRUE ~ T)))

## Calculate whether, on each night, they switch or stay at the same roost
roostSwitches <- map(roostSequences, ~.x %>%
                       group_by(trackId) %>%
                       summarize(nSwitch = sum(switch),
                                 nStay = sum(!switch),
                                 nights = n(),
                                 propSwitch = nSwitch/nights) %>%
                       dplyr::select(trackId, propSwitch))

### Roost Diversity -------------------------------------------------------
## (Shannon index)
shannon <- map(roosts_seasons, ~.x %>%
                 st_drop_geometry() %>%
                 mutate(roostID = as.character(roostID),
                        roostID = case_when(is.na(roostID) ~ paste(trackId, dateOnly, sep = "_"),
                                            TRUE ~ roostID)) %>%
                 group_by(trackId, roostID) %>%
                 summarize(n = n()) %>%
                 ungroup() %>%
                 group_by(trackId) %>%
                 dplyr::select(trackId, roostID, n) %>%
                 mutate(nNights = sum(n),
                        uniqueRoosts = length(unique(roostID))) %>%
                 mutate(prop = n/nNights,
                        lnProp = log(prop),
                        item = prop*lnProp) %>%
                 group_by(trackId) %>%
                 summarize(shannon = -sum(item)))

## Roost ranking
ranking <- map(roosts_seasons, ~.x %>%
                 st_drop_geometry() %>%
                 mutate(roostID = as.character(roostID),
                        roostID = case_when(is.na(roostID) ~ paste(trackId, dateOnly, sep = "_"),
                                            TRUE ~ roostID)) %>%
                 dplyr::select(trackId, roostID, dateOnly) %>%
                 distinct() %>%
                 group_by(trackId) %>%
                 mutate(nNights = n()) %>%
                 ungroup() %>%
                 dplyr::select(-dateOnly) %>%
                 group_by(trackId, roostID, nNights) %>%
                 summarize(nightsUsed = n()) %>%
                 mutate(propUsed = nightsUsed/nNights) %>%
                 ungroup() %>%
                 group_by(trackId) %>%
                 arrange(desc(nightsUsed), .by_group = T) %>%
                 mutate(rank = 1:n()))

mostUsedRoostProp <- map(ranking, ~.x %>%
                           group_by(trackId) %>%
                           filter(rank == 1) %>%
                           dplyr::select(trackId, "mostUsedRoostProp" = propUsed))

## MOVEMENT ----------------------------------------------------------------
### Daily Flight Duration ---------------------------------------------------
dfd <- map(seasons, ~.x %>% 
             st_drop_geometry() %>%
             group_by(trackId) %>%
             mutate(flight = ifelse(ground_speed > 5, T, F),
                    lagTimestamp = lag(timestamp),
                    lagFlight = lag(flight)) %>%
             mutate(dur = case_when(flight & lagFlight ~ difftime(timestamp, lagTimestamp, units = "secs"),
                                    flight & !lagFlight ~ 0,
                                    !flight & lagFlight ~ difftime(timestamp, lagTimestamp, units = "secs"),
                                    is.na(flight)|is.na(lagFlight) ~ 0)) %>%
             group_by(trackId, dateOnly) %>%
             summarize(totalFlightDuration = as.numeric(sum(dur, na.rm = T))))

dfdSumm <- map(dfd, ~.x %>%
                 group_by(trackId) %>%
                 summarize(meanDFD = mean(totalFlightDuration, na.rm = T),
                           minDFD = min(totalFlightDuration, na.rm = T),
                           maxDFD = max(totalFlightDuration, na.rm = T),
                           medDFD = median(totalFlightDuration, na.rm = T)))

### Daily Distance Traveled/Mean Displacement -------------------------------

# numCores <- parallel::detectCores() - 5
# cl <- parallel::makeCluster(numCores)
# 
# dailyMovementList_seasons <- vector(mode = "list", length = length(seasons))
# for(i in 1:length(dailyMovementList_seasons)){
#   dailyMovementList <- seasons[[i]] %>%
#     dplyr::group_by(trackId, dateOnly) %>%
#     dplyr::group_split(.keep = T)
#   
#   start.mc <- Sys.time()
#   mvmtList <- parallel::parLapply(dailyMovementList, function(x){
#     out <- dplyr::mutate(x,
#                          displacement = as.vector(sf::st_distance(geometry, geometry[1])),
#                          dist_m = sf::st_distance(geometry, dplyr::lag(geometry), by_element = T),
#                          difftime_s = difftime(timestamp, dplyr::lag(timestamp), units = "secs"),
#                          speed_ms = as.numeric(dist_m)/as.numeric(difftime_s),
#                          dist_10minEquiv = speed_ms*600,
#                          endpointDist = sf::st_distance(geometry[1], geometry[dplyr::n()]))
#     return(out)
#   }, 
#   cl = cl)
#   dailyMovement <- data.table::rbindlist(mvmtList)
#   end.mc <- Sys.time()
#   dur <- end.mc - start.mc
#   dailyMovementList_seasons[[i]] <- dailyMovement
#   cat(paste("Finished season", i, "in", dur, "seconds", "\n"))
# }
# 
# save(dailyMovementList_seasons, file = "data/dailyMovementList_seasons.Rda")
load("data/dailyMovementList_seasons.Rda") # we should only get one NA value per individual per day, which means that we should have as many individual*days as there are NA values. The only other way we could have an NA value here is if one of the geometries if NA, and I don't think there are any NA geometries. Also, the same rows should be NA for dist_m and difftime_s.

dailyMovementSumm <- map(dailyMovementList_seasons, ~.x %>%
                           sf::st_drop_geometry() %>%
                           group_by(trackId, dateOnly) %>%
                           summarize(dmd = max(as.numeric(displacement), na.rm = T),
                                     ddt = sum(as.numeric(dist_m), na.rm = T),
                                     endpointDist = as.numeric(endpointDist)[1],
                                     tort = as.numeric(ddt)/as.numeric(endpointDist), .groups = "drop"))

mnMvmt <- map(dailyMovementSumm, ~.x %>%
                group_by(trackId) %>%
                summarize(meanDMD = mean(dmd, na.rm = T),
                          meanDDT = mean(ddt, na.rm = T),
                          sdDMD = sd(dmd, na.rm =T),
                          sdDDT = sd(ddt, na.rm = T),
                          mnTort = mean(tort, na.rm = T),
                          sdTort = sd(tort, na.rm = T)))

mnMvmt[[1]] %>% is.na() %>% colSums() # yessssssssssssssss
dailyMovementSumm[[1]] %>% is.na() %>% colSums() # also yessssss

## Compile all movement metrics --------------------------------------------
movementBehavior <- map2(indsKUDAreas, dailyAltitudesSumm, ~left_join(.x, .y, by = "trackId")) %>%
  map2(., roostSwitches, ~left_join(.x, .y, by = "trackId")) %>%
  map2(., shannon, ~left_join(.x, .y, by = "trackId")) %>%
  map2(., mostUsedRoostProp, ~left_join(.x, .y, by = "trackId")) %>%
  map2(., dfdSumm, ~left_join(.x, .y, by = "trackId")) %>%
  map2(., mnMvmt, ~left_join(.x, .y, by = "trackId")) %>%
  map(., na.omit) %>%
  map2(., .y = seasonNames, ~.x %>% mutate(seasonUnique = .y))

## Scale vars
movementBehaviorScaled <- map(movementBehavior, ~.x %>%
                                mutate(across(-c(trackId, seasonUnique), function(x){
                                  as.numeric(as.vector(scale(x)))
                                })))

## Save raw movement data ------------------------------------------------------
save(movementBehavior, file = "data/movementBehavior.Rda")
save(movementBehaviorScaled, file = "data/movementBehaviorScaled.Rda")
