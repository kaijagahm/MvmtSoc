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
library(elevatr) # for getting elevation information
library(raster) # for getting elevation information
library(viridis)
library(naniar)
library(parallel) # for running long distance calculations in parallel
library(corrplot)
library(future) # for parallel computing

## Load data ---------------------------------------------------------------
base::load("data/seasons_10min.Rda") # data rarefied to 10 minute intervals. Going to use that for everything.
base::load("data/datasetAssignments.Rda")
# add number of points per day
seasons_10min <- map(seasons_10min, ~.x %>% group_by(Nili_id, dateOnly, dataset) %>% mutate(ppd = length(unique(timestamp))) %>% ungroup() %>% group_by(Nili_id) %>% mutate(daysTracked = length(unique(dateOnly))) %>% ungroup())

mnPPD <- seasons_10min %>%
  purrr::list_rbind() %>%
  group_by(seasonUnique, Nili_id, dateOnly, dataset) %>%
  summarize(ppd = ppd[1]) %>%
  group_by(seasonUnique, Nili_id, dataset) %>%
  summarize(mnPPD = mean(ppd))
save(mnPPD, file = "data/mnPPD.Rda")

ppd <- map(seasons_10min, ~.x %>% # points per day
             sf::st_drop_geometry() %>%
             dplyr::select(Nili_id, dataset, dateOnly, ppd) %>%
             distinct())

base::load("data/roosts_seasons.Rda") # don't need a separate roost dataset for the rarefied data, because roosts are only once per day, and it makes sense to use the most detailed dataset possible.
roosts_seasons <- map(roosts_seasons, ~.x %>% group_by(Nili_id) %>% mutate(daysTracked = length(unique(roost_date))) %>% ungroup())
roosts_seasons <- roosts_seasons[-1] # remove summer 2020
base::load("data/datasetAssignments.Rda")
datasetAssignments <- datasetAssignments[-1] # remove summer 2020
test <- map2(roosts_seasons, datasetAssignments, ~left_join(.x, .y, by = "Nili_id"))
seasonNames <- map_chr(seasons_10min, ~as.character(.x$seasonUnique[1]))
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
load("data/hrList_indivs.Rda")

indivs <- map(hrList_indivs, ~map_chr(.x, ~.x$Nili_id[1]))
hrList_indivs_SP <- map(hrList_indivs, ~map(.x, ~sp::SpatialPoints(.x[,c("x", "y")]))) # returns a list of lists, where each element is a spatial points data frame for one individual over the course of the whole season.

kuds_indivs <- map(hrList_indivs_SP, ~map(.x, ~{
  if(nrow(.x@coords) >= 5){k <- kernelUD(.x, h = "href", grid = 100, extent = 1)}
  else{k <- NULL} # changed back to the href value here--important to have different h for individuals with different numbers of points, and the fact that we'll take percentages of the resulting errors should mean that it comes out in the wash anyway.
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
table(unlist(map(roosts_seasons, ~as.numeric(lengths(st_intersects(.x, roostPolygons)))))) # should be only 0 or 1. Good!

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

# How do these relate to numbers of days tracked?

## MOVEMENT ----------------------------------------------------------------
### Daily Flight Duration ---------------------------------------------------
dfd <- map(seasons_10min, ~.x %>%  # using the 10-minute rarefied data, for consistency. Shouldn't really affect the estimates much.
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
             group_by(Nili_id, dateOnly, ppd) %>%
             summarize(totalFlightDuration = as.numeric(sum(dur, na.rm = T))))

# Sanity check for dfd: should always be <86400 and should never be negative
purrr::list_rbind(dfd) %>%
  ggplot(aes(x = totalFlightDuration))+
  geom_histogram() # okay, that looks gorgeous. Just to make sure, let's look at a summary
purrr::list_rbind(dfd) %>%
  pull(totalFlightDuration) %>%
  summary() # yay! no negative values, and nothing over 50000 (or very rarely.) Median is 13208, which is around 3.66 hours of flight per day. 40000, which is in the tail, is 11 hours of flight per day. That doesn't make sense. Even 30000 is 8.3 hours of flight per day, which still seems really high but isn't utterly out of the question biologically... or maybe it is? I wonder if I need to look into doing something about this. 
# I assume this just happens when you have one point far away from the others that's a flight point, and the previous one was also a flight point, so all of that time gets counted as flight. This may just need to count as error; I don't think there's something *fundamentally* wrong in how I'm doing the calculation.

purrr::list_rbind(dfd) %>%
  filter(totalFlightDuration < 0)

## Does daily flight duration depend on the number of points?
walk(dfd, ~{
  p <- .x %>% 
    ggplot(aes(x = ppd, y = totalFlightDuration))+
    geom_point()+
    geom_smooth(method = "lm")
  print(p)}) # maybe a relationship, but need to deal with weird outlier values first.

# Why do we still have individuals with so many points per day when it was supposed to be rarefied?? Go back to data prep and check on this. For now, let's look at a graph with those ones removed:
walk(dfd, ~{
  p <- .x %>% 
    filter(ppd < 75) %>%
    ggplot(aes(x = ppd, y = totalFlightDuration))+
    geom_point()+
    geom_smooth(method = "lm")
  print(p)})
# ok, we do have a pretty strong relationship here between number of points per day and daily flight duration. But I don't think that will be fixed by removing the individuals under 30. 

walk(dfd, ~{
  p <- .x %>% 
    filter(ppd < 75, ppd > 30) %>%
    ggplot(aes(x = ppd, y = totalFlightDuration))+
    geom_point()+
    geom_smooth(method = "lm")
  print(p)})
# yeah, confirmed, this doesn't do much to change the relationship. May just need to correct DFD by points per day before taking the mean. XXX

dfdSumm <- map(dfd, ~.x %>%
                 group_by(Nili_id) %>%
                 summarize(meanDFD = mean(totalFlightDuration, na.rm = T),
                           minDFD = min(totalFlightDuration, na.rm = T),
                           maxDFD = max(totalFlightDuration, na.rm = T),
                           medDFD = median(totalFlightDuration, na.rm = T)))

### Daily Distance Traveled/Mean Displacement -------------------------------

# set up parallel backend with 4 sessions (change this to match your machine)
future::plan(future::multicore, workers = 4)

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
save(dailyMovementList_seasons, file = "data/dailyMovementList_seasons.Rda")
load("data/dailyMovementList_seasons.Rda") 
dailyMovementList_seasons <- map2(dailyMovementList_seasons, ppd, ~.x %>%
                                    left_join(.y)) # join points per day (ppd)

# Investigate relationship between movement metrics and ppd
names(dailyMovementList_seasons) <- seasonNames
purrr::list_rbind(dailyMovementList_seasons, names_to = "seasonUnique") %>%
  pivot_longer(cols = c("dmd", "ddt", "tort_dmd"), names_to = "metric", values_to = "value") %>%
  ggplot(aes(x = ppd, y = value, col = seasonUnique))+
  geom_point(size = 0.2)+
  facet_wrap(~metric, scales = "free")+
  geom_smooth(method = "lm")+
  theme_classic() # ok, once again, the movement metrics are strongly correlated with the number of points, but I don't think removing the ppd under 30 will really change that.

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
                                  propOver1km = sum(height_above_ground > 1000)/n())) %>%
  map2(., ppd, ~.x %>% left_join(.y))
names(dailyAltitudes) <- seasonNames
dailyAltitudes %>% purrr::list_rbind(., names_to = "seasonUnique") %>%
  ggplot(aes(x = ppd, y = maxAltitude, col = seasonUnique))+
  geom_smooth(method = "lm")+
  geom_point(size = 0.2)+
  theme_classic()

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
  map2(., .y = seasonNames, ~.x %>% mutate(seasonUnique = .y) %>% relocate(seasonUnique, .after = "Nili_id")) %>%
  map2(., datasetAssignments, ~.x %>% left_join(.y))

## Add age and sex
ageSex <- map(seasons_10min, ~.x %>% st_drop_geometry() %>% 
                dplyr::select(Nili_id, birth_year, sex) %>% 
                distinct())

movementBehavior <- map2(movementBehavior, ageSex, ~left_join(.x, .y, by = "Nili_id"))


## Scale vars
movementBehaviorScaled <- map(movementBehavior, ~.x %>%
                                mutate(across(-c(Nili_id, seasonUnique, birth_year, sex, dataset), function(x){
                                  as.numeric(as.vector(scale(x)))
                                })))

## Save raw movement data ------------------------------------------------------
save(movementBehavior, file = "data/movementBehavior.Rda")
save(movementBehaviorScaled, file = "data/movementBehaviorScaled.Rda")
