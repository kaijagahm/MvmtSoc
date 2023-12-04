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
load("data/cc.Rda")

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
base::load("data/seasons_10min.Rda") # data rarefied to 10 minute intervals. Going to use that for everything.
base::load("data/datasetAssignments.Rda")
base::load("data/roosts_seasons.Rda") # don't need a separate roost dataset for the rarefied data, because roosts are only once per day, and it makes sense to use the most detailed dataset possible.
roosts_seasons <- map(roosts_seasons, ~.x %>% group_by(Nili_id) %>% 
                        mutate(daysTracked = length(unique(roost_date))) %>% ungroup())

roosts_seasons <- roosts_seasons[-1] # remove summer 2020
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
names(daysTracked_seasons) <- seasonNames
dts <- purrr::list_rbind(daysTracked_seasons, names_to = "season")

# VVV Visualize days tracked
distviz(dts, "propDaysTracked", "season")

# Movement Behavior --------------------------------------------------------
## HOME RANGE ---------------------------------------------------------------
## Individual home ranges
hrList_indivs <- purrr::map(seasons_10min, ~.x %>%
                       dplyr::select(Nili_id) %>%
                       st_transform(32636) %>%
                       ungroup() %>%
                       mutate(x = sf::st_coordinates(.)[,1],
                              y = sf::st_coordinates(.)[,2]) %>%
                       st_drop_geometry() %>%
                       group_by(Nili_id) %>%
                       group_split(.keep = T))
save(hrList_indivs, file = "data/hrList_indivs.Rda") # for use in 01.5_KDERarefaction.R
load("data/hrList_indivs.Rda")

indivs <- map(hrList_indivs, ~map_chr(.x, ~.x$Nili_id[1]))
hrList_indivs_SP <- map(hrList_indivs, ~map(.x, ~sp::SpatialPoints(.x[,c("x", "y")]))) # returns a list of lists, where each element is a spatial points data frame for one individual over the course of the whole season.
save(hrList_indivs_SP, file = "data/hrList_indivs_SP.Rda")

kuds_indivs <- map(hrList_indivs_SP, ~map(.x, ~{
  if(nrow(.x@coords) >= 5){k <- kernelUD(.x, h = "href", grid = 100, extent = 1)}
  else{k <- NULL} # changed back to the href value here--important to have different h for individuals with different numbers of points, and the fact that we'll take percentages of the resulting errors should mean that it comes out in the wash anyway.
  return(k)}, .progress = T))
save(kuds_indivs, file = "data/kuds_indivs.Rda")
load("data/kuds_indivs.Rda")

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
names(indsKUDAreas) <- seasonNames
ikuda_df <- purrr::list_rbind(indsKUDAreas, names_to = "season")

# VVV Visualize core areas, home ranges, and ratios
distviz(ikuda_df, "homeRange", "season")
distviz(ikuda_df, "coreArea", "season")
distviz(ikuda_df, "coreAreaFidelity", "season")

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
             group_by(Nili_id, dateOnly) %>%
             summarize(totalFlightDuration = as.numeric(sum(dur, na.rm = T))))

dfdSumm <- map(dfd, ~.x %>%
                 group_by(Nili_id) %>%
                 summarize(meanDFD = mean(totalFlightDuration, na.rm = T),
                           minDFD = min(totalFlightDuration, na.rm = T),
                           maxDFD = max(totalFlightDuration, na.rm = T),
                           medDFD = median(totalFlightDuration, na.rm = T)))

names(dfdSumm) <- seasonNames
dfddf <- purrr::list_rbind(dfdSumm, names_to = "season") %>%
  mutate(meanDFD = meanDFD/60/60,
         minDFD = minDFD/60/60,
         maxDFD = maxDFD/60/60,
         medDFD = medDFD/60/60)
distviz(dfddf, "meanDFD", "season")
distviz(dfddf, "minDFD", "season")
distviz(dfddf, "maxDFD", "season")

## Time spent flying in summer/fall/breeding
dfddf <- dfddf %>%
  mutate(szn = as.factor(str_extract(season, "summer|breeding|fall")),
         year = str_extract(season, "[0-9]{4}"))

# Do the seasons differ in mean daily flight duration?
dfddf %>%
  ggplot(aes(x = meanDFD, col = szn, group = factor(season)))+
  geom_density(size = 1.5, alpha = 0.7)+
  theme_classic()+
  scale_color_manual(values = c(cc$breedingColor, cc$fallColor, cc$summerColor))

# what about min?
dfddf %>%
  ggplot(aes(x = minDFD, col = szn, group = factor(season)))+
  geom_density(size = 1.5, alpha = 0.7)+
  theme_classic()+
  scale_color_manual(values = c(cc$breedingColor, cc$fallColor, cc$summerColor)) # fewer zero days in summer! Makes sense.

# what about max?
dfddf %>%
  ggplot(aes(x = maxDFD, col = szn, group = factor(season)))+
  geom_density(size = 1.5, alpha = 0.7)+
  theme_classic()+
  scale_color_manual(values = c(cc$breedingColor, cc$fallColor, cc$summerColor)) #less differentiation, but still the order we expect.

# To see whether this trend is true within individuals, we'd have to do a mixed model. Seems like overkill for this, but it might be a worthwhile question in itself (or maybe someone has done it already?) #XXX add to questions for Marta

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
dailyMovementList_seasons <- purrr::map(seasons_10min, calc_metrics, .progress = T)
save(dailyMovementList_seasons, file = "data/dailyMovementList_seasons.Rda")
load("data/dailyMovementList_seasons.Rda") 

mnMvmt <- map(dailyMovementList_seasons, ~.x %>%
                group_by(Nili_id) %>%
                summarize(meanDMD = mean(dmd, na.rm = T),
                          meanDDT = mean(ddt, na.rm = T),
                          mnTort = mean(tort_dmd, na.rm = T)))

mnMvmt[[1]] %>% is.na() %>% colSums() # yessssssssssssssss

names(mnMvmt) <- seasonNames
mm <- purrr::list_rbind(mnMvmt, names_to = "season")
distviz(mm, "meanDMD", "season")
distviz(mm, "meanDDT", "season") # huh, those distributions do seem to differ more...
distviz(mm, "mnTort", "season")

# Would be good to do this by season same as above

### Altitude -------------------------------------------------
# daily altitude stats
# Note that I've already cleaned outlier altitudes in 00_dataPrep.R. 
# Now just need to restrict to ground_speed > 5m/s
dailyAltitudes <- map(seasons_10min, ~.x %>%
                        st_drop_geometry() %>%
                        filter(ground_speed >= 5) %>% # we only care about the altitude of in-flight points.
                        group_by(Nili_id, dateOnly) %>%
                        summarize(meanAltitude = mean(height_above_ground, na.rm  =T),
                                  maxAltitude = max(height_above_ground, na.rm = T),
                                  medAltitude = median(height_above_ground, na.rm = T)) %>%
                        mutate(meanAltitude = case_when(meanAltitude < 0 ~ NA,
                                                        TRUE ~ meanAltitude),
                               maxAltitude = case_when(maxAltitude < 0 ~ NA, 
                                                       TRUE ~ maxAltitude),
                               medAltitude = case_when(medAltitude < 0 ~ NA,
                                                       TRUE ~ medAltitude)))
names(dailyAltitudes) <- seasonNames

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
                            summarize(mnDailyMaxAlt = mean(maxAltitude, na.rm = T),
                                      mnDailyMnAlt = mean(meanAltitude, na.rm = T),
                                      mnDailyMedAlt = mean(medAltitude, na.rm = T)))

## ALL (compile movement metrics) --------------------------------------------
movementBehavior <- map2(indsKUDAreas, dailyAltitudesSumm, ~left_join(.x, .y, by = "Nili_id")) %>%
  map2(., roostSwitches, ~left_join(.x, .y, by = "Nili_id")) %>%
  map2(., shannon, ~left_join(.x, .y, by = "Nili_id")) %>%
  map2(., dfdSumm, ~left_join(.x, .y, by = "Nili_id")) %>%
  map2(., mnMvmt, ~left_join(.x, .y, by = "Nili_id")) %>%
  map2(., daysTracked_seasons, ~left_join(.x, .y, by = "Nili_id")) %>%
  map2(., .y = seasonNames, ~.x %>% mutate(seasonUnique = .y) %>% relocate(seasonUnique, .after = "Nili_id")) %>%
  map2(., datasetAssignments, ~.x %>% left_join(.y))

## Add age and sex
ageSex <- map(seasons_10min, ~.x %>% st_drop_geometry() %>% 
                dplyr::select(Nili_id, birth_year, sex) %>% 
                distinct())

movementBehavior <- map2(movementBehavior, ageSex, ~left_join(.x, .y, by = "Nili_id"))


# Visualize ---------------------------------------------------------------
mbdf <- purrr::list_rbind(movementBehavior)
distviz(mbdf, varname = "coreArea", seasoncol = "seasonUnique")
distviz(mbdf, varname = "homeRange", seasoncol = "seasonUnique")
distviz(mbdf, varname = "coreAreaFidelity", seasoncol = "seasonUnique")
distviz(mbdf, varname = "mnDailyMaxAlt", seasoncol = "seasonUnique")
distviz(mbdf, varname = "mnDailyMnAlt", seasoncol = "seasonUnique")
distviz(mbdf, varname = "mnDailyMedAlt", seasoncol = "seasonUnique")
distviz(mbdf, varname = "propSwitch", seasoncol = "seasonUnique")
distviz(mbdf, varname = "shannon", seasoncol = "seasonUnique")
distviz(mbdf, varname = "uniqueRoosts", seasoncol = "seasonUnique")
distviz(mbdf, varname = "meanDFD", seasoncol = "seasonUnique")
distviz(mbdf, varname = "minDFD", seasoncol = "seasonUnique")
distviz(mbdf, varname = "maxDFD", seasoncol = "seasonUnique")
distviz(mbdf, varname = "medDFD", seasoncol = "seasonUnique")
distviz(mbdf, varname = "meanDMD", seasoncol = "seasonUnique")
distviz(mbdf, varname = "meanDDT", seasoncol = "seasonUnique")
distviz(mbdf, varname = "mnTort", seasoncol = "seasonUnique")
# I think these all look reasonable

# Remove individuals not included for mode10 ------------------------------
load("data/toKeep_fixrate.Rda")
toKeep_fixrate <- toKeep_fixrate[-1]
movementBehavior_mode10 <- map2(movementBehavior, toKeep_fixrate, ~.x %>%
                                  filter(Nili_id %in% .y))

## Scale vars
movementBehaviorScaled <- map(movementBehavior, ~.x %>%
                                mutate(across(-c(Nili_id, seasonUnique, birth_year, sex, dataset), function(x){
                                  as.numeric(as.vector(scale(x)))
                                })))

movementBehaviorScaled_mode10 <- map(movementBehavior_mode10, ~.x %>%
                                mutate(across(-c(Nili_id, seasonUnique, birth_year, sex, dataset), function(x){
                                  as.numeric(as.vector(scale(x)))
                                })))

## Save raw movement data ------------------------------------------------------
save(movementBehavior, file = "data/movementBehavior.Rda")
save(movementBehaviorScaled, file = "data/movementBehaviorScaled.Rda")

save(movementBehavior_mode10, file = "data/movementBehavior_mode10.Rda")
save(movementBehaviorScaled_mode10, file = "data/movementBehaviorScaled_mode10.Rda") # not going to use this mode10 data--Noa and I have agreed that the PCA should be run on the main movementBehaviorScaled data, because we need to include all of the individuals that will eventually make it into *any* regression (e.g. roost situation). But I'm leaving this here just in case.

# Investigate the movement variables across datasets ----------------------
load("data/toKeep_fixrate.Rda")
toKeep_fixrate <- toKeep_fixrate[-1]

mb <- map2(movementBehavior, toKeep_fixrate, ~.x %>%
             mutate(mode10 = case_when(Nili_id %in% .y ~ T,
                                       TRUE ~ F))) %>%
  purrr::list_rbind()

# variables we think could be related: ddt, tort, dfd
mb %>%
  ggplot(aes(x = mode10, y = meanDDT))+
  geom_boxplot()+
  facet_wrap(~seasonUnique) # some small differences but not much

mb %>%
  ggplot(aes(x = mode10, y = mnTort))+
  geom_boxplot()+
  facet_wrap(~seasonUnique) # pretty much no diff, or at least no consistent diff

mb %>%
  ggplot(aes(x = mode10, y = meanDFD))+
  geom_boxplot()+
  facet_wrap(~seasonUnique) # no difference here either

# No evident/consistent differences in these measures between mode10 individuals and other individuals
