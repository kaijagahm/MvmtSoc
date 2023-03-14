# Script for a preliminary investigation of movement metrics. So far, this analysis is only the 

# Setup -------------------------------------------------------------------
## Load packages -----------------------------------------------------------
library(vultureUtils)
library(sf)
library(tidyverse)
library(igraph)
library(mapview) # for quick maps
library(MASS) # for KDE
library(vegan) # for Shannon diversity
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
load("data/datAnnotCleaned.Rda")
roostPolygons <- sf::st_read("data/roosts25_cutOffRegion.kml") %>%
  sf::st_set_crs("WGS84") %>%
  sf::st_transform(32636)

# Fix time zone so dates make sense ---------------------------------------
## Overwrite the dateOnly column from the new times
datAnnotCleaned <- datAnnotCleaned %>%
  mutate(timestampIsrael = lubridate::with_tz(timestamp, tzone = "Israel"),
         dateOnly = lubridate::date(timestampIsrael))

# Time: restrict to 2022 breeding season -----------------------------
bs2022 <- datAnnotCleaned %>%
  filter(timestampIsrael > lubridate::ymd("2021-12-01"), 
         timestampIsrael < lubridate::ymd("2022-07-01"))

# Space: restrict to southern indivs --------------------------------------
## Make it an sf object
bs2022 <- bs2022 %>%
  sf::st_as_sf(coords = c("location_long", "location_lat"), remove = F) %>%
  sf::st_set_crs("WGS84") %>%
  sf::st_transform(32636)

centroids_bs2022 <- bs2022%>%
  group_by(trackId) %>%
  summarize(geometry = st_union(geometry)) %>%
  st_centroid()
#mapview(centroids_bs2022, zcol = "trackId")

hist(st_coordinates(centroids_bs2022)[,2]) # plot y coords to find a good cutoff point

## filter to only southern indivs
southernIndivs <- centroids_bs2022 %>%
  filter(st_coordinates(.)[,2] < 3550000) %>%
  pull(trackId)

bs2022 <- bs2022 %>%
  filter(trackId %in% southernIndivs)

# How many points per day per individual?
bs2022 %>%
  st_drop_geometry() %>%
  group_by(trackId, dateOnly) %>%
  summarize(nPoints = n()) %>%
  ggplot(aes(x = nPoints))+
  geom_histogram()

# theoretically, should only have 72 points per day--12 hours, 6 points per hour. That should vary a bit. I don't really understand why so many individuals/days have more than 100 points per day, but let's proceed for now.
# 72/3 = 24. For simplicity, let's restrict to individual/days with at least 25 points per day.
bs2022 <- bs2022 %>%
  group_by(trackId, dateOnly) %>%
  filter(n() >= 25)

## For how many days is each individual tracked (after filtering for points per day)?
daysTracked_bs2022 <- bs2022 %>%
  st_drop_geometry() %>%
  group_by(trackId) %>%
  summarize(nDaysTracked = length(unique(dateOnly)))

totalTimePeriod <- as.numeric(max(bs2022$dateOnly) - min(bs2022$dateOnly))

bs2022 <- bs2022 %>%
  left_join(daysTracked_bs2022, by = "trackId") %>%
  mutate(propDaysTracked = nDaysTracked/totalTimePeriod)

# Exclude individuals tracked for less than 1/3 of the time
bs2022 <- bs2022 %>%
  filter(propDaysTracked >= 0.33)

# Movement Behavior --------------------------------------------------------

## HOME RANGE ---------------------------------------------------------------
hrList <- bs2022 %>%
  ungroup() %>%
  dplyr::select(trackId) %>%
  st_transform(32636) %>%
  mutate(x = st_coordinates(.)[,1], y = st_coordinates(.)[,2]) %>%
  st_drop_geometry() %>%
  group_by(trackId) %>%
  group_split(.keep = T)

testHR <- hrList[[3]]
testHRSP <- SpatialPoints(testHR[,c("x", "y")])
k <- kernelUD(testHRSP, h = "href", grid = 100, extent = 0.1)
image(k)

hrListSP <- map(hrList, ~SpatialPoints(.x[,c("x", "y")]))

kuds <- map(hrListSP, ~{
  if(nrow(.x@coords)>= 5){
    k <- kernelUD(.x, h = "href", grid = 100, extent = 1)}
  else{
    k <- NULL
  }
  return(k)}, .progress = T)

inds <- map_dfr(hrList, ~.x[1, "trackId"])

whichNotNull <- which(map_lgl(kuds, is.null) == F)
(propNotNull <- length(whichNotNull)/length(kuds))
kudsNotNull <- kuds[whichNotNull]
walk(kudsNotNull, image)

### Core Area (50%) ---------------------------------------------------------
kud_areas_50 <- map_dbl(kudsNotNull, ~kernel.area(.x, percent = 50), .progress =T)
### Home Range (95%) ---------------------------------------------------------
kud_areas_95 <- map_dbl(kudsNotNull, ~kernel.area(.x, percent = 95), .progress = T)

indsKUDAreas <- inds[whichNotNull,] %>%
  bind_cols("coreArea" = kud_areas_50,
            "homeRange" = kud_areas_95)

### Core Area Fidelity (50%/95%) --------------------------------------------
indsKUDAreas <- indsKUDAreas %>%
  mutate(coreAreaFidelity = coreArea/homeRange)

### 3D Home Range: Altitude -------------------------------------------------
# Get elevation information
# elevs_z09 <- elevatr::get_elev_raster(bs2022 %>%
#                                         st_transform(crs = "WGS84"),                                   z = 9)
# save(elevs_z09, file = "data/elevs_z09.Rda")
load("data/elevs_z09.Rda")

bs2022$groundElev_z09 <- raster::extract(x = elevs_z09, 
                                         y = bs2022 %>% 
                                           st_transform(crs = "WGS84"))
bs2022 <- bs2022 %>%
  mutate(height_above_ground = height_above_msl - groundElev_z09)

# set any negative differences to 0
bs2022 <- bs2022 %>%
  mutate(height_above_ground = case_when(height_above_ground < 0 ~ 0,
                                         TRUE ~ height_above_ground))

# calculate daily altitude stats
dailyAltitudes <- bs2022 %>%
  st_drop_geometry() %>%
  group_by(trackId, dateOnly) %>%
  summarize(meanAltitude = mean(height_above_ground, na.rm  =T),
            maxAltitude = max(height_above_ground, na.rm = T),
            medAltitude = median(height_above_ground, na.rm = T),
            propOver1km = sum(height_above_ground > 1000)/n())

dailyAltitudes %>%
  ggplot(aes(x = trackId, y = propOver1km))+
  geom_boxplot() 

dailyAltitudesSumm <- dailyAltitudes %>%
  group_by(trackId) %>%
  summarize(mnDailyMaxAlt = mean(maxAltitude, na.rm = T),
            mnDailyMnAlt = mean(meanAltitude, na.rm = T),
            mnDailyPropOver1km = mean(propOver1km, na.rm = T),
            mnDailyMedAlt = mean(medAltitude, na.rm = T))

## ROOST SITE USE ----------------------------------------------------------
## get roost locations
# r <- get_roosts_df(df = bs2022, id = "trackId", timestamp = "timestamp", x = "location_long", y = "location_lat", ground_speed = "ground_speed", speed_units = "m/s") %>%
#   st_as_sf(coords = c("location_long", "location_lat"), remove = F) %>%
#   st_set_crs("WGS84") %>%
#   st_transform(32636)
# save(r, file = "data/r.Rda")
load("data/r.Rda")

## make them fall into polygons
roostIDs <- as.numeric(st_intersects(x = r, y = roostPolygons))
r$roostID <- roostIDs
## For any that are NA, generate random ID's for the roosts
## how many are NA?
sum(is.na(r$roostID))
ids <- paste(r, 1:sum(is.na(r$roostID)))

### Roost Switches ----------------------------------------------------------
## Calculate sequences of roost locations
roostSequences <- r %>%
  st_drop_geometry() %>%
  dplyr::select(trackId, dateOnly, roostID) %>%
  arrange(trackId, dateOnly) %>%
  group_by(trackId) %>%
  mutate(diff = roostID - lag(roostID),
         switch = case_when(diff == 0 ~ F,
                            is.na(diff) ~ T,
                            TRUE ~ T))
## Calculate whether, on each night, they switch or stay at the same roost
roostSwitches <- roostSequences %>%
  group_by(trackId) %>%
  summarize(nSwitch = sum(switch),
            nStay = sum(!switch),
            nights = n(),
            propSwitch = nSwitch/nights) %>%
  dplyr::select(trackId, propSwitch)

roostSwitches %>%
  ggplot(aes(x = propSwitch))+
  geom_histogram(binwidth = 0.1, col = "black", fill = "lightblue")+
  theme_classic()+
  ylab("Number of vultures")+
  xlab("Prop. nights switching roosts")


### Roost Diversity -------------------------------------------------------
## (Shannon index)
shannon <- r %>%
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
  summarize(shannon = -sum(item))

## Roost ranking
ranking <- r %>%
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
  mutate(rank = 1:n())

mostUsedRoostProp <- ranking %>%
  group_by(trackId) %>%
  filter(rank == 1) %>%
  dplyr::select(trackId, "mostUsedRoostProp" = propUsed)

ranking %>%
  ggplot(aes(x = rank, y = propUsed, group = trackId))+
  #geom_point()+
  geom_line()

## MOVEMENT ----------------------------------------------------------------
### Daily Flight Duration ---------------------------------------------------
dfd <- bs2022 %>%
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
  summarize(totalFlightDuration = sum(dur, na.rm = T))

dfdSumm <- dfd %>%
  group_by(trackId) %>%
  summarize(meanDFD = mean(totalFlightDuration, na.rm = T),
            minDFD = min(totalFlightDuration, na.rm = T),
            maxDFD = max(totalFlightDuration, na.rm = T),
            medDFD = median(totalFlightDuration, na.rm = T))

### Daily Distance Traveled/Mean Displacement -------------------------------

# numCores <- parallel::detectCores() - 5
# cl <- parallel::makeCluster(numCores)
# 
# ## XX should rarify data to only keep points every 10 minutes. 
# dailyMovementList <- bs2022 %>%
#   #dplyr::filter(trackId %in% c("A00w", "A03w")) %>% # for testing, just one indiv
#   dplyr::group_by(trackId, dateOnly) %>%
#   dplyr::group_split(.keep = T)
# 
# start.mc <- Sys.time()
# mvmtList <- parallel::parLapply(dailyMovementList, function(x){
#   out <- dplyr::mutate(x,
#                        displacement = as.vector(sf::st_distance(geometry, geometry[1])),
#                        dist_m = sf::st_distance(geometry, dplyr::lag(geometry), by_element = T),
#                        difftime_s = difftime(timestamp, dplyr::lag(timestamp), units = "secs"),
#                        speed_ms = as.numeric(dist_m)/as.numeric(difftime_s),
#                        dist_10minEquiv = speed_ms*600,
#                        endpointDist = sf::st_distance(geometry[1], geometry[dplyr::n()]))
#   return(out)},  cl = cl)
# dailyMovement <- data.table::rbindlist(mvmtList)
# end.mc <- Sys.time()
# end.mc - start.mc
# head(dailyMovement)
# dailyMovement %>% is.na() %>% colSums()
# 
# save(dailyMovement, file = "data/dailyMovement.Rda")
load("data/dailyMovement.Rda") # we should only get one NA value per individual per day, which means that we should have as many individual*days as there are NA values. The only other way we could have an NA value here is if one of the geometries if NA, and I don't think there are any NA geometries. Also, the same rows should be NA for dist_m and difftime_s.

dailyMovement %>% st_drop_geometry() %>% dplyr::select(trackId, dateOnly) %>% distinct() %>% nrow() # okay good.

dailyMovementSumm <- dailyMovement %>%
  sf::st_drop_geometry() %>%
  group_by(trackId, dateOnly) %>%
  summarize(dmd = max(as.numeric(displacement), na.rm = T),
            ddt = sum(as.numeric(dist_m), na.rm = T),
            endpointDist = as.numeric(endpointDist)[1],
            tort = as.numeric(ddt)/as.numeric(endpointDist), .groups = "drop")

mnMvmt <- dailyMovementSumm %>%
  group_by(trackId) %>%
  summarize(meanDMD = mean(dmd, na.rm = T),
            meanDDT = mean(ddt, na.rm = T),
            sdDMD = sd(dmd, na.rm =T),
            sdDDT = sd(ddt, na.rm = T),
            mnTort = mean(tort, na.rm = T),
            sdTort = sd(tort, na.rm = T))

mnMvmt %>% is.na() %>% colSums() # yessssssssssssssss
dailyMovementSumm %>% is.na() %>% colSums() # also yessssss

## MULTIVARIATE MOVEMENT METRIC --------------------------------------------
## All movement behavior information
movementBehavior <- indsKUDAreas %>%
  left_join(., dailyAltitudesSumm, by = "trackId") %>%
  left_join(., roostSwitches, by = "trackId") %>%
  left_join(., shannon, by = "trackId") %>%
  left_join(., mostUsedRoostProp, by = "trackId") %>%
  left_join(., dfdSumm, by = "trackId") %>%
  left_join(., mnMvmt, by = "trackId") %>%
  na.omit()

## Scale all predictor vars
movementBehaviorScaled <- movementBehavior %>%
  mutate(across(-trackId, function(x) as.numeric(as.vector(scale(x)))))

movementBehaviorScaled_forPCA <- movementBehaviorScaled %>%dplyr::select(-c(mostUsedRoostProp, sdTort, mnDailyMnAlt, sdDMD, meanDFD, coreArea, propSwitch, mnDailyPropOver1km))
M <- cor(movementBehaviorScaled_forPCA[,-1])
corrplot(M, order = "AOE", type = "lower", diag = FALSE)

# Mfull <- cor(movementBehaviorScaled[,-1])
# corrplot(Mfull, order = "AOE", type = "lower", diag = FALSE)

#PCAs
pca_br2022 <- prcomp(x = movementBehaviorScaled_forPCA[,-1])

autoplot(pca_br2022, loadings = T, loadings.label = T)+theme_classic()+ggtitle("Breeding season 2022")

contrib_br2022 <- get_pca_var(pca_br2022)$contrib
fviz_screeplot(pca_br2022, addlabels = T)
contrib_br2022[,1:3]

# Kind of looks like our main axes are "movement range" and "amount of soaring/twisting/turning". Which kind of makes sense! # Okay but not anymore. Now they're something totally different and uninterpretable.

# Exploring questions that come up from the PCA
## Why is shannon opposite to propSwitch?
movementBehavior %>%
  ungroup() %>%
  ggplot(aes(x = propSwitch, y = shannon, col = as.numeric(uniqueRoosts)))+
  scale_color_viridis()+
  geom_point(size = 3)+
  geom_smooth(method = "lm")+
  theme_classic()# indeed, really strong relationship here. What's up with that? Individuals that switch roosts more often have lower roost diversity?

movementBehavior %>%
  ggplot(aes(x = uniqueRoosts/nNights, y = propSwitch))+
  geom_point()+
  geom_smooth(method = "lm")

movementBehavior %>%
  ggplot(aes(x = uniqueRoosts/nNights, y = shannon))+
  geom_point()+
  geom_smooth(method = "lm")

movementBehavior %>%
  ggplot(aes(x = propSwitch, y = shannon, col = mostUsedRoostProp))+
  geom_point(size = 2)+
  scale_color_viridis()+
  geom_smooth(method = "lm")

# Ok, the roost story isn't quite making sense to me, still. Let's come back to this and try to figure it out. I'm sure there's something to be learned if I look up the relationship between diversity and species richness for e.g. plant communities. 

# Social Networks ---------------------------------------------------------
load("data/datAnnotCleaned.Rda")
datAnnotCleaned <- datAnnotCleaned %>%
  sf::st_as_sf(coords = c("location_long", "location_lat"), crs = "WGS84", remove = F) %>%
  st_transform(32636)

roosts <- get_roosts_df(df = datAnnotCleaned, id = "trackId", timestamp = "timestamp", x = "location_long", y = "location_lat", ground_speed = "ground_speed", speed_units = "m/s") %>%
  st_as_sf(coords = c("location_long", "location_lat"), remove = F, crs = "WGS84") %>%
  st_transform(32636)

## Split into seasons
datAnnotCleaned <- datAnnotCleaned %>%
  mutate(month = lubridate::month(timestamp),
         year = lubridate::year(timestamp),
         season = case_when(month %in% 7:11 ~ "nb",
                            month %in% c(12, 1:6) ~ "b"),
         seasonUnique = case_when(season == "nb" ~ paste(season, year, sep = "_"),
                                  season == "b" & month == 12 ~ 
                                    paste(season, year+1, sep = "_"),
                                  TRUE ~ paste(season, year, sep = "_")))
## By month
months <- datAnnotCleaned %>%
  group_by(month, year) %>%
  group_split(.keep = T)
monthRoosts <- roosts %>%
  group_by(lubridate::month(timestamp), lubridate::year(timestamp)) %>%
  group_split(.keep = T)

flightMonths <- map(months, ~vultureUtils::getFlightEdges(.x, roostPolygons = roostPolygons, distThreshold = 1000, return = "sri"))
feedingMonths <- map(months, ~vultureUtils::getFeedingEdges(.x, roostPolygons = roostPolygons, distThreshold = 50, return = "sri"))
roostMonths <- map(monthRoosts, ~vultureUtils::getRoostEdges(.x, mode = "polygon", roostPolygons = roostPolygons, return = "sri"))

flightMonths_g <- map(flightMonths, ~vultureUtils::makeGraph(mode = "sri", data = .x, weighted = T)) %>%
  map(., ~delete.edges(.x, E(.x)[E(.x)$weight <= 0|is.na(E(.x)$weight)]))

feedingMonths_g <- map(feedingMonths,  ~vultureUtils::makeGraph(mode = "sri", data = .x, weighted = T)) %>%
  map(., ~delete.edges(.x, E(.x)[E(.x)$weight <= 0|is.na(E(.x)$weight)]))

roostMonths_g <- map(roostMonths,  ~vultureUtils::makeGraph(mode = "sri", data = .x, weighted = T)) %>%
  map(., ~delete.edges(.x, E(.x)[E(.x)$weight <= 0|is.na(E(.x)$weight)]))

monthsData <- imap_dfr(flightMonths_g, ~{
  df <- data.frame(degree = igraph::degree(.x),
                   strength = igraph::strength(.x),
                   pageRank = igraph::page_rank(.x)$vector,
                   trackId = names(degree(.x))) %>%
    bind_cols(month = .y,
              type = "flight")
  
  return(df)
}) %>%
  bind_rows(imap_dfr(feedingMonths_g, ~{
    df <- data.frame(degree = igraph::degree(.x),
                     strength = igraph::strength(.x),
                     pageRank = igraph::page_rank(.x)$vector,
                     trackId = names(degree(.x))) %>%
      bind_cols(month = .y,
                type = "feeding")
    
    return(df)
  })) %>%
  bind_rows(imap_dfr(roostMonths_g, ~{
    df <- data.frame(degree = igraph::degree(.x),
                     strength = igraph::strength(.x),
                     pageRank = igraph::page_rank(.x)$vector,
                     trackId = names(degree(.x))) %>%
      bind_cols(month = .y,
                type = "roosting")
    
    return(df)
  }))


## By season
seasons <- datAnnotCleaned %>%
  group_by(seasonUnique) %>%
  group_split(.keep = T)
seasonRoosts <- roosts %>%
  mutate(season = case_when(lubridate::month(timestamp) %in% 7:11 ~ "nb",
                            TRUE ~ "b"),
         seasonUnique = case_when(season == "b" & lubridate::month(timestamp) == 12 ~ paste(season, lubridate::year(timestamp)+1, sep = "_"),
                                  TRUE ~ paste(season, lubridate::year(timestamp), sep = "_"))) %>%
  group_by(seasonUnique) %>%
  group_split(.keep = T)

flightSeasons <- map(seasons, ~vultureUtils::getFlightEdges(.x, roostPolygons = roostPolygons, distThreshold = 1000, return = "sri"))
feedingSeasons <- map(seasons, ~vultureUtils::getFeedingEdges(.x, roostPolygons = roostPolygons, distThreshold = 50, return = "sri"))
roostSeasons <- map(seasonRoosts, ~vultureUtils::getRoostEdges(.x, mode = "polygon", roostPolygons = roostPolygons, return = "sri"))

flightSeasons_g <- map(flightSeasons, ~vultureUtils::makeGraph(mode = "sri", data = .x, weighted = T)) %>%
  map(., ~delete.edges(.x, E(.x)[E(.x)$weight <= 0|is.na(E(.x)$weight)]))
feedingSeasons_g <- map(feedingSeasons,  ~vultureUtils::makeGraph(mode = "sri", data = .x, weighted = T)) %>%
  map(., ~delete.edges(.x, E(.x)[E(.x)$weight <= 0|is.na(E(.x)$weight)]))
roostSeasons_g <- map(roostSeasons,  ~vultureUtils::makeGraph(mode = "sri", data = .x, weighted = T)) %>%
  map(., ~delete.edges(.x, E(.x)[E(.x)$weight <= 0|is.na(E(.x)$weight)]))

seasonsData <- imap_dfr(flightSeasons_g, ~{
  df <- data.frame(degree = igraph::degree(.x),
                   strength = igraph::strength(.x),
                   pageRank = igraph::page_rank(.x)$vector,
                   trackId = names(degree(.x))) %>%
    bind_cols(season = .y,
              type = "flight")
  
  return(df)
}) %>%
  bind_rows(imap_dfr(feedingSeasons_g, ~{
    df <- data.frame(degree = igraph::degree(.x),
                     strength = igraph::strength(.x),
                     pageRank = igraph::page_rank(.x)$vector,
                     trackId = names(degree(.x))) %>%
      bind_cols(season = .y,
                type = "feeding")
    
    return(df)
  })) %>%
  bind_rows(imap_dfr(roostSeasons_g, ~{
    df <- data.frame(degree = igraph::degree(.x),
                     strength = igraph::strength(.x),
                     pageRank = igraph::page_rank(.x)$vector,
                     trackId = names(degree(.x))) %>%
      bind_cols(season = .y,
                type = "roosting")
    
    return(df)
  }))


## Join month and season data
networkMetrics <- monthsData %>%
  pivot_longer(cols = "month", values_to = "value", names_to = "timePeriod") %>%
  bind_rows(seasonsData %>%
              pivot_longer(cols = "season", values_to = "value", names_to = "timePeriod")) %>%
  group_by(type, timePeriod, value) %>%
  mutate(n = n()) %>%
  ungroup() %>%
  mutate(degreeRelative = degree/n,
         strengthRelative = strength/n,
         pageRankRelative = pageRank/n,
         sbd = strength/degree)


# JOIN network metrics to movement data -----------------------------------

# get number of days tracked for each individual
daysTracked <- bs2022 %>%
  st_drop_geometry() %>%
  group_by(trackId) %>%
  summarize(daysTracked = length(unique(dateOnly)))

# JOIN
all <- left_join(networkMetrics, dailyMovementSumm, by = "trackId") %>% 
  left_join(., shannon, by = "trackId") %>% 
  left_join(., roostSwitches, by = "trackId") %>%
  left_join(., indsKUDAreas, by = "trackId") %>%
  left_join(., dfdSumm, by = "trackId") %>%
  left_join(., dailyAltitudesSumm, by = "trackId")

# now, excluding home range, we have our independent and dependent variables in the same place.

# Analysis -------------------------------------------------------
## For now, just looking at 2022 breeding season, which is the fourth season out of 6
seasonNames <- map_chr(seasons, ~.x$seasonUnique[1])

# 2022 breeding season
## Daily Max Displacement vs. Degree
all %>%
  filter(timePeriod == "season", value == 2) %>%
  ggplot(aes(x = meanDMD, y = degree, col = type))+
  geom_point()+
  geom_smooth(method = "lm")+
  facet_wrap(~type)+
  ylab("Degree centrality")+
  xlab("Mean daily max displacement")+
  theme_classic()+
  theme(legend.position = "none")

## Daily Max Displacement vs. Strength
all %>%
  filter(timePeriod == "season", value == 2) %>%
  ggplot(aes(x = meanDMD, y = strength, col = type))+
  geom_point()+
  geom_smooth(method = "lm")+
  facet_wrap(~type)+
  ylab("Strength centrality")+
  xlab("Mean daily max displacement")+
  theme_classic()+
  theme(legend.position = "none")

## Daily Max Displacement vs. Strength by Degree
all %>%
  filter(timePeriod == "season", value == 2) %>%
  ggplot(aes(x = meanDMD, y = sbd, col = type))+
  geom_point()+
  geom_smooth(method = "lm")+
  facet_wrap(~type)+
  ylab("Strength by degree")+
  xlab("Mean daily max displacement")+
  theme_classic()+
  theme(legend.position = "none")

## Daily Distance Traveled vs. Degree
all %>%
  filter(timePeriod == "season", value == 2) %>%
  ggplot(aes(x = meanDDT, y = degree, col = type))+
  geom_point()+
  geom_smooth(method = "lm")+
  facet_wrap(~type)+
  ylab("Degree centrality")+
  xlab("Mean daily distance traveled")+
  theme_classic()+
  theme(legend.position = "none")

## Daily Distance Traveled vs. Strength
all %>%
  filter(timePeriod == "season", value == 2) %>%
  ggplot(aes(x = meanDDT, y = strength, col = type))+
  geom_point()+
  geom_smooth(method = "lm")+
  facet_wrap(~type)+
  ylab("Strength centrality")+
  xlab("Mean daily distance traveled")+
  theme_classic()+
  theme(legend.position = "none")

## Daily Distance Traveled vs. Strength by Degree
all %>%
  filter(timePeriod == "season", value == 2) %>%
  ggplot(aes(x = meanDDT, y = sbd, col = type))+
  geom_point()+
  geom_smooth(method = "lm")+
  facet_wrap(~type)+
  ylab("Strength by degree")+
  xlab("Mean daily distance traveled")+
  theme_classic()+
  theme(legend.position = "none")

## Log Home Range Size vs. Degree
all %>%
  filter(timePeriod == "season", value == 2) %>%
  ggplot(aes(x = log(homeRange), y = degree, col = type))+
  geom_point()+
  geom_smooth(method = "lm")+
  facet_wrap(~type, scales = "free_y")+
  ylab("Degree centrality")+
  xlab("Log home range size")+
  theme_classic()+
  theme(legend.position = "none")

## Log Home Range Size vs. Strength
all %>%
  filter(timePeriod == "season", value == 2) %>%
  ggplot(aes(x = log(homeRange), y = strength, col = type))+
  geom_point()+
  geom_smooth(method = "lm")+
  facet_wrap(~type, scales = "free_y")+
  ylab("Strength centrality")+
  xlab("Log home range size")+
  theme_classic()+
  theme(legend.position = "none")

## Log Home Range Size vs. Strength
all %>%
  filter(timePeriod == "season", value == 2) %>%
  ggplot(aes(x = log(homeRange), y = sbd, col = type))+
  geom_point()+
  geom_smooth(method = "lm")+
  facet_wrap(~type, scales = "free_y")+
  ylab("Strength by degree")+
  xlab("Log home range size")+
  theme_classic()+
  theme(legend.position = "none")

## Daily Distance Traveled vs. Degree, 2021 and 2022 breeding seasons
all %>%
  filter(timePeriod == "season", value %in% 1:2) %>%
  ggplot(aes(x = meanDDT, y = degreeRelative, col = factor(value)))+
  geom_point()+
  geom_smooth(method = "lm")+
  scale_color_manual(name = "Year", values = c("red", "blue"))+
  theme_classic()+
  theme(text = element_text(size = 18))+
  facet_wrap(~type, scales = "free_y")+
  ylab("Relative degree")+
  xlab("Mean daily distance traveled")

## Daily Distance Traveled vs. Strength, 2021 and 2022 breeding seasons
all %>%
  filter(timePeriod == "season", value %in% 1:2) %>%
  ggplot(aes(x = meanDDT, y = strengthRelative, col = factor(value)))+
  geom_point()+
  geom_smooth(method = "lm")+
  scale_color_manual(name = "Year", values = c("red", "blue"))+
  theme_classic()+
  theme(text = element_text(size = 18))+
  facet_wrap(~type, scales = "free_y")+
  ylab("Relative strength")+
  xlab("Mean daily distance traveled")

## Daily Distance Traveled vs. Strength-by-degree, 2021 and 2022 breeding seasons
all %>%
  filter(timePeriod == "season", value %in% 1:2) %>%
  ggplot(aes(x = meanDDT, y = sbd, col = factor(value)))+
  geom_point()+
  geom_smooth(method = "lm")+
  scale_color_manual(name = "Year", values = c("red", "blue"))+
  theme_classic()+
  theme(text = element_text(size = 18))+
  facet_wrap(~type, scales = "free_y")+
  ylab("Strength by degree")+
  xlab("Mean daily distance traveled")










all %>%
  filter(timePeriod == "season", value %in% 1:2, type == "feeding") %>%
  ggplot(aes(x = meanDDT, y = degreeRelative, col = factor(value)))+
  geom_point()+
  geom_smooth(method = "lm")+
  scale_color_manual(name = "Year", values = c("red", "blue"))+
  theme_classic()+
  ggtitle("Co-feeding (breeding)")+
  theme(text = element_text(size = 18))+
  ylim(c(0, 1))+
  ylab("Relative degree")+
  xlab("Mean daily distance traveled")

all %>%
  filter(timePeriod == "season", value %in% 1:2, type == "flight") %>%
  ggplot(aes(x = meanDDT, y = strengthRelative, col = factor(value)))+
  geom_point()+
  geom_smooth(method = "lm")+
  scale_color_manual(name = "Year", values = c("red", "blue"))+
  theme_classic()+
  ggtitle("Co-flight (breeding)")+
  theme(text = element_text(size = 18))+
  ylab("Relative strength (strength/n)")+
  xlab("Mean daily distance traveled")

all %>%
  filter(timePeriod == "season", value %in% 1:2, type == "feeding") %>%
  ggplot(aes(x = meanDDT, y = strengthRelative, col = factor(value)))+
  geom_point()+
  geom_smooth(method = "lm")+
  scale_color_manual(name = "Year", values = c("red", "blue"))+
  theme_classic()+
  ggtitle("Co-feeding (breeding)")+
  theme(text = element_text(size = 18))+
  ylab("Relative strength (strength/n)")+
  xlab("Mean daily distance traveled")


