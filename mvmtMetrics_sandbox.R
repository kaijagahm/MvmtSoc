# Script for a preliminary investigation of movement metrics

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

## Load data ---------------------------------------------------------------
load("data/datAnnotCleaned.Rda")
roostPolygons <- sf::st_read("data/roosts25_cutOffRegion.kml") %>%
  sf::st_set_crs("WGS84") %>%
  sf::st_transform(32636)

# Time: restrict to 2021-2022 breeding season -----------------------------
bs <- datAnnotCleaned %>%
  filter(timestamp > lubridate::ymd("2021-12-01"), 
         timestamp < lubridate::ymd("2022-07-01"))

# Space: restrict to southern indivs --------------------------------------
## Make it an sf object
bs <- bs %>%
  sf::st_as_sf(coords = c("location_long", "location_lat"), remove = F) %>%
  sf::st_set_crs("WGS84") %>%
  sf::st_transform(32636)

centroids <- bs %>%
  group_by(trackId) %>%
  summarize(geometry = st_union(geometry)) %>%
  st_centroid()
#mapview(centroids, zcol = "trackId")

hist(st_coordinates(centroids)[,2]) # plot y coords to find a good cutoff point

## filter to only southern indivs
southernIndivs <- centroids %>%
  filter(st_coordinates(.)[,2] < 3550000) %>%
  pull(trackId)

bs <- bs %>%
  filter(trackId %in% southernIndivs)

## For how many days is each individual tracked?
bs %>%
  group_by(trackId) %>%
  summarize(nDaysTracked = length(unique(dateOnly)))

# Movement Behavior --------------------------------------------------------

## HOME RANGE ---------------------------------------------------------------
hrList <- bs %>%
  dplyr::select(trackId) %>%
  st_transform(32636) %>%
  mutate(x = st_coordinates(.)[,1], y = st_coordinates(.)[,2]) %>%
  st_drop_geometry() %>%
  group_by(trackId) %>%
  group_split(.keep = T)
hrListSP <- map(hrList, ~SpatialPoints(.x[,c("x", "y")]))
kuds <- map(hrListSP, ~{
  if(nrow(.x@coords)>= 5){
    k <- kernelUD(.x, h = "href", grid = 100, extent = 3)}
  else{
    k <- NULL
  }
  return(k)}, .progress = T)

inds <- map_dfr(hrList, ~.x[1, "trackId"])

whichNotNull <- which(map_lgl(kuds, is.null) == F)
kudsNotNull <- kuds[whichNotNull]


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
## this is extremely preliminary. Will need to incorporate a DEM. Right now I'm just curious to include the altitude and see what characteristics emerge.
altitudes <- bs %>%
  dplyr::select(trackId, dateOnly, timestamp, location_lat, location_long, height_above_msl) %>% # correct for lowest possible elevation
  mutate(height_above_msl = case_when(height_above_msl < -430 ~ -430,
                                      TRUE ~ height_above_msl),
         hour = lubridate::hour(timestamp))
hist(altitudes$height_above_msl)

# mean/median/max daily flight altitudes
# dailyAltitudes <- altitudes %>%
#   group_by(trackId, dateOnly) %>%
#   summarize(meanAltitude = mean(height_above_msl, na.rm = T),
#             maxAltitude = max(height_above_msl, na.rm = T),
#             medAltitude = median(height_above_msl, na.rm = T),
#             propOver1km = sum(height_above_msl > 1000)/n())
# save(dailyAltitudes, file = "data/dailyAltitudes.Rda")
load("data/dailyAltitudes.Rda")

# It doesn't make sense that some birds are spending nearly all their time in a given day over 1km high...oh wait yes it does, because Israel has mountains/high points, and it could be roosting there. Okay so I clearly can't really do this analysis before adding the DEM. Ah well.

### exploration: expect tight correlation between pct time spent over 1km and mean altitude
dailyAltitudes %>%
  mutate(year = lubridate::year(dateOnly)) %>%
  ggplot(aes(x = propOver1km, y = maxAltitude, col = factor(year)))+
  geom_point()+
  geom_smooth(method = "lm")+
  facet_wrap(~year)

## ROOST SITE USE ----------------------------------------------------------
## get roost locations
# r <- get_roosts_df(df = bs, id = "trackId", timestamp = "timestamp", x = "location_long", y = "location_lat", ground_speed = "ground_speed", speed_units = "m/s") %>%
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
  group_by(trackId) %>%
  summarize(nNights = n(),
            uniqueVals = length(unique(roostID)),
            shannon = vegan::diversity(as.numeric(as.factor(roostID)), 
                                       index = "shannon")) %>%
  dplyr::select(trackId, shannon)


## MOVEMENT ----------------------------------------------------------------
### Tortuosity --------------------------------------------------------------
# XXX to do!

### Daily Flight Duration ---------------------------------------------------
dfd <- bs %>%
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
  summarize(MDFD = mean(totalFlightDuration, na.rm = T),
            minDFD = min(totalFlightDuration, na.rm = T),
            maxDFD = max(totalFlightDuration, na.rm = T),
            medDFD = median(totalFlightDuration, na.rm = T))

### Daily Distance Traveled/Mean Displacement -------------------------------
dailyMovement <- bs %>%
  group_by(trackId, dateOnly) %>%
  summarize(displacement = st_distance(geometry, geometry[1]),
            lead = geometry[row_number()+1],
            difftime_s = difftime(timestamp, lag(timestamp), units = "secs"),
            dist_m = st_distance(geometry, lead, by_element = T),
            speed_ms = as.numeric(dist_m)/as.numeric(difftime_s),
            dist_10minEquiv = speed_ms*600)
save(dailyMovement, file = "data/dailyMovement.Rda")
load("data/dailyMovement.Rda")

dailyMovementSumm <- dailyMovement %>%
  sf::st_drop_geometry() %>%
  group_by(trackId, dateOnly) %>%
  summarize(dmd = max(as.numeric(displacement), na.rm = T),
            ddt = sum(as.numeric(dist_m), na.rm = T))

dailyMovementSumm %>%
  ggplot(aes(x = dmd))+
  geom_histogram()+
  xlab("Daily Max Displacement (m)")

dailyMovementSumm %>%
  ggplot(aes(x = ddt))+
  geom_histogram()+
  xlab("Daily Distance Traveled (m)")

## MULTIVARIATE MOVEMENT METRIC --------------------------------------------
## Scale all predictor vars
allScaled <- all %>%
  group_by(timePeriod, value, type) %>%
  mutate(meanDMD = as.vector(scale(meanDMD)),
         meanDDT = as.vector(scale(meanDDT)),
         shannon = as.vector(scale(shannon)),
         propSwitch = as.vector(scale(propSwitch)),
         coreArea = as.vector(scale(coreArea)),
         homeRange = as.vector(scale(homeRange)),
         hr50_95 = as.vector(scale(hr50_95)))

allScaled_br2021_mat <- allScaled %>% 
  filter(timePeriod == "season", value == 1) %>%
  ungroup() %>%
  dplyr::select(meanDMD, meanDDT, shannon, propSwitch, coreArea, 
                homeRange, hr50_95) %>%
  filter(!is.na(meanDMD)) %>%
  filter(!is.na(hr50_95))

allScaled_br2022_mat <- allScaled %>% 
  filter(timePeriod == "season", value == 2) %>%
  ungroup() %>%
  dplyr::select(meanDMD, meanDDT, shannon, propSwitch, coreArea, 
                homeRange, hr50_95) %>%
  filter(!is.na(meanDMD)) %>%
  filter(!is.na(hr50_95))

# PCAs
pca_br2021 <- prcomp(x = allScaled_br2021_mat)
pca_br2022 <- prcomp(x = allScaled_br2022_mat)

autoplot(pca_br2021, loadings = T, loadings.label = T)+theme_classic()+ggtitle("Breeding season 2021")
autoplot(pca_br2022, loadings = T, loadings.label = T)+theme_classic()+ggtitle("Breeding season 2022")

contrib_br2021 <- get_pca_var(pca_br2021)$contrib
contrib_br2021[,1:3]
contrib_br2022 <- get_pca_var(pca_br2022)$contrib
contrib_br2022[,1:3]

# Exploring questions that come up from the PCA
## Shannon roost diversity vs. roost switching percentage
all %>%
  ggplot(aes(x = propSwitch, y = shannon))+
  geom_point()+
  geom_smooth(method = "lm")


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
mnMvmt <- dailyMovementSumm %>%
  group_by(trackId) %>%
  summarize(meanDMD = mean(dmd, na.rm = T),
            sdDMD = sd(dmd, na.rm = T),
            meanDDT = mean(ddt, na.rm = T),
            sdDDT = sd(ddt, na.rm = T))

# get number of days tracked for each individual
daysTracked <- bs %>%
  st_drop_geometry() %>%
  group_by(trackId) %>%
  summarize(daysTracked = length(unique(dateOnly)))

# JOIN
all <- left_join(networkMetrics, mnMvmt, by = "trackId") %>% 
  left_join(., shannon, by = "trackId") %>% 
  left_join(., roostSwitches, by = "trackId") %>%
  left_join(., indsKUDAreas, by = "trackId") %>%
  left_join(., dfdSumm, by = "trackId") %>%
  left_join(., daysTracked, by = "trackId")

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


