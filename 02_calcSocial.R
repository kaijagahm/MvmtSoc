# Setup ---------------------------------------------------------------
## Packages ---------------------------------------------------------------
library(vultureUtils)
library(igraph)
# In lieu of tidyverse
library(ggplot2)
library(tidyr)
library(stringr)
library(readr)
library(dplyr)
library(tibble)
library(purrr)
#
library(sf)
source("evenness.R")

## Data ---------------------------------------------------------------
load("data/seasons_forSoc.Rda")
load("data/seasons_forSoc_mode10.Rda")
seasons_forSoc <- map(seasons_forSoc, ~st_as_sf(.x, coords = c("location_long", "location_lat"), crs = "WGS84", remove = F))
roostPolygons <- sf::st_read("data/roosts50_kde95_cutOffRegion.kml")
load("data/roosts_seasons.Rda")
load("data/roosts_seasons_mode10.Rda")
load("data/datasetAssignments.Rda")

seasonNames <- map_chr(seasons_forSoc, ~as.character(.x$seasonUnique[1]))
seasons_forSoc <- seasons_forSoc[-which(seasonNames == "2020_summer")]
seasons_forSoc_mode10 <- seasons_forSoc_mode10[-which(seasonNames == "2020_summer")]
datasetAssignments <- datasetAssignments[-which(seasonNames == "2020_summer")]
roosts_seasons <- roosts_seasons[-which(seasonNames == "2020_summer")]
roosts_seasons_mode10 <- roosts_seasons_mode10[-which(seasonNames == "2020_summer")]
seasonNames <- seasonNames[-which(seasonNames == "2020_summer")]

# Investigate the data ----------------------------------------------------
# How many individuals do we have with vs. without excluding those with a lower sampling rate?
all <- map_dbl(seasons_forSoc, ~length(unique(.x$Nili_id)))
highFixRate <- map_dbl(seasons_forSoc_mode10, ~length(unique(.x$Nili_id)))
diff <- all - highFixRate # we should not have both positive and negative values here. That's bad. 
diff # number of individuals lost when we restrict it to only those with a high fix rate. 

# Social Networks ---------------------------------------------------------
flightSeasons <- map(seasons_forSoc, ~vultureUtils::getFlightEdges(.x, roostPolygons = roostPolygons, distThreshold = 1000, idCol = "Nili_id", return = "sri"))
flightSeasons_mode10 <- map(seasons_forSoc_mode10, ~vultureUtils::getFlightEdges(.x, roostPolygons = roostPolygons, distThreshold = 1000, idCol = "Nili_id", return = "sri"))

# XXX just testing here!
flightEdges <- map(seasons_forSoc, ~vultureUtils::getFlightEdges(.x, roostPolygons = roostPolygons, distThreshold = 1000, idCol = "Nili_id", return = "edges"))
flightEdges_15min <- map(seasons_forSoc, ~vultureUtils::getFlightEdges(.x, roostPolygons = roostPolygons, distThreshold = 1000, timeThreshold = "15 minutes", idCol = "Nili_id", return = "edges"))
flightEdges_mode10 <- map(seasons_forSoc_mode10, ~vultureUtils::getFlightEdges(.x, roostPolygons = roostPolygons, distThreshold = 1000, idCol = "Nili_id", return = "edges"))
flightEdges_mode10_15min <- map(seasons_forSoc_mode10, ~vultureUtils::getFlightEdges(.x, roostPolygons = roostPolygons, distThreshold = 1000, timeThreshold = "15 minutes", idCol = "Nili_id", return = "edges"))

feedingEdges <- map(seasons_forSoc, ~vultureUtils::getFeedingEdges(.x, roostPolygons = roostPolygons, distThreshold = 50, idCol = "Nili_id", return = "edges"))
feedingEdges_15min <- map(seasons_forSoc, ~vultureUtils::getFeedingEdges(.x, roostPolygons = roostPolygons, distThreshold = 50, timeThreshold = "15 minutes", idCol = "Nili_id", return = "edges"))
feedingEdges_mode10 <- map(seasons_forSoc_mode10, ~vultureUtils::getFeedingEdges(.x, roostPolygons = roostPolygons, distThreshold = 50, idCol = "Nili_id", return = "edges"))
feedingEdges_mode10_15min <- map(seasons_forSoc_mode10, ~vultureUtils::getFeedingEdges(.x, roostPolygons = roostPolygons, distThreshold = 50, timeThreshold = "15 minutes", idCol = "Nili_id", return = "edges"))

l <- list(flightEdges, flightEdges_15min, flightEdges_mode10, flightEdges_mode10_15min, feedingEdges, feedingEdges_15min, feedingEdges_mode10, feedingEdges_mode10_15min)
save(l, file = "l.Rda")
rows <- map(l, ~.x %>% map_dbl(., nrow) %>% as.data.frame() %>% mutate(season = factor(seasonNames, levels = seasonNames)))
rowsdf <- purrr::list_rbind(rows, names_to = "which") %>%
  rename("nInteractions" = ".") %>%
  mutate(type = case_when(which %in% 1:4 ~ "flight",
                          TRUE ~ "feeding"),
         mins = case_when(which %in% c(1, 3, 5, 7) ~ 10,
                          TRUE ~ 15),
         restricted = case_when(which %in% c(1, 2, 5, 6) ~ F,
                                TRUE ~ T))
rowsdf %>%
  ggplot(aes(x = season, y = nInteractions))+
  geom_point(aes(col = type, pch = restricted))+
  geom_line(aes(col = type, lty = restricted))

# Comparison of number of interactions
nInteractions_all_flight <- map_dbl(flightEdges, nrow)
nInteractions_mode10_flight <- map_dbl(flightEdges_mode10, nrow)
diff_flight <- nInteractions_all_flight - nInteractions_mode10_flight
propDiff_flight <- diff_flight/nInteractions_all_flight

nInteractions_all_feeding <- map_dbl(feedingEdges, nrow)
nInteractions_mode10_feeding <- map_dbl(feedingEdges_mode10, nrow)
diff_flight <- nInteractions_all_flight - nInteractions_mode10_flight
propDiff <- diff_flight/nInteractions_all_flight
# XXX end testing



feedingSeasons <- map(seasons_forSoc, ~vultureUtils::getFeedingEdges(.x, roostPolygons = roostPolygons, distThreshold = 50, return = "sri")) # XXX start here
feedingSeasons_mode10 <- map(seasons_forSoc_mode10, ~vultureUtils::getFeedingEdges(.x, roostPolygons = roostPolygons, distThreshold = 50, return = "sri")) # XXX start here
roostSeasons <- map(roosts_seasons, ~vultureUtils::getRoostEdges(.x, mode = "polygon", roostPolygons = roostPolygons, return = "sri", latCol = "location_lat", longCol = "location_long", idCol = "Nili_id", dateCol = "roost_date"))

# #
save(flightSeasons, file = "data/flightSeasons.Rda")
save(feedingSeasons, file = "data/feedingSeasons.Rda")
save(roostSeasons, file = "data/roostSeasons.Rda")

load("data/flightSeasons.Rda")
load("data/feedingSeasons.Rda")
load("data/roostSeasons.Rda")

flightSeasons_g <- map(flightSeasons, ~vultureUtils::makeGraph(mode = "sri", data = .x, weighted = T))
feedingSeasons_g <- map(feedingSeasons,  ~vultureUtils::makeGraph(mode = "sri", data = .x, weighted = T))
roostSeasons_g <- map(roostSeasons,  ~vultureUtils::makeGraph(mode = "sri", data = .x, weighted = T))

flightSeasons_g_mode10 <- map(flightSeasons_mode10, ~vultureUtils::makeGraph(mode = "sri", data = .x, weighted = T))
feedingSeasons_g_mode10 <- map(feedingSeasons_mode10,  ~vultureUtils::makeGraph(mode = "sri", data = .x, weighted = T))

networkMetrics <- map2_dfr(flightSeasons_g, seasonNames, ~{
  df <- data.frame(degree = igraph::degree(.x),
                   strength = igraph::strength(.x),
                   pageRank = igraph::page_rank(.x)$vector,
                   evenness = evenness(.x),
                   Nili_id = names(degree(.x))) %>%
    bind_cols(season = .y,
              type = "flight")
  
  return(df)
}) %>%
  bind_rows(map2_dfr(feedingSeasons_g, seasonNames, ~{
    df <- data.frame(degree = igraph::degree(.x),
                     strength = igraph::strength(.x),
                     pageRank = igraph::page_rank(.x)$vector,
                     evenness = evenness(.x),
                     Nili_id = names(degree(.x))) %>%
      bind_cols(season = .y,
                type = "feeding")
    
    return(df)
  })) %>%
  bind_rows(map2_dfr(roostSeasons_g, seasonNames, ~{
    df <- data.frame(degree = igraph::degree(.x),
                     strength = igraph::strength(.x),
                     pageRank = igraph::page_rank(.x)$vector,
                     evenness = evenness(.x),
                     Nili_id = names(degree(.x))) %>%
      bind_cols(season = .y,
                type = "roosting")

    return(df)
  }))

networkMetrics_mode10 <- map2_dfr(flightSeasons_g_mode10, seasonNames, ~{
  df <- data.frame(degree = igraph::degree(.x),
                   strength = igraph::strength(.x),
                   pageRank = igraph::page_rank(.x)$vector,
                   evenness = evenness(.x),
                   Nili_id = names(degree(.x))) %>%
    bind_cols(season = .y,
              type = "flight")
  
  return(df)
}) %>%
  bind_rows(map2_dfr(feedingSeasons_g_mode10, seasonNames, ~{
    df <- data.frame(degree = igraph::degree(.x),
                     strength = igraph::strength(.x),
                     pageRank = igraph::page_rank(.x)$vector,
                     evenness = evenness(.x),
                     Nili_id = names(degree(.x))) %>%
      bind_cols(season = .y,
                type = "feeding")
    
    return(df)
  }))

# Add info about number of indivs and relative measures
networkMetrics <- networkMetrics %>%
  group_by(season, type) %>%
  mutate(n = length(unique(Nili_id))) %>% # CAREFUL HERE! Was getting the wrong degree bc conflating number of rows with number of individuals.
  ungroup() %>%
  mutate(degreeRelative = degree/n,
         strengthRelative = strength/n,
         pageRankRelative = pageRank/n,
         sbd = strength/degree,
         evenness = evenness) %>%
  dplyr::select(season, type, n, Nili_id, degree, degreeRelative, strength, strengthRelative, sbd, pageRank, pageRankRelative, evenness)

networkMetrics_mode10 <- networkMetrics_mode10 %>%
  group_by(season, type) %>%
  mutate(n = length(unique(Nili_id))) %>% 
  ungroup() %>%
  mutate(degreeRelative = degree/n,
         strengthRelative = strength/n,
         pageRankRelative = pageRank/n,
         sbd = strength/degree,
         evenness = evenness) %>%
  dplyr::select(season, type, n, Nili_id, degree, degreeRelative, strength, strengthRelative, sbd, pageRank, pageRankRelative, evenness)

# How do the degree distributions compare?
networkMetrics %>%
  ggplot(aes(x = degreeRelative))+
  geom_density()+
  

# Save network metrics ----------------------------------------------------
# Assign datasets
datasetAssignments <- map2(datasetAssignments, seasonNames, ~.x %>% mutate(seasonUnique = .y)) %>% purrr::list_rbind()

networkMetrics <- left_join(networkMetrics, datasetAssignments, by = c("Nili_id", "season" = "seasonUnique"))

save(networkMetrics, file = "data/networkMetrics.Rda")

# Investigate zeroes ------------------------------------------------------
# Are zeroes for degree more likely to come from one dataset vs the other?
networkMetrics %>%
  filter(degree == 0) %>%
  pull(dataset) %>% table() # hmm, nope, looks like they are spread over both.

# What about a two-way table?
zer <- networkMetrics %>% filter(degree == 0)
table(zer$season, zer$dataset) # there are more consistently some zeroes in the inpa data, but it's generally fairly well spread, making me think that these zeroes are real. Hmm.. Is it the same individuals over and over again?

zer %>% group_split(season) %>% map(., ~.x %>% pull(Nili_id) %>% unique() %>% sort()) # okay we definitely have some repeats, but they aren't blatantly all the same.

# So I guess I conclude from this that the zeroes are real and should be treated as real... which probably means we need