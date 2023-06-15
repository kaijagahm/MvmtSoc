# Setup ---------------------------------------------------------------
## Packages ---------------------------------------------------------------
library(vultureUtils)
library(igraph)
library(tidyverse)
library(sf)
source("evenness.R")

## Data ---------------------------------------------------------------
load("data/seasons_forSoc.Rda")
seasons_forSoc <- map(seasons_forSoc, ~st_as_sf(.x, coords = c("location_long", "location_lat"), crs = "WGS84", remove = F))
roostPolygons <- sf::st_read("data/roosts50_kde95_cutOffRegion.kml")
load("data/roosts_seasons.Rda")
seasonNames <- map_chr(seasons_forSoc, ~as.character(.x$seasonUnique[1]))

# Social Networks ---------------------------------------------------------
# flightSeasons <- map(seasons_forSoc, ~vultureUtils::getFlightEdges(.x, roostPolygons = roostPolygons, distThreshold = 1000, idCol = "Nili_id", return = "sri"))
# feedingSeasons <- map(seasons_forSoc, ~vultureUtils::getFeedingEdges(.x, roostPolygons = roostPolygons, distThreshold = 50, return = "sri")) # XXX start here
# roostSeasons <- map(roosts_seasons, ~vultureUtils::getRoostEdges(.x, mode = "polygon", roostPolygons = roostPolygons, return = "sri", latCol = "location_lat", longCol = "location_long", idCol = "Nili_id", dateCol = "roost_date"))
# 
# save(flightSeasons, file = "data/flightSeasons.Rda")
# save(feedingSeasons, file = "data/feedingSeasons.Rda")
# save(roostSeasons, file = "data/roostSeasons.Rda")

load("data/flightSeasons.Rda")
load("data/feedingSeasons.Rda")
load("data/roostSeasons.Rda")

flightSeasons_g <- map(flightSeasons, ~vultureUtils::makeGraph(mode = "sri", data = .x, weighted = T))
feedingSeasons_g <- map(feedingSeasons,  ~vultureUtils::makeGraph(mode = "sri", data = .x, weighted = T))
roostSeasons_g <- map(roostSeasons,  ~vultureUtils::makeGraph(mode = "sri", data = .x, weighted = T))

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

# Add info about number of indivs and relative measures
networkMetrics <- networkMetrics %>%
  group_by(season, type) %>%
  mutate(n = length(unique(Nili_id))) %>% # CAREFUL HERE! wrong number of rows.
  ungroup() %>%
  mutate(degreeRelative = degree/n,
         strengthRelative = strength/n,
         pageRankRelative = pageRank/n,
         sbd = strength/degree,
         evenness = evenness) %>%
  dplyr::select(season, type, n, Nili_id, degree, degreeRelative, strength, strengthRelative, sbd, pageRank, pageRankRelative, evenness)

# Save network metrics ----------------------------------------------------
save(networkMetrics, file = "data/networkMetrics.Rda")
