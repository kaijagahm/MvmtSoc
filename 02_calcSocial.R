# Setup ---------------------------------------------------------------
## Packages ---------------------------------------------------------------
library(vultureUtils)
library(igraph)
library(tidyverse)
library(sf)

## Data ---------------------------------------------------------------
load("data/seasons.Rda")
roostPolygons <- sf::st_read("data/roosts50_kde95_cutOffRegion.kml")
load("data/roosts_seasons.Rda")
roosts_seasons <- map(roosts_seasons, ~.x %>% mutate(location_lat = st_coordinates(.)[1],
                                                     location_long = st_coordinates(.)[2]))
seasonNames <- map_chr(seasons, ~.x$seasonUnique[1])

# Social Networks ---------------------------------------------------------
flightSeasons <- map(seasons, ~vultureUtils::getFlightEdges(.x, roostPolygons = roostPolygons, distThreshold = 1000, return = "sri"))
feedingSeasons <- map(seasons, ~vultureUtils::getFeedingEdges(.x, roostPolygons = roostPolygons, distThreshold = 50, return = "sri"))
roostSeasons <- map(roosts_seasons, ~vultureUtils::getRoostEdges(.x, mode = "polygon", roostPolygons = roostPolygons, return = "sri", latCol = "location_lat", longCol = "location_long", idCol = "trackId", dateCol = "roost_date"))

flightSeasons_g <- map(flightSeasons, ~vultureUtils::makeGraph(mode = "sri", data = .x, weighted = T))
feedingSeasons_g <- map(feedingSeasons,  ~vultureUtils::makeGraph(mode = "sri", data = .x, weighted = T))
roostSeasons_g <- map(roostSeasons,  ~vultureUtils::makeGraph(mode = "sri", data = .x, weighted = T))

networkMetrics <- map2_dfr(flightSeasons_g, seasonNames, ~{
  df <- data.frame(degree = igraph::degree(.x),
                   strength = igraph::strength(.x),
                   pageRank = igraph::page_rank(.x)$vector,
                   trackId = names(degree(.x))) %>%
    bind_cols(season = .y,
              type = "flight")
  
  return(df)
}) %>%
  bind_rows(map2_dfr(feedingSeasons_g, seasonNames, ~{
    df <- data.frame(degree = igraph::degree(.x),
                     strength = igraph::strength(.x),
                     pageRank = igraph::page_rank(.x)$vector,
                     trackId = names(degree(.x))) %>%
      bind_cols(season = .y,
                type = "feeding")
    
    return(df)
  })) %>%
  bind_rows(map2_dfr(roostSeasons_g, seasonNames, ~{
    df <- data.frame(degree = igraph::degree(.x),
                     strength = igraph::strength(.x),
                     pageRank = igraph::page_rank(.x)$vector,
                     trackId = names(degree(.x))) %>%
      bind_cols(season = .y,
                type = "roosting")
    
    return(df)
  }))

# Add info about number of indivs and relative measures
networkMetrics <- networkMetrics %>%
  group_by(season) %>%
  mutate(n = n()) %>%
  ungroup() %>%
  mutate(degreeRelative = degree/n,
         strengthRelative = strength/n,
         pageRankRelative = pageRank/n,
         sbd = strength/degree) %>%
  dplyr::select(season, type, n, trackId, degree, degreeRelative, strength, strengthRelative, sbd, pageRank, pageRankRelative)

# Save network metrics ----------------------------------------------------
save(networkMetrics, file = "data/networkMetrics.Rda")
