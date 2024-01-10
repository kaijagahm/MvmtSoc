# Setup ---------------------------------------------------------------
## Packages ---------------------------------------------------------------
library(vultureUtils)
library(igraph)
library(tidyverse)
library(sf)
source("scripts/evenness.R")

## Data ---------------------------------------------------------------
load("data/derived/seasons_forSoc.Rda")
load("data/derived/seasons_forSoc_mode10.Rda")
seasons_forSoc <- map(seasons_forSoc, ~st_as_sf(.x, coords = c("location_long", "location_lat"), crs = "WGS84", remove = F))
seasons_forSoc_mode10 <- map(seasons_forSoc_mode10, ~st_as_sf(.x, coords = c("location_long", "location_lat"), crs = "WGS84", remove = F))
roostPolygons <- sf::st_read("data/raw/roosts50_kde95_cutOffRegion.kml")
load("data/derived/roosts_seasons.Rda")
load("data/derived/roosts_seasons_mode10.Rda")
load("data/orphan/datasetAssignments.Rda")

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

round((map_dbl(seasons_forSoc, nrow) - map_dbl(seasons_forSoc_mode10, nrow))/map_dbl(seasons_forSoc, nrow), 3)*100

# How many did we add by including the INPA data?
indivs_all <- map2(datasetAssignments, seasons_forSoc, ~.x %>% filter(Nili_id %in% .y$Nili_id)) %>%
  map_dfr(., ~.x %>% pull(dataset) %>% table())

indivs_mode10 <- map2(datasetAssignments, seasons_forSoc_mode10, ~.x %>% filter(Nili_id %in% .y$Nili_id)) %>%
  map_dfr(., ~.x %>% pull(dataset) %>% table())

# Social Networks ---------------------------------------------------------
flightSeasons_mode10 <- map(seasons_forSoc_mode10, ~vultureUtils::getFlightEdges(.x, roostPolygons = roostPolygons, distThreshold = 1000, idCol = "Nili_id", return = "both", getLocs = T))
flightSeasons_mode10_edges <- map(flightSeasons_mode10, ~.x$edges)
flightSeasons_mode10 <- map(flightSeasons_mode10, ~.x$sri)

feedingSeasons_mode10 <- map(seasons_forSoc_mode10, ~vultureUtils::getFeedingEdges(.x, roostPolygons = roostPolygons, distThreshold = 50, return = "both", getLocs = T))
feedingSeasons_mode10_edges <- map(feedingSeasons_mode10, ~.x$edges)
feedingSeasons_mode10 <- map(feedingSeasons_mode10, ~.x$sri)

roostSeasons <- map(roosts_seasons, ~vultureUtils::getRoostEdges(.x, mode = "polygon", roostPolygons = roostPolygons, return = "sri", latCol = "location_lat", longCol = "location_long", idCol = "Nili_id", dateCol = "roost_date"))
# NOTE: using roosts_seasons, not roosts_seasons_mode10, for the roost network. So we end up including a lot more individuals in that network. 
# #
save(flightSeasons_mode10, file = "data/derived/flightSeasons_mode10.Rda")
save(flightSeasons_mode10_edges, file = "data/derived/flightSeasons_mode10_edges.Rda")
save(feedingSeasons_mode10, file = "data/derived/feedingSeasons_mode10.Rda")
save(feedingSeasons_mode10_edges, file = "data/derived/feedingSeasons_mode10_edges.Rda")
save(roostSeasons, file = "data/derived/roostSeasons.Rda")

load("data/derived/flightSeasons_mode10.Rda")
load("data/derived/feedingSeasons_mode10.Rda")
load("data/derived/roostSeasons.Rda")

flightSeasons_mode10_g <- map(flightSeasons_mode10, ~vultureUtils::makeGraph(mode = "sri", data = .x, weighted = T))
feedingSeasons_mode10_g <- map(feedingSeasons_mode10,  ~vultureUtils::makeGraph(mode = "sri", data = .x, weighted = T))
roostSeasons_g <- map(roostSeasons,  ~vultureUtils::makeGraph(mode = "sri", data = .x, weighted = T))

# Describing the networks -------------------------------------------------
# Network-level measures
fldens <- map(flightSeasons_mode10_g, igraph::edge_density) %>% unlist()
fedens <- map(feedingSeasons_mode10_g, igraph::edge_density) %>% unlist()
rodens <- map(roostSeasons_g, igraph::edge_density) %>% unlist()
dens <- data.frame(season = factor(seasonNames, levels = seasonNames), flight = fldens, feeding = fedens, roosting = rodens) %>%
  pivot_longer(cols = c("flight", "feeding", "roosting"), names_to = "type", values_to = "density")

dens %>%
  ggplot(aes(x = season, y = density, col = type, group = type))+
  geom_point()+
  geom_line()

# Extracting individual-level network measures ----------------------------
networkMetrics <- map2_dfr(flightSeasons_mode10_g, seasonNames, ~{
  df <- data.frame(degree = igraph::degree(.x),
                   strength = igraph::strength(.x),
                   pageRank = igraph::page_rank(.x)$vector,
                   evenness = evenness(.x),
                   Nili_id = names(degree(.x))) %>%
    bind_cols(season = .y,
              type = "flight")
  
  return(df)
}) %>%
  bind_rows(map2_dfr(feedingSeasons_mode10_g, seasonNames, ~{
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
  mutate(n = length(unique(Nili_id))) %>% # CAREFUL HERE! Was getting the wrong degree bc conflating number of rows with number of individuals.
  ungroup() %>%
  mutate(degreeRelative = degree/n,
         strengthRelative = strength/n,
         pageRankRelative = pageRank/n,
         sbd = strength/degree,
         evenness = evenness) %>%
  dplyr::select(season, type, n, Nili_id, degree, degreeRelative, strength, strengthRelative, sbd, pageRank, pageRankRelative, evenness)

# Save network metrics ----------------------------------------------------
# Assign datasets
datasetAssignments <- map2(datasetAssignments, seasonNames, ~.x %>% mutate(seasonUnique = .y)) %>% purrr::list_rbind()
networkMetrics <- left_join(networkMetrics, datasetAssignments, by = c("Nili_id", "season" = "seasonUnique"))
save(networkMetrics, file = "data/derived/networkMetrics.Rda")

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