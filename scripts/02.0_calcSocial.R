# Setup ---------------------------------------------------------------
## Packages ---------------------------------------------------------------
library(vultureUtils)
library(igraph)
library(tidyverse)
library(sf)

## Data ---------------------------------------------------------------
load("data/dataPrep/downsampled_10min_forSocial.Rda") # this is the last dataset we produced in the dataPrep script before moving on to the further cleaning for movement, so this is the one we're going to use for the social interactions.
sfdata <- map(downsampled_10min_forSocial, ~st_as_sf(.x, coords = c("location_long", "location_lat"), crs = "WGS84", remove = F))
roostPolygons <- sf::st_read("data/raw/roosts50_kde95_cutOffRegion.kml")
rm(downsampled_10min_forSocial)
gc()
load("data/dataPrep/season_names.Rda")
load("data/dataPrep/roosts.Rda") # XXX load

# Investigate the data ----------------------------------------------------
# How many individuals do we have with vs. without excluding those with a lower sampling rate?
all <- map_dbl(sfdata, ~length(unique(.x$Nili_id)))
nindivs <- map(sfdata, ~.x %>% sf::st_drop_geometry() %>% select(Nili_id, dataset) %>% distinct() %>% group_by(dataset) %>% summarize(n = length(unique(Nili_id)))) %>% map2(.x = ., .y = season_names, ~.x %>% mutate(seasonUnique = .y)) %>% purrr::list_rbind() 
nindivs # so generally more ornitela than inpa individuals, but by the later seasons we do have double digits of inpa individuals.

# Social Networks ---------------------------------------------------------
flight <- vector(mode = "list", length = length(sfdata))
for(i in 1:length(sfdata)){
  cat("Working on iteration", i, "\n")
  dat <- sfdata[[i]]
  fl <- vultureUtils::getFlightEdges(dat, roostPolygons = roostPolygons,
                                     distThreshold = 1000, idCol = "Nili_id",
                                     return = "both", getLocs = T)
  flight[[i]] <- fl
  rm(fl)
}

flightEdges <- map(flight, "edges")
flightSRI <- map(flight, "sri")
save(flight, file = "data/calcSocial/flight.Rda")
save(flightEdges, file = "data/calcSocial/flightEdges.Rda")
save(flightSRI, file = "data/calcSocial/flightSRI.Rda")
rm(flight)
rm(flightEdges)
rm(flightSRI)
gc()

feeding <- vector(mode = "list", length = length(sfdata))
for(i in 1:length(sfdata)){
  cat("Working on iteration", i, "\n")
  dat <- sfdata[[i]]
  fe <- vultureUtils::getFeedingEdges(dat, roostPolygons = roostPolygons,
                                     distThreshold = 50, idCol = "Nili_id",
                                     return = "both", getLocs = T)
  feeding[[i]] <- fe
  rm(fe)
}

feedingEdges <- map(feeding, "edges")
feedingSRI <- map(feeding, "sri")
save(feeding, file = "data/calcSocial/feeding.Rda")
save(feedingEdges, file = "data/calcSocial/feedingEdges.Rda")
save(feedingSRI, file = "data/calcSocial/feedingSRI.Rda")
rm(feeding)
rm(feedingEdges)
rm(feedingSRI)
gc()

roosting <- map(roosts, ~{
  vultureUtils::getRoostEdges(.x, mode = "polygon", 
                              roostPolygons = roostPolygons, 
                              return = "sri", 
                              latCol = "location_lat", 
                              longCol = "location_long", 
                              idCol = "Nili_id", 
                              dateCol = "roost_date")
  }, .progress = T)

save(roosting, file = "data/calcSocial/roosting.Rda")
rm(roosting)


# XXX START HERE

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