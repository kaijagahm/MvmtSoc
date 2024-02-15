# Exploring repeatability
library(tidyverse)
library(vultureUtils)
library(igraph)
library(sf)
library(tidygraph)
library(ggraph)
library(zoo)

# Repeatability across seasons --------------------------------------------
load("data/mixedModels/linked.Rda")

## Just using the traits we've already calculated and plotting across seasons, and coloring by the average value for each individual across all of its seasons, let's see if we get gradients

plotRepeat <- function(df, var, situation = "flight", ylab){
  legend_title <- paste0("Mean\n", var, "\nvalue")
  plt <- df %>%
    filter(type == situation) %>%
    group_by(Nili_id) %>%
    mutate(avg = mean(.data[[var]], na.rm = T)) %>%
    ggplot(aes(x = seasonUnique, y = get(var), group = Nili_id, color = avg))+
    geom_point()+
    geom_line(alpha = 0.7)+
    theme_classic()+
    scale_color_viridis_c(name = legend_title)+
    xlab("Season")+
    ylab(ylab)+
    theme(axis.title = element_text(size = 16))
  return(plt)
}

# PREDICTORS
## note that for these, the situation doesn't matter (defaults to flight) because the predictors are just repeated over and over for each season
plotRepeat(linked, "movement", ylab = "Movement")
plotRepeat(linked, "space_use", ylab = "Space use (log-transformed)")
plotRepeat(linked, "roost_div", ylab = "Roost diversification")

# SOCIAL METRICS
plotRepeat(linked, "normDegree", situation = "flight", ylab = "Normalized degree (flight)")
plotRepeat(linked, "normDegree", situation = "feeding", ylab = "Normalized degree (feeding)")
plotRepeat(linked, "normDegree", situation = "roosting", ylab = "Normalized degree (roosting)")
plotRepeat(linked, "normStrength", situation = "flight", ylab = "Normalized strength (flight)")
plotRepeat(linked, "normStrength", situation = "feeding", ylab = "Normalized strength (feeding)")
plotRepeat(linked, "normStrength", situation = "roosting", ylab = "Normalized strength (roosting)")

# Just by looking at these colors, I would say that the measures (both predictors and responses) are somewhat repeatable, but not strongly.

# Let's break the networks down into smaller bits -------------------------
## I think I have to actually re-make the edge lists here

############### Data ingest chunk taken from calcSocial.R
load("data/dataPrep/downsampled_10min_forSocial.Rda") # this is the last dataset we produced in the dataPrep script before moving on to the further cleaning for movement, so this is the one we're going to use for the social interactions.
sfdata <- map(downsampled_10min_forSocial, ~st_as_sf(.x, coords = c("location_long", "location_lat"), crs = "WGS84", remove = F))
datasetAssignments <- map(downsampled_10min_forSocial, ~.x %>% select(Nili_id, dataset, seasonUnique) %>% distinct() %>% group_by(Nili_id, seasonUnique) %>%
                            mutate(dataset = case_when(n() > 1 ~ "both",
                                                       TRUE ~ dataset)) %>%
                            ungroup() %>%
                            distinct())
save(datasetAssignments, file = "data/calcSocial/datasetAssignments.Rda")
load("data/calcSocial/datasetAssignments.Rda")
rm(downsampled_10min_forSocial)
gc()
roostPolygons <- sf::st_read("data/raw/roosts50_kde95_cutOffRegion.kml")
load("data/dataPrep/season_names.Rda")
load("data/dataPrep/roosts.Rda") # XXX load
load("data/derived/cc.Rda")
############### \Data ingest chunk taken from calcSocial.R
# Lump the sf data back together and then split it into days
sfdata_lumped <- purrr::list_rbind(sfdata)
sfdata_days <- sfdata_lumped %>%
  group_by(dateOnly) %>%
  group_split() %>%
  map(~sf::st_as_sf(.x, coords = c("location_long", "location_lat"), remove = F, crs = "WGS84"))
save(sfdata_days, file = "data/calcSocial/sfdata_days.Rda")
days <- lubridate::ymd(map_chr(sfdata_days, ~as.character(.x$dateOnly[1])))

future::plan(future::multisession, workers = 15)
tictoc::tic()
flightDays <- suppressWarnings(furrr::future_map(sfdata_days, ~{
  library(sf)
  vultureUtils::getFlightEdges(dataset = .x, roostPolygons = roostPolygons,
                                                distThreshold = 1000, idCol = "Nili_id",
                                                return = "both", getLocs = T)
}, .progress = T, options = furrr_options(seed = NULL)))
tictoc::toc()

future::plan(future::multisession, workers = 15)
tictoc::tic()
feedingDays <- suppressWarnings(furrr::future_map(sfdata_days, ~{
  library(sf)
  vultureUtils::getFeedingEdges(.x, roostPolygons = roostPolygons,
                                distThreshold = 50, idCol = "Nili_id",
                                return = "both", getLocs = T)
}, .progress = T))
tictoc::toc()

roostdata_days <- purrr::list_rbind(roosts) %>%
  group_by(roost_date) %>%
  group_split()

future::plan(future::multisession, workers = 15)
tictoc::tic()
roostDays <- suppressWarnings(furrr::future_map(roostdata_days, ~{
  library(sf)
  vultureUtils::getRoostEdges(.x, mode = "polygon", 
                              roostPolygons = roostPolygons, 
                              return = "sri", 
                              latCol = "location_lat", 
                              longCol = "location_long", 
                              idCol = "Nili_id", 
                              dateCol = "roost_date")
}, .progress = T))
tictoc::toc()
days_r <- lubridate::ymd(map_chr(roostdata_days, ~as.character(.x$roost_date[1])))

flightDays_edges <- map(flightDays, "edges")
feedingDays_edges <- map(feedingDays, "edges")
flightDays_sri <- map(flightDays, "sri")
feedingDays_sri <- map(feedingDays, "sri")
roostDays_sri <- roostDays

save(flightDays, file = "data/calcSocial/flightDays.Rda")
save(feedingDays, file = "data/calcSocial/feedingDays.Rda")
save(flightDays_edges, file = "data/calcSocial/flightDays_edges.Rda")
save(feedingDays_edges, file = "data/calcSocial/feedingDays_edges.Rda")
save(flightDays_sri, file = "data/calcSocial/flightDays_sri.Rda")
save(feedingDays_sri, file = "data/calcSocial/feedingDays_sri.Rda")
save(roostDays_sri, file ="data/calcSocial/roostDays_sri.Rda")

# Let's fix up the ones that have length 0 (this wouldn't have happened for full seasons, which is why I didn't consider it until now)
fix <- function(data){
  unique_indivs <- unique(data$Nili_id)
  sri <- as.data.frame(expand.grid(unique_indivs, unique_indivs)) %>%
    setNames(c("ID1", "ID2")) %>%
    mutate(sri = 0) %>%
    filter(as.character(ID1) < as.character(ID2))
  return(sri)
}
flightDays_sri <- map2(flightDays_sri, sfdata_days, ~{
  if(nrow(.x) > 0){
    out <- .x
  }else{
    out <- fix(data = .y)
  }
  return(out)
})

feedingDays_sri <- map2(feedingDays_sri, sfdata_days, ~{
  if(nrow(.x) > 0){
    out <- .x
  }else{
    out <- fix(data = .y)
  }
  return(out)
})

roostDays_sri <- map2(roostDays_sri, roostdata_days, ~{
  if(nrow(.x) > 0){
    out <- .x
  }else{
    out <- fix(data = .y)
  }
  return(out)
})

future::plan(future::multisession, workers = 10)
flightDays_graphs <- furrr::future_map(flightDays_sri, ~vultureUtils::makeGraph(mode = "sri", data = .x, weighted = T), .progress = T)
save(flightDays_graphs, file = "data/calcSocial/flightDays_graphs.Rda")

future::plan(future::multisession, workers = 10)
feedingDays_graphs <- furrr::future_map(feedingDays_sri, ~vultureUtils::makeGraph(mode = "sri", data = .x, weighted = T), .progress = T)
save(feedingDays_graphs, file = "data/calcSocial/feedingDays_graphs.Rda")

future::plan(future::multisession, workers = 10)
roostDays_graphs <- furrr::future_map(roostDays_sri, ~vultureUtils::makeGraph(mode = "sri", data = .x, weighted = T), .progress = T)
save(roostDays_graphs, file = "data/calcSocial/roostDays_graphs.Rda")

flightDays_metrics <- map2(flightDays_graphs, days, ~{
  if(length(.x) > 0){
    metrics <- data.frame(degree = igraph::degree(.x),
                          strength = igraph::strength(.x),
                          Nili_id = names(igraph::degree(.x)),
                          type = "flight",
                          date = .y)
  }else{
    metrics <- data.frame(degree = NA,
                          strength = NA,
                          Nili_id = NA,
                          type = "")
  }
})
flightDays_metrics_df <- purrr::list_rbind(flightDays_metrics)

feedingDays_metrics <- map2(feedingDays_graphs, days, ~{
  if(length(.x) > 0){
    metrics <- data.frame(degree = igraph::degree(.x),
                          strength = igraph::strength(.x),
                          Nili_id = names(igraph::degree(.x)),
                          type = "feeding",
                          date = .y)
  }else{
    metrics <- data.frame(degree = NA,
                          strength = NA,
                          Nili_id = NA,
                          type = "")
  }
})
feedingDays_metrics_df <- purrr::list_rbind(feedingDays_metrics)

roostDays_metrics <- map2(roostDays_graphs, days_r, ~{
  if(length(.x) > 0){
    metrics <- data.frame(degree = igraph::degree(.x),
                          strength = igraph::strength(.x),
                          Nili_id = names(igraph::degree(.x)),
                          type = "roosting",
                          date = .y)
  }else{
    metrics <- data.frame(degree = NA,
                          strength = NA,
                          Nili_id = NA,
                          type = "")
  }
})
roostDays_metrics_df <- purrr::list_rbind(roostDays_metrics)

days_metrics_df <- bind_rows(flightDays_metrics_df, feedingDays_metrics_df, roostDays_metrics_df)


# Daily graph plots -------------------------------------------------------
tbl_graphs_flight <- map(flightDays_graphs, ~as_tbl_graph(.x) %>%
                    activate(nodes) %>%
                    mutate(degree = igraph::degree(.))) 
tbl_graphs_feeding <- map(feedingDays_graphs, ~as_tbl_graph(.x) %>%
                            activate(nodes) %>%
                            mutate(degree = igraph::degree(.))) 
tbl_graphs_roosting <- map(roostDays_graphs, ~as_tbl_graph(.x) %>%
                            activate(nodes) %>%
                            mutate(degree = igraph::degree(.))) 

future::plan(future::multisession, workers = 10)
furrr::future_map2(tbl_graphs_flight, days, ~{
  title <- paste(.y, "flight")
  g <- ggraph(.x, layout = "fr")+
    geom_edge_link(alpha = 0.3)+
    geom_node_point(aes(col = degree, size = degree))+
    geom_node_text(aes(label = name), nudge_y = 0.3, nudge_x = 0.2)+
    scale_color_viridis()+
    theme_void()+
    ggtitle(title)
  ggsave(g, filename = paste0("fig/networkGraphs/daily/flight_", .y, ".png"), width = 6, height = 5)
}, .progress = T)

future::plan(future::multisession, workers = 10)
furrr::future_map2(tbl_graphs_feeding, days, ~{
  title <- paste(.y, "feeding")
  g <- ggraph(.x, layout = "fr")+
    geom_edge_link(alpha = 0.3)+
    geom_node_point(aes(col = degree, size = degree))+
    geom_node_text(aes(label = name), nudge_y = 0.3, nudge_x = 0.2)+
    scale_color_viridis()+
    theme_void()+
    ggtitle(title)
  ggsave(g, filename = paste0("fig/networkGraphs/daily/feeding_", .y, ".png"), width = 6, height = 5)
}, .progress = T)

future::plan(future::multisession, workers = 10)
furrr::future_map2(tbl_graphs_roosting, days_r, ~{
  title <- paste(.y, "roosting")
  g <- ggraph(.x, layout = "fr")+
    geom_edge_link(alpha = 0.3)+
    geom_node_point(aes(col = degree, size = degree))+
    geom_node_text(aes(label = name), nudge_y = 0.3, nudge_x = 0.2)+
    scale_color_viridis()+
    theme_void()+
    ggtitle(title)
  ggsave(g, filename = paste0("fig/networkGraphs/daily/roosting_", .y, ".png"), width = 6, height = 5)
}, .progress = T)
