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
# datasetAssignments <- map(downsampled_10min_forSocial, ~.x %>% select(Nili_id, dataset, seasonUnique) %>% distinct() %>% group_by(Nili_id, seasonUnique) %>%
#                             mutate(dataset = case_when(n() > 1 ~ "both",
#                                                        TRUE ~ dataset)) %>%
#                             ungroup() %>%
#                             distinct())
# save(datasetAssignments, file = "data/calcSocial/datasetAssignments.Rda")
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

sfdata_lumped <- sfdata_lumped %>%
  select(-c(tag_id, sensor_type_id, acceleration_raw_x, acceleration_raw_y, acceleration_raw_z, barometric_height, battery_charge_percent, battery_charging_current, external_temperature, gps_hdop, gps_satellite_count, gps_time_to_fix, import_marked_outlier, light_level, magnetic_field_raw_x, magnetic_field_raw_y, magnetic_field_raw_z, ornitela_transmission_protocol, tag_voltage, update_ts, visible, deployment_id, event_id, sensor_type, tag_local_identifier, location_long.1, location_lat.1, optional, sensor, earliest_date_born, exact_date_of_birth, group_id, individual_id, latest_date_born, local_identifier, marker_id, mates, mortality_date, mortality_latitude, mortality_type, nick_name, offspring, parents, ring_id, siblings, taxon_canonical_name, taxon_detail, number_of_events, number_of_deployments))

map_dbl(sfdata, ~length(unique(.x$dateOnly))) # lengths of the days. Shortest season is 90 days, so let's go up to 45 day windows.
gc()
# Prepare for cutting -----------------------------------------------------

# It has become evident that I can only use a single season here  --------
# otherwise just way too much memory
testseason <- sfdata[[9]]
ndays <- c(1, 2, 5, 10, 20, 30, 40)
min <- min(testseason$dateOnly)
max <- max(testseason$dateOnly)
breaks <- map(ndays, ~seq(from = as.POSIXct(min), to = as.POSIXct(max), by = paste(.x, "day", sep = " ")))
annotated <- map(breaks, ~testseason %>%
                   mutate(int = cut(dateOnly, breaks = as.Date(.x), include.lowest = T))) #4.8 GB

# XXX I'll have to find a way to include the last few dates in these (i.e. add one more element to "breaks" that's [last one + 1 interval unit]), but for now it's fine to leave them as NA.

split_sfData <- purrr::map(annotated, ~{
  cat("grouping\n")
  lst <- .x %>%
    group_by(int) %>%
    group_split()
  cat("converting to sf\n")
  lst <- map(lst, ~sf::st_as_sf(.x, coords = c("location_long", "location_lat"), remove = F, crs = "WGS84"))
  return(lst)
  cat("cleaning up\n")
  gc()
}, .progress = T)
rm(annotated) # we have to be very careful with space here.
gc()
# 
length(split_sfData) == length(ndays) # should be TRUE
map_dbl(split_sfData, length)
save(split_sfData, file = "data/split_sfData.Rda")
load("data/split_sfData.Rda")
brks <- map(split_sfData, ~map_chr(.x, ~{as.character(.x$int[1])})) # extract the date breaks
gc()

testseason_roosts <- roosts[[9]]
#roostdata_lumped <- purrr::list_rbind(roosts)
annotated_roosts <- map(breaks, ~testseason_roosts %>%
                          mutate(int = cut(roost_date, breaks = as.Date(.x), include.lowest = T)))
split_sfData_roosts <- purrr::map(annotated_roosts, ~{
  lst <- .x %>%
    group_by(int) %>%
    group_split()
  lst <- map(lst, ~sf::st_as_sf(.x, coords = c("location_long", "location_lat"), remove = F, crs = "WGS84"))
  return(lst)
}, .progress = T)
rm(annotated_roosts)
save(split_sfData_roosts, file = "data/split_sfData_roosts.Rda")
load("data/split_sfData_roosts.Rda")
brks_roosts <- map(split_sfData_roosts, ~map_chr(.x, ~{as.character(.x$int[1])})) # extract the date breaks for roosting data
identical(brks, brks_roosts) # TRUE yay!

length(split_sfData_roosts) == length(ndays) # TRUE
map_dbl(split_sfData_roosts, length) # should be very similar to the same for non-roost data, if not identical.

# Okay, now for each of these let's get feeding, flight, and roosting edges
# XXX start here
outs_flight <- vector(mode = "list", length = length(ndays))
outs_feeding <- vector(mode = "list", length = length(ndays))
outs_roosting <- vector(mode = "list", length = length(ndays))
for(split in 1:length(ndays)){
  cat("Working on data split into", ndays[split], "day intervals\n") 
  datalist <- split_sfData[[split]]
  roostlist <- split_sfData_roosts[[split]]
  cat("working on flight\n")
  future::plan(future::multisession, workers = 16)
  fl <- suppressWarnings(furrr::future_map(datalist, ~{
    vultureUtils::getFlightEdges(.x, roostPolygons = roostPolygons,
                                 distThreshold = 1000, idCol = "Nili_id",
                                 return = "sri")
  }, .progress = T))
  
  cat("working on feeding\n")
  fe <- suppressWarnings(furrr::future_map(datalist, ~{
    vultureUtils::getFeedingEdges(.x, roostPolygons = roostPolygons,
                                  distThreshold = 50, idCol = "Nili_id",
                                  return = "sri")
  }, .progress = T))
  
  cat("working on roosting\n")
  ro <- suppressWarnings(furrr::future_map(roostlist, ~{
    vultureUtils::getRoostEdges(.x, mode = "polygon", 
                                roostPolygons = roostPolygons, 
                                return = "sri", 
                                latCol = "location_lat", 
                                longCol = "location_long", 
                                idCol = "Nili_id", 
                                dateCol = "roost_date")
  }, .progress = T))
  outs_flight[[split]] <- fl
  outs_feeding[[split]] <- fe
  outs_roosting[[split]] <- ro
  rm(datalist)
  rm(roostlist)
  rm(fl)
  rm(fe)
  rm(ro)
  gc()
}

save(outs_flight, file = "data/calcSocial/outs_flight.Rda")
save(outs_feeding, file = "data/calcSocial/outs_feeding.Rda")
save(outs_roosting, file = "data/calcSocial/outs_roosting.Rda")

# Need to fix the ones that have length 0
fix <- function(data){
  unique_indivs <- unique(data$Nili_id)
  sri <- as.data.frame(expand.grid(unique_indivs, unique_indivs)) %>%
    setNames(c("ID1", "ID2")) %>%
    mutate(sri = 0) %>%
    filter(as.character(ID1) < as.character(ID2))
  return(sri)
}

flout <- vector(mode = "list", length = length(ndays))
feout <- vector(mode = "list", length = length(ndays))
roout <- vector(mode = "list", length = length(ndays))

for(i in 1:length(ndays)){
  sri_flight <- outs_flight[[i]]
  sri_feeding <- outs_feeding[[i]]
  sri_roosting <- outs_roosting[[i]]
  data_flight <- split_sfData[[i]]
  data_feeding <- split_sfData[[i]]
  data_roosting <- split_sfData_roosts[[i]]
  
  data_flight_fixed <- map2(.x = sri_flight, 
                            .y = data_flight, ~{
                              if(nrow(.x) > 0){
                                out <- .x
                              }else{
                                out <- fix(data = .y)
                              }
                              return(out)
                            })
  
  data_feeding_fixed <- map2(.x = sri_feeding, 
                            .y = data_feeding, ~{
                              if(nrow(.x) > 0){
                                out <- .x
                              }else{
                                out <- fix(data = .y)
                              }
                              return(out)
                            })
  data_roosting_fixed <- map2(.x = sri_roosting, 
                            .y = data_roosting, ~{
                              if(nrow(.x) > 0){
                                out <- .x
                              }else{
                                out <- fix(data = .y)
                              }
                              return(out)
                            })
  flout[[i]] <- data_flight_fixed
  feout[[i]] <- data_feeding_fixed
  roout[[i]] <- data_roosting_fixed
  rm(sri_flight)
  rm(sri_feeding)
  rm(sri_roosting)
  rm(data_flight)
  rm(data_feeding)
  rm(data_roosting)
}

# Now we can make the graphs
graphs_flight <- vector(mode = "list", length = length(ndays))
graphs_feeding <- vector(mode = "list", length = length(ndays))
graphs_roosting <- vector(mode = "list", length = length(ndays))
for(i in 1:length(ndays)){
  flight <- flout[[i]]
  feeding <- feout[[i]]
  roosting <- roout[[i]]
  future::plan(future::multisession, workers = 16)
  flightgraphs <- furrr::future_map(flight, ~vultureUtils::makeGraph(mode = "sri", data = .x, weighted = T),
                                    .progress = T)
  feedinggraphs <- furrr::future_map(feeding, ~vultureUtils::makeGraph(mode = "sri", data = .x, weighted = T),
                                     .progress = T)
  roostinggraphs <- furrr::future_map(roosting, ~vultureUtils::makeGraph(mode = "sri", data = .x, weighted = T), .progress = T)
  graphs_flight[[i]] <- flightgraphs
  graphs_feeding[[i]] <- feedinggraphs
  graphs_roosting[[i]] <- roostinggraphs
  rm(flightgraphs)
  rm(feedinggraphs)
  rm(roostinggraphs)
}

save(graphs_flight, file = "data/graphs_flight.Rda")
save(graphs_feeding, file = "data/graphs_feeding.Rda")
save(graphs_roosting, file = "data/graphs_roosting.Rda")

# Get metrics for each graph
metrics_flight <- vector(mode = "list", length = length(ndays))
metrics_feeding <- vector(mode = "list", length = length(ndays))
metrics_roosting <- vector(mode = "list", length = length(ndays))

for(i in 1:length(ndays)){
  fl <- graphs_flight[[i]]
  fe <- graphs_feeding[[i]]
  ro <- graphs_roosting[[i]]
  
  metrics_flight[[i]] <- map2(fl, brks[[i]], ~{
    if(length(.x) > 0){
      out <- data.frame(degree = igraph::degree(.x),
                        strength = igraph::strength(.x),
                        Nili_id = names(igraph::degree(.x)),
                        int = .y,
                        n = length(V(.x)),
                        type = "flight",
                        ndays = ndays[i])
    }else{
      out <- data.frame(degree = NA, strength = NA, Nili_id = NA, int = .y, 
                        n = 0, type = "flight", ndays = ndays[i])
    }
    return(out)
  })
  metrics_feeding[[i]] <- map2(fe, brks[[i]], ~{
    if(length(.x) > 0){
      out <- data.frame(degree = igraph::degree(.x),
                        strength = igraph::strength(.x),
                        Nili_id = names(igraph::degree(.x)),
                        int = .y,
                        n = length(V(.x)),
                        type = "feeding",
                        ndays = ndays[i])
    }else{
      out <- data.frame(degree = NA, strength = NA, Nili_id = NA, int = .y, 
                        n = 0, type = "feeding", ndays = ndays[i])
    }
    return(out)
  })
  metrics_roosting[[i]] <- map2(ro, brks_roosts[[i]], ~{
    if(length(.x) > 0){
      out <- data.frame(degree = igraph::degree(.x),
                        strength = igraph::strength(.x),
                        Nili_id = names(igraph::degree(.x)),
                        int = .y,
                        n = length(V(.x)),
                        type = "roosting",
                        ndays = ndays[i])
    }else{
      out <- data.frame(degree = NA, strength = NA, Nili_id = NA, int = .y, 
                        n = 0, type = "roosting", ndays = ndays[i])
    }
    return(out)
  })
}
metrics_flight <- map(metrics_flight, ~purrr::list_rbind(.x))
metrics_feeding <- map(metrics_feeding, ~purrr::list_rbind(.x))
metrics_roosting <- map(metrics_roosting, ~purrr::list_rbind(.x))

metrics <- bind_rows(purrr::list_rbind(metrics_flight), 
                     purrr::list_rbind(metrics_feeding), 
                     purrr::list_rbind(metrics_roosting))

### XXX START HERE
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
