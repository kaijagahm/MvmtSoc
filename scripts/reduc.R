# Load libraries
library(tidyverse)
library(lubridate)
library(tnet)
library(sna)

# Data ingest (already sliced into appropriate chunks)
load("data/calcSocial/feedingEdges.Rda")
feedingEdges <- purrr::list_rbind(feedingEdges) %>% mutate(date = lubridate::date(minTimestamp))
load("data/calcSocial/flightEdges.Rda")
flightEdges <- purrr::list_rbind(flightEdges) %>% mutate(date = lubridate::date(minTimestamp))
load("data/calcSocial/roostingEdges.Rda")
roostingEdges <- purrr::list_rbind(roostingEdges)
source("scripts/muxLib DF.R")

cutfun <- function(vec, days){
  min <- min(vec)
  max <- max(vec)
  ngroups <- ceiling(as.numeric(max-min)/days)
  brks <- seq(from = min, by = days, length.out = ngroups + 1)
  cut <- cut(vec, breaks = brks, include.lowest = T, right = F)
}

# Feed into code to do the rest
doReduce <- function(data, groupingCol){
  # Make a binary edgelist, grouping by the group variable
  bin <- data[,c("ID1", "ID2", groupingCol)] %>%
    distinct() %>%
    mutate(weight = 1)
  
  opp_edges <- bin
  names(opp_edges)[1:2] <- c("ID2", "ID1")
  
  all_bin <- bind_rows(bin, opp_edges) # attach 
  
  # Add layer numbers (days) to make the next step easier
  all_bin <- all_bin %>%
    arrange(.data[[groupingCol]], ID1, ID2) %>%
    mutate(layer = as.numeric(.data[[groupingCol]])) %>%
    select(-all_of(groupingCol)) %>%
    rename("layer1" = "layer") %>%
    mutate(layer2 = layer1) %>%
    relocate(layer1, .after = "ID1") %>%
    relocate(layer2, .after = "ID2")
  
  self_edges <- expand.grid(as.character(unique(all_bin$ID1)), 1:(max(all_bin$layer1)-1)) # create all possible combinations of indiv ID and layer
  colnames(self_edges) <- c("ID1", "layer1")
  
  self_edges <- self_edges %>%
    mutate(ID2 = ID1, layer2 = layer1 + 1, weight = 1)
  self_edges2 <- self_edges %>%
    mutate(layer1 = layer1 + 1, layer2 = layer2-1)
  self_edges_all <- bind_rows(self_edges, self_edges2)
  
  # Combine with symmetrical edgelist
  all_edges <- bind_rows(all_bin, self_edges)
  
  # If we're staying in R, muxViz needs each edgelist to number the vultures from 1 to the number of vultures
  
  all_edges <- all_edges %>%
    mutate(ID1 = as.integer(factor(ID1)),
           ID2 = as.integer(factor(ID2)))
  
  # Save
  save(all_edges, file = "data/derived/multilayer/all_edges.Rda")
  write.table(all_edges, file = "data/derived/multilayer/all_edges.txt",
              row.names = FALSE, col.names = FALSE)
  
  # Create a file describing layers, required by MuxViz
  write.table(rbind(c("layerID", "layerLabel"),
                    cbind(1:length(unique(all_edges$layer1)),
                          1:length(unique(all_edges$layer1)))),
              file = "data/derived/multilayer/layerlist.txt",
              row.names = FALSE, col.names = FALSE)
  
  # Same for a file describing the nodes
  write.table(rbind(c("nodeID", "nodelLabel"),
                    cbind(1:length(unique(all_edges$ID1)),
                          1:length(unique(all_edges$ID1)))),
              file = "data/derived/multilayer/nodelist.txt",
              row.names = FALSE, col.names = FALSE)
  
  # Write a config file
  write.table(paste0("data/derived/multilayer/edgelist2.txt",
                     ";data/derived/multilayer/layerlist.txt",
                     ";data/derived/multilayer/nodelist.txt"),
              file = "data/derived/multilayer/config.txt",
              quote = F, row.names = F, col.names = F)
  
  # Next file ---------------------------------------------------------------
  load("data/derived/multilayer/all_edges.Rda")
  source("scripts/muxLib DF.R")
  build_supra_AM <- function(edgelist){
    BuildSupraAdjacencyMatrixFromExtendedEdgelist(
      mEdges = as.data.frame(edgelist[,1:5]),
      Layers = length(unique(edgelist$layer1)),
      Nodes = length(unique(edgelist$ID1)), isDirected=F)
  }
  
  inters_SAM <- build_supra_AM(all_edges)
  
  # Temporal reducibility
  #layers <- length(unique(all_edges$layer1))
  nodes <- length(unique(all_edges$ID1))
  sam <- as(inters_SAM, "matrix")
  inters_reduce = GetMultilayerReducibility(
    SupraAdjacencyMatrix = sam, # David Fisher's hack to make this run without errors
    Layers = length(unique(all_edges$layer1)),
    Nodes = nodes,
    Method = "single",
    Type = "Categorical") # we get debug messages but the code does run.
  
  gq <- inters_reduce$gQualityFunction
  
  df <- data.frame("Amount of aggregation" = 1:length(gq),
                   "Difference in relative entropy" = gq)
  return(df)
}

flightEdges <- flightEdges %>% 
  mutate(group_01 = cutfun(date, days = 1),
         group_02 = cutfun(date, days = 2),
         group_05 = cutfun(date, days = 5),
         group_10 = cutfun(date, days = 10),
         group_20 = cutfun(date, days = 20),
         group_40 = cutfun(date, days = 40))

feedingEdges <- feedingEdges %>% 
  mutate(group_01 = cutfun(date, days = 1),
         group_02 = cutfun(date, days = 2),
         group_05 = cutfun(date, days = 5),
         group_10 = cutfun(date, days = 10),
         group_20 = cutfun(date, days = 20),
         group_40 = cutfun(date, days = 40))

roostingEdges <- roostingEdges %>% 
  mutate(group_01 = cutfun(date, days = 1),
         group_02 = cutfun(date, days = 2),
         group_05 = cutfun(date, days = 5),
         group_10 = cutfun(date, days = 10),
         group_20 = cutfun(date, days = 20),
         group_40 = cutfun(date, days = 40))

groupcols <- paste("group", c("02", "05", "10", "20", "40"), sep = "_")

outs_flight <- vector(mode = "list", length = length(groupcols))
outs_feeding <- vector(mode = "list", length = length(groupcols))
outs_roosting <- vector(mode = "list", length = length(groupcols))

for(i in 2:length(groupcols)){
  cat("doing ", i, "\n")
  cat("doing flight\n")
  outs_flight[[i]] <- doReduce(flightEdges, groupingCol = groupcols[i])
}


outs_flight <- map(flightEdges, ~doReduce(.x, groupingCol = "group"))
outs_flight <- map2(outs_flight, season_names, ~.x %>% mutate(season = .y)) %>% purrr::list_rbind()
