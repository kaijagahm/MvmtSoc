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
  cat("building supra am\n")
  build_supra_AM <- function(edgelist){
    BuildSupraAdjacencyMatrixFromExtendedEdgelist(
      mEdges = as.data.frame(edgelist[,1:5]),
      Layers = length(unique(edgelist$layer1)),
      Nodes = length(unique(edgelist$ID1)), isDirected=F)
  }
  cat("built supra am\n")
  
  inters_SAM <- build_supra_AM(all_edges)
  
  # Temporal reducibility
  #layers <- length(unique(all_edges$layer1))
  nodes <- length(unique(all_edges$ID1))
  sam <- as(inters_SAM, "matrix")
  cat("reducing\n")
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
  cat("done!\n")
}

flightEdges <- flightEdges %>% 
  mutate(timewindow_01 = cutfun(date, days = 1),
         timewindow_02 = cutfun(date, days = 2),
         timewindow_05 = cutfun(date, days = 5),
         timewindow_10 = cutfun(date, days = 10),
         timewindow_20 = cutfun(date, days = 20),
         timewindow_40 = cutfun(date, days = 40))

feedingEdges <- feedingEdges %>% 
  mutate(timewindow_01 = cutfun(date, days = 1),
         timewindow_02 = cutfun(date, days = 2),
         timewindow_05 = cutfun(date, days = 5),
         timewindow_10 = cutfun(date, days = 10),
         timewindow_20 = cutfun(date, days = 20),
         timewindow_40 = cutfun(date, days = 40))

roostingEdges <- roostingEdges %>% 
  mutate(timewindow_01 = cutfun(date, days = 1),
         timewindow_02 = cutfun(date, days = 2),
         timewindow_05 = cutfun(date, days = 5),
         timewindow_10 = cutfun(date, days = 10),
         timewindow_20 = cutfun(date, days = 20),
         timewindow_40 = cutfun(date, days = 40))

groupcols <- paste("timewindow", c("02", "05", "10", "20", "40"), sep = "_")

# To make this more tractable, let's just take the first few days
mindate <- min(flightEdges$date)
stopdate <- min(flightEdges$date) + 80

flightEdges_test <- flightEdges %>% filter(date >= mindate, date <= stopdate)
feedingEdges_test <- feedingEdges %>% filter(date >= mindate, date <= stopdate)
roostingEdges_test <- roostingEdges %>% filter(date >= mindate, date <= stopdate)

outs_flight <- vector(mode = "list", length = length(groupcols))
outs_feeding <- vector(mode = "list", length = length(groupcols))
outs_roosting <- vector(mode = "list", length = length(groupcols))

future::plan(future::multisession, workers = 10)
outs_flight <- furrr::future_map(groupcols, ~{
  cat("doing", .x, "\n")
  cat("doing flight\n")
  tryCatch(
    expr = {
      doReduce(flightEdges_test, groupingCol = .x)
    },
    error = function(e){ 
      return(data.frame("Amount.of.aggregation" = NA,
                        "Difference.in.relative.entropy" = NA))
    }
  )
}, .progress = T)
outs_flight <- map2(outs_flight, groupcols, ~.x %>% mutate(group = .y))
out_flight <- purrr::list_rbind(outs_flight) %>% mutate(type = "flight")

outs_feeding <- furrr::future_map(groupcols, ~{
  cat("doing", .x, "\n")
  cat("doing feeding\n")
  tryCatch(
    expr = {
      doReduce(feedingEdges_test, groupingCol = .x)
    },
    error = function(e){ 
      return(data.frame("Amount.of.aggregation" = NA,
                        "Difference.in.relative.entropy" = NA))
    }
  )
}, .progress = T)
outs_feeding <- map2(outs_feeding, groupcols, ~.x %>% mutate(group = .y))
out_feeding <- purrr::list_rbind(outs_feeding) %>% mutate(type = "feeding")

outs_roosting <- furrr::future_map(groupcols, ~{
  cat("doing", .x, "\n")
  cat("doing roosting\n")
  tryCatch(
    expr = {
      doReduce(roostingEdges_test, groupingCol = .x)
    },
    error = function(e){ 
      return(data.frame("Amount.of.aggregation" = NA,
                        "Difference.in.relative.entropy" = NA))
    }
  )
}, .progress = T)

outs_roosting <- map2(outs_roosting, groupcols, ~.x %>% mutate(group = .y))
out_roosting <- purrr::list_rbind(outs_roosting) %>% mutate(type = "roosting")

out <- bind_rows(out_flight, out_feeding, out_roosting)

out %>%
  ggplot(aes(x = Amount.of.aggregation, y = Difference.in.relative.entropy, col = type))+
  geom_point()+geom_line()+
  facet_wrap(~group, scales = "free", ncol = 1)+
  theme_minimal()+
  scale_color_manual(name = "Social\nsituation",
                     values = c(cc$feedingColor, cc$flightColor, cc$roostingColor))
