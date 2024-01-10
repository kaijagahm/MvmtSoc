# Setup for temporal multilayer network analysis
library(tidyverse)
library(lubridate)
library(tnet)
library(sna)

load("data/derived/flightSeasons_mode10_edges.Rda")
fs <- flightSeasons_mode10_edges
testseason <- fs[[5]] # just doing this for one arbitrary season

# Make a binary edgelist
bin <- testseason %>%
  mutate(day = lubridate::date(minTimestamp)) %>%
  mutate(hour = floor_date(minTimestamp, unit = "hours")) %>%
  select(ID1, ID2, day, hour) %>%
  distinct() %>%
  mutate(weight = 1)

# Plot associations
bin %>%
  group_by(hour) %>% 
  summarize(n = n()) %>%
  ggplot(aes(x = hour, y = n))+
  geom_point()+
  geom_path(alpha = 0.5)+
  geom_smooth(method = "lm") # generally more in-flight interactions over the course of the season, which makes sense because it's warming up.

opp_edges <- bin
names(opp_edges)[1:2] <- c("ID2", "ID1")

all_bin <- bind_rows(bin, opp_edges) # attach 

# Add layer numbers (days) to make the next step easier
all_bin <- all_bin %>%
  select(-day) %>%
  arrange(hour, ID1, ID2) %>%
  mutate(layer = as.integer(as.factor(hour))) %>%
  select(-hour) %>%
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
inters_reduce = GetMultilayerReducibility(
    as(inters_SAM, "matrix"), # David Fisher's hack to make this run without errors
    Layers = length(unique(all_edges$layer1)),
    Nodes = length(unique(all_edges$ID1)),
    Method = "single",
    Type = "Categorical") # we get debug messages but the code does run.

gq <- inters_reduce$gQualityFunction

df <- data.frame("Amount of aggregation" = 1:length(gq),
                 "Difference in relative entropy" = gq)
df %>%
  ggplot(aes(x = Amount.of.aggregation, y = Difference.in.relative.entropy))+
  geom_point()+
  geom_line()
