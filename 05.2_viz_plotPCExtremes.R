# Script for creating maps showing individuals' trajectories on either end of the principal component spectrum.
library(tidyverse)
library(ggspatial)
library(sf)

load("data/linked.Rda")
load("data/seasons_10min.Rda")
seasonNames <- map_chr(seasons_10min, ~as.character(.x$seasonUnique[1]))

daysTracked <- map(seasons_10min, ~.x %>% st_drop_geometry() %>% select(Nili_id, seasonUnique, daysTracked) %>% distinct()) %>% purrr::list_rbind()

# Visualize PC extremes ---------------------------------------------------
# Only want to include individuals that participated in all three
forplots <- linked %>%
  group_by(Nili_id, season, year) %>%
  filter(length(unique(type)) == 3) %>%
  ungroup() %>%
  filter(!is.na(PC1)) %>%
  mutate(seasonUnique = paste(year, season, sep = "_")) %>%
  left_join(daysTracked, by = c("seasonUnique", "Nili_id"))

# Take a look at the biplot again
forplots %>%
  ggplot(aes(x = PC1, y = PC2, col = daysTracked))+
  geom_point(size = 2)+
  geom_vline(aes(xintercept = 0))+
  geom_hline(aes(yintercept = 0))+
  theme_bw()+
  scale_color_viridis_c()+
  facet_wrap(~seasonUnique)

# PC1 low
forplots %>%
  filter(PC1 >-5 & PC1 < -3, PC2 < 0 & PC2 > -0.5) %>%
  select(season, year, Nili_id, PC1, PC2, daysTracked) %>%
  distinct() # let's use castor fall 2022

# PC1 high
forplots %>%
  filter(PC1 > 4.75 & PC1 < 5, PC2 > 0 & PC2 < 0.5) %>%
  select(season, year, Nili_id, PC1, PC2, daysTracked) %>%
  distinct() # let's use cale fall 2022

names(seasons_10min) <- seasonNames
lowPC1Data <- seasons_10min[["2022_fall"]] %>%
  filter(Nili_id == "castor")
highPC1Data <- seasons_10min[["2022_fall"]] %>%
  filter(Nili_id == "cale")

# PC2 low
forplots %>%
  filter(PC2 < -2.5 & PC2 > -3, PC1 < 2) %>%
  select(season, year, Nili_id, PC1, PC2, daysTracked) %>%
  distinct() # circus breeding 2023

# PC2 high
forplots %>%
  filter(PC2 > 3, PC1 > -1, PC1 < 1) %>%
  select(season, year, Nili_id, PC1, PC2, daysTracked) %>%
  distinct() # hippocrates breeding 2023

lowPC2Data <- seasons_10min[["2023_breeding"]] %>%
  filter(Nili_id == "circus")
highPC2Data <- seasons_10min[["2023_breeding"]] %>%
  filter(Nili_id == "hippocrates")

lowHighData <- bind_rows(lowPC1Data %>% mutate(level = "low", pc = 1),
                        highPC1Data %>% mutate(level = "high", pc = 1),
                        lowPC2Data %>% mutate(level = "low", pc = 2),
                        highPC2Data %>% mutate(level = "high", pc = 2)) %>%
  mutate(id = paste(level, pc)) %>%
  mutate(level = factor(level, levels = c("low", "high"))) %>%
  select(Nili_id, trackId, season, year, seasonUnique, timestamp, dateOnly, location_lat, location_long, age_group, level, id, pc) %>%
  group_by(level, id) %>%
  mutate(nDays = length(unique(dateOnly))) %>%
  ungroup()
pc1 <- lowHighData %>% filter(pc == 1)
pc2 <- lowHighData %>% filter(pc == 2)

# PC1 map--points and lines
pc1plot <- ggplot(data = pc1) +
  ggspatial::annotation_map_tile("cartolight", zoom = 9) + # this works well; only problem is that it takes a little while to fetch the tiles.
  geom_sf(aes(col = level), size = 1) +
  ggspatial::annotation_scale()+
  geom_path(data = pc1, 
            aes(x = location_long, y = location_lat, col = level),
            size = 0.5, alpha = 0.5)+
  scale_color_manual(values = c("#E3809C", "#C70039"))+
  theme(legend.position = "none",
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        panel.background = element_rect(fill = "#FFFCF6"),
        plot.background = element_rect(fill = "#FFFCF6"))+
  ylab("") + xlab("")+
  facet_wrap(~level)
pc1plot

# PC2 map--points and lines
pc2plot <- ggplot(data = pc2) +
  ggspatial::annotation_map_tile("cartolight", zoom = 9) + # this works well; only problem is that it takes a little while to fetch the tiles.
  geom_sf(aes(col = level), size = 1) +
  ggspatial::annotation_scale()+
  geom_path(data = pc2, 
            aes(x = location_long, y = location_lat, col = level),
            size = 0.5, alpha = 0.5)+
  scale_color_manual(name = "PC2", values = c("#C1A5FC", "#6E2EF9"))+
  theme(legend.position = "none",
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        panel.background = element_rect(fill = "#FFFCF6"),
        plot.background = element_rect(fill = "#FFFCF6"))+
  ylab("") + xlab("")+
  facet_wrap(~level)
pc2plot # XXX come back to this--this one looks way different.
