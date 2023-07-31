# Script for creating maps showing individuals' trajectories on either end of the principal component spectrum.
load("data/linked.Rda")
load("data/seasons_10min.Rda")
seasonNames <- map_chr(seasons_10min, ~as.character(.x$seasonUnique[1]))

# Visualize PC extremes ---------------------------------------------------
# Only want to include individuals that participated in all three
forplots <- linked %>%
  group_by(Nili_id, season, year) %>%
  filter(length(unique(type)) == 3) %>%
  ungroup()

forplots %>%
  ggplot(aes(x = PC1, y = PC2))+
  geom_point()+
  geom_vline(aes(xintercept = 0))+
  geom_hline(aes(yintercept = 0))+
  theme_bw()

forplots %>%
  filter(PC1 < -4, PC2 > -0.5, PC2 < 0.5) %>%
  select(season, year, Nili_id, PC1, PC2) %>%
  distinct() # let's use castor fall 2022

forplots %>%
  filter(PC1 > 4.5, PC2 > -0.5, PC2 < 0.5) %>%
  select(season, year, Nili_id, PC1, PC2) %>%
  distinct() # let's use cale fall 2022

names(seasons_10min) <- seasonNames
minPC1Data <- seasons_10min[["2022_fall"]] %>%
  filter(Nili_id == "castor")
maxPC1Data <- seasons_10min[["2022_fall"]] %>%
  filter(Nili_id == "cale")

forplots %>%
  filter(PC2 < -3.5, PC1 > -1, PC1 < 1) %>%
  select(season, year, Nili_id, PC1, PC2) %>%
  distinct() # hippocrates breeding 2023

forplots %>%
  filter(PC2 > 3, PC1 > -1, PC1 < 1) %>%
  select(season, year, Nili_id, PC1, PC2) %>%
  distinct() # jojo breeding 2022

minPC2Data <- seasons_10min[["2023_breeding"]] %>%
  filter(Nili_id == "hippocrates")
maxPC2Data <- seasons_10min[["2022_breeding"]] %>%
  filter(Nili_id == "jojo")

# 
# minPC1 <- forplots %>% filter(PC1 == min(PC1, na.rm = T)) %>%
#   dplyr::select(Nili_id, year, bnb) %>%
#   distinct() %>%
#   mutate(seasonName = paste(year, bnb, sep ="_"))
# maxPC1 <- forplots %>% filter(PC1 == max(PC1, na.rm = T)) %>%
#   dplyr::select(Nili_id, year, bnb) %>%
#   distinct() %>%
#   mutate(seasonName = paste(year, bnb, sep ="_"))
# minPC2 <- forplots %>% filter(PC2 == min(PC2, na.rm = T)) %>%
#   dplyr::select(Nili_id, year, bnb) %>%
#   distinct() %>%
#   mutate(seasonName = paste(year, bnb, sep ="_"))
# maxPC2 <- forplots %>% filter(PC2 == max(PC2, na.rm = T)) %>%
#   dplyr::select(Nili_id, year, bnb) %>%
#   distinct() %>%
#   mutate(seasonName = paste(year, bnb, sep ="_"))

minmaxData <- bind_rows(minPC1Data %>% mutate(minmax = "min", pc = 1),
                        maxPC1Data %>% mutate(minmax = "max", pc = 1),
                        minPC2Data %>% mutate(minmax = "min", pc = 2),
                        maxPC2Data %>% mutate(minmax = "max", pc = 2)) %>%
  mutate(id = paste(minmax, pc)) %>%
  mutate(minmax = factor(minmax, levels = c("min", "max")))
pc1 <- minmaxData %>% filter(pc == 1)
pc2 <- minmaxData %>% filter(pc == 2)

# PC1 map--points and lines
pc1plot <- ggplot(data = pc1) +
  ggspatial::annotation_map_tile("cartolight", zoom = 9) + # this works well; only problem is that it takes a little while to fetch the tiles.
  geom_sf(aes(col = minmax), size = 1) +
  ggspatial::annotation_scale()+
  geom_path(data = pc1, 
            aes(x = location_long, y = location_lat, col = minmax),
            size = 0.5, alpha = 0.5)+
  scale_color_manual(values = c("#E3809C", "#C70039"))+
  theme(legend.position = "none",
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        panel.background = element_rect(fill = "#FFFCF6"),
        plot.background = element_rect(fill = "#FFFCF6"))+
  ylab("") + xlab("")+
  facet_wrap(~minmax)
pc1plot

# PC2 map--points and lines
pc2plot <- ggplot(data = pc2) +
  ggspatial::annotation_map_tile("cartolight", zoom = 9) + # this works well; only problem is that it takes a little while to fetch the tiles.
  geom_sf(aes(col = minmax), size = 1) +
  ggspatial::annotation_scale()+
  geom_path(data = pc2, 
            aes(x = location_long, y = location_lat, col = minmax),
            size = 0.5, alpha = 0.5)+
  scale_color_manual(name = "PC2", values = c("#C1A5FC", "#6E2EF9"))+
  theme(legend.position = "none",
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        panel.background = element_rect(fill = "#FFFCF6"),
        plot.background = element_rect(fill = "#FFFCF6"))+
  ylab("") + xlab("")+
  facet_wrap(~minmax)
pc2plot

mapview(pc1, zcol = "minmax", lwd = 0, cex = 2, alpha = 0.5, col.regions = c("red", "blue"))

mapview(pc2, zcol = "minmax", lwd = 0, cex = 2, alpha = 0.5, col.regions = c("red", "blue"))