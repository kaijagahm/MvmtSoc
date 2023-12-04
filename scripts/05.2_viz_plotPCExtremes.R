# Script for creating maps showing individuals' trajectories on either end of the principal component spectrum.
library(tidyverse)
library(ggspatial)
library(sf)
library(grid)
load("data/kuds_indivs.Rda")
load("data/hrList_indivs.Rda")

theme_set(theme_classic(base_size = 9))

load("data/linked.Rda")
load("data/seasons_10min.Rda")
seasonNames <- map_chr(seasons_10min, ~as.character(.x$seasonUnique[1]))
names(seasons_10min) <- seasonNames
load("data/derived/cc.Rda")

daysTracked <- map(seasons_10min, ~.x %>% st_drop_geometry() %>% dplyr::select(Nili_id, seasonUnique, daysTracked) %>% distinct()) %>% purrr::list_rbind()

# Visualize PC extremes ---------------------------------------------------
# Only want to include individuals that participated in all three
forplots <- linked %>%
  group_by(Nili_id, season, year) %>%
  filter(length(unique(type)) == 3) %>%
  ungroup() %>%
  filter(!is.na(PC1)) %>%
  mutate(seasonUnique = paste(year, season, sep = "_")) %>%
  left_join(daysTracked, by = c("seasonUnique", "Nili_id"))

# Take a look at the biplot again, this time facetted by season
forplots %>%
  ggplot(aes(x = PC1, y = PC2, col = daysTracked))+
  geom_point(size = 2)+
  geom_vline(aes(xintercept = 0))+
  geom_hline(aes(yintercept = 0))+
  theme_bw()+
  scale_color_viridis_c()+
  facet_wrap(~seasonUnique)

# colored by days tracked
forplots %>%
  ggplot(aes(x = PC1, y = PC2, col = daysTracked))+
  geom_point(size = 2)+
  geom_vline(aes(xintercept = 0))+
  geom_hline(aes(yintercept = 0))+
  theme_bw()+
  scale_color_viridis_c()

# colored by year*season
forplots %>%
  ggplot(aes(x = PC1, y = PC2, col = seasonUnique))+
  geom_point(size = 2)+
  geom_vline(aes(xintercept = 0))+
  geom_hline(aes(yintercept = 0))+
  theme_bw()+
  scale_color_viridis_d()+
  stat_ellipse()

# colored by season only, shapes by year
forplots %>%
  ggplot(aes(x = PC1, y = PC2, col = season))+
  geom_point(size = 2, aes(pch = year))+
  geom_vline(aes(xintercept = 0))+
  geom_hline(aes(yintercept = 0))+
  theme_bw()+
  stat_ellipse()+
  scale_color_manual(values = c(cc[["breedingColor"]], cc[["fallColor"]], cc[["summerColor"]]))

# Automatic plotting: top and bottom 5 for each PC
pc1_top5 <- forplots %>%
  dplyr::select(-c(type, n, degree, degreeRelative, strength, strengthRelative, sbd, pageRank, pageRankRelative, evenness, dataset)) %>%
  distinct() %>%
  arrange(desc(PC1)) %>%
  mutate(group = "pc1top") %>%
  head(5) %>% dplyr::select(Nili_id, seasonUnique, PC1, PC2, daysTracked, group)

pc1_bottom5 <- forplots %>%
  dplyr::select(-c(type, n, degree, degreeRelative, strength, strengthRelative, sbd, pageRank, pageRankRelative, evenness, dataset)) %>%
  distinct() %>%
  arrange(PC1) %>%
  mutate(group = "pc1bottom") %>%
  head(5) %>% dplyr::select(Nili_id, seasonUnique, PC1, PC2, daysTracked, group)

pc2_top5 <- forplots %>%
  dplyr::select(-c(type, n, degree, degreeRelative, strength, strengthRelative, sbd, pageRank, pageRankRelative, evenness, dataset)) %>%
  distinct() %>%
  arrange(desc(PC2)) %>%
  mutate(group = "pc2top") %>%
  head(5) %>% dplyr::select(Nili_id, seasonUnique, PC1, PC2, daysTracked, group)

pc2_bottom5 <- forplots %>%
  dplyr::select(-c(type, n, degree, degreeRelative, strength, strengthRelative, sbd, pageRank, pageRankRelative, evenness, dataset)) %>%
  distinct() %>%
  arrange(PC2) %>%
  mutate(group = "pc2bottom") %>%
  head(5) %>% dplyr::select(Nili_id, seasonUnique, PC1, PC2, daysTracked, group)

pcs <- bind_rows(pc1_top5, pc1_bottom5, pc2_top5, pc2_bottom5)

for(i in 1:nrow(pcs)){
  ind <- pcs$Nili_id[i]
  szn <- pcs$seasonUnique[i]
  pc1 <- pcs$PC1[i]
  pc2 <- pcs$PC2[i]
  dt <- pcs$daysTracked[i]
  grp <- pcs$group[i]
  data <- seasons_10min[[szn]] %>%
    filter(Nili_id == ind)
  grob <- grid::grobTree(textGrob(paste0("PC1 = ", round(pc1, 2), 
                                   "  PC2 = ", round(pc2, 2), "\nDays = ", dt, 
                                   "  Season = ", szn,
                                   "\nID = ", ind), 
                            x = 0.05, y = 0.93, hjust = 0),
                         gp=gpar(col = "blue", fontsize=4))
  plt <- ggplot(data = data) +
    ggspatial::annotation_map_tile("cartolight", zoom = 9) + 
    geom_sf(size = 0.2, alpha = 0.5) +
    ggspatial::annotation_scale(text_cex = 0.4)+
    geom_path(data = data, 
              aes(x = location_long, y = location_lat),
              linewidth = 0.1, alpha = 0.5)+
    theme(legend.position = "none",
          strip.background = element_blank(),
          strip.text.x = element_blank(),
          text = element_text(size = 5)
          #panel.background = element_rect(fill = "#FFFCF6"),
          #plot.background = element_rect(fill = "#FFFCF6")
    )+
    ylab("") + xlab("")+ annotation_custom(grob)
  filename <- paste0("fig/pcExtremes/", paste(grp, ind, szn, round(pc1, 2), round(pc2, 2), dt, sep = "_"), ".png")
  ggsave(plt, filename = filename, width = 3, height = 2.5)
}

# Manual plotting of extremes

# PC1 low
forplots %>%
  filter(PC1 >-5 & PC1 < -3, PC2 < 0 & PC2 > -0.5) %>%
  dplyr::select(season, year, Nili_id, PC1, PC2, daysTracked) %>%
  distinct() # let's use castor fall 2022

# PC1 high
forplots %>%
  filter(PC1 > 4.75 & PC1 < 5, PC2 > 0 & PC2 < 0.5) %>%
  dplyr::select(season, year, Nili_id, PC1, PC2, daysTracked) %>%
  distinct() # let's use cale fall 2022

names(seasons_10min) <- seasonNames
lowPC1Data <- seasons_10min[["2022_fall"]] %>%
  filter(Nili_id == "castor")
highPC1Data <- seasons_10min[["2022_fall"]] %>%
  filter(Nili_id == "cale")

# PC2 low
forplots %>%
  filter(PC2 < -2.5 & PC2 > -3, PC1 < 2) %>%
  dplyr::select(season, year, Nili_id, PC1, PC2, daysTracked) %>%
  distinct() # circus breeding 2023

# PC2 high
forplots %>%
  filter(PC2 > 3, PC1 > -1, PC1 < 1) %>%
  dplyr::select(season, year, Nili_id, PC1, PC2, daysTracked) %>%
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
  dplyr::select(Nili_id, trackId, season, year, seasonUnique, timestamp, dateOnly, location_lat, location_long, age_group, level, id, pc) %>%
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


# Home range visualization ------------------------------------------------
indivs <- map(hrList_indivs, ~map_chr(.x, ~.x$Nili_id[1]))

# get home ranges and core areas
# hrs <- map(kuds_indivs, ~.x %>% map(., ~getverticeshr(.x, percent = 95)))
# cas <- map(kuds_indivs, ~.x %>% map(., ~getverticeshr(.x, percent = 50)))
# save(hrs, file = "data/hrs.Rda")
# save(cas, file = "data/cas.Rda")
load("data/hrs.Rda")
load("data/cas.Rda")

# transform to sf objects
hrs_sf <- map(hrs, ~do.call(rbind, .x)) %>% map(., ~sf::st_as_sf(.x))
cas_sf <- map(cas, ~do.call(rbind, .x)) %>% map(., ~sf::st_as_sf(.x))

# add back the individual IDs
hrs_sf <- map2(hrs_sf, indivs, ~.x %>% mutate(Nili_id = .y))
cas_sf <- map2(cas_sf, indivs, ~.x %>% mutate(Nili_id = .y))
hrs_sf <- map2(hrs_sf, seasonNames, ~.x %>% mutate(seasonUnique = .y,
                                                       year = str_extract(seasonUnique, "[0-9]{4}"),
                                                       season = str_extract(seasonUnique, "[a-z]+")))
cas_sf <- map2(cas_sf, seasonNames, ~.x %>% mutate(seasonUnique = .y,
                                                   year = str_extract(seasonUnique, "[0-9]{4}"),
                                                   season = str_extract(seasonUnique, "[a-z]+")))
hrs_sf_df <- do.call(rbind, hrs_sf) %>% sf::st_set_crs(32636)
cas_sf_df <- do.call(rbind, cas_sf) %>% sf::st_set_crs(32636)

# Make some plots
hr_season_facet <- hrs_sf_df %>%
  ggplot()+
  ggspatial::annotation_map_tile("cartolight", zoom = 9) + 
  ggspatial::annotation_scale()+
  geom_sf(aes(fill = Nili_id, col = Nili_id), alpha = 0.05)+
  facet_grid(rows = vars(year), cols = vars(season))+
  theme(legend.position = "none")+
  ggtitle("Home ranges (95% KUD)")
ggsave(hr_season_facet, filename = "fig/hr_season_facet.png", height = 7, width = 5)

ca_season_facet <- cas_sf_df %>%
  ggplot()+
  ggspatial::annotation_map_tile("cartolight", zoom = 9) + 
  ggspatial::annotation_scale()+
  geom_sf(aes(fill = Nili_id, col = Nili_id), alpha = 0.05)+
  facet_grid(rows = vars(year), cols = vars(season))+
  theme(legend.position = "none")+
  ggtitle("Core areas (50% KUD)")
ggsave(ca_season_facet, filename = "fig/ca_season_facet.png", height = 7, width = 5)

hr_season_color <- hrs_sf_df %>%
  ggplot()+
  ggspatial::annotation_map_tile("cartolight", zoom = 9) + 
  ggspatial::annotation_scale()+
  geom_sf(aes(fill = season, col = season), alpha = 0.1)+
  facet_wrap(~year)+
  scale_color_manual(values = c(cc[["breedingColor"]], cc[["fallColor"]], cc[["summerColor"]]))+
  scale_fill_manual(values = c(cc[["breedingColor"]], cc[["fallColor"]], cc[["summerColor"]]))+
  ggtitle("Home ranges (95% KUD)")
ggsave(hr_season_color, filename = "fig/hr_season_color.png", height = 7, width = 5)

ca_season_color <- cas_sf_df %>%
  ggplot()+
  ggspatial::annotation_map_tile("cartolight", zoom = 9) + 
  ggspatial::annotation_scale()+
  geom_sf(aes(fill = season, col = season), alpha = 0.1)+
  facet_wrap(~year)+
  scale_color_manual(values = c(cc[["breedingColor"]], cc[["fallColor"]], cc[["summerColor"]]))+
  scale_fill_manual(values = c(cc[["breedingColor"]], cc[["fallColor"]], cc[["summerColor"]]))+
  ggtitle("Core areas (50% KUD)")
ggsave(ca_season_color, filename = "fig/ca_season_color.png", height = 7, width = 5)

hr_year_color <- hrs_sf_df %>%
  ggplot()+
  ggspatial::annotation_map_tile("cartolight", zoom = 9) + 
  ggspatial::annotation_scale()+
  geom_sf(aes(fill = year, col = year), alpha = 0.1)+
  facet_wrap(~season)+
  ggtitle("Home ranges (95% KUD)")
ggsave(hr_year_color, filename = "fig/hr_year_color.png", height = 5, width = 7)

ca_year_color <- cas_sf_df %>%
  ggplot()+
  ggspatial::annotation_map_tile("cartolight", zoom = 9) + 
  ggspatial::annotation_scale()+
  geom_sf(aes(fill = year, col = year), alpha = 0.1)+
  facet_wrap(~season)+
  ggtitle("Core areas (50% KUD)")
ggsave(ca_year_color, filename = "fig/ca_year_color.png", height = 5, width = 7)

# Plots with movement tracks and home ranges ------------------------------
glimpse(forplots)





# Home ranges and movements together --------------------------------------
szn <- 3
id <- 11
testkud <- kuds_indivs[[szn]][[id]]
hr <- testkud %>% getverticeshr(., percent = 95) %>% st_as_sf(., crs = 32636)
ca <- testkud %>% getverticeshr(., percent = 50) %>% fortify()
testhr <- hrList_indivs[[szn]][[id]]
name <- as.character(testhr[1,1])
testdata <- linked %>%
  mutate(seasonUnique = paste(year, season, sep = "_")) %>%
  filter(Nili_id == name, seasonUnique == seasonNames[szn])

ggplot() +
  ggspatial::annotation_map_tile("cartolight", zoom = 6) + 
  geom_sf(size = 0.2, alpha = 0.5) +
  ggspatial::annotation_scale(text_cex = 0.4)+
  geom_path(data = testhr, aes(x = x, y = y), linewidth = 0.1, alpha = 0.5)+
  geom_point(data = testhr, aes(x = x, y = y), size = 0.5)+
  geom_sf(data = hr)+
  NULL

ggplot(hr) +
  geom_polygon(aes(x=long, y=lat, group = group, fill = id, colour = id),
               alpha = .4) +
  theme_bw() +
  coord_equal()+
  geom_sf(data = testhr)
  
  
  theme(legend.position = "none",
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        text = element_text(size = 5)
  )+
  ylab("") + xlab("")+ annotation_custom(grob)

