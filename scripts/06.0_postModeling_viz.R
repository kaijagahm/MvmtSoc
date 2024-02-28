library(tidyverse)
library(sf)
library(mapview)
load("data/dataPrep/downsampled_10min_forSocial.Rda")
roostpolys <- sf::st_read("data/raw/roosts50_kde95_cutOffRegion.kml")
load("data/dataPrep/season_names.Rda")
load("data/dataPrep/roosts.Rda")
load("data/mixedModels/linked.Rda")

# Edge effects ------------------------------------------------------------

# Central roosts ----------------------------------------------------------
roosts <- map2(roosts, season_names, ~.x %>% mutate(seasonUnique = .y))
roosts <- purrr::list_rbind(roosts) %>%
  select(Nili_id, seasonUnique, date, roost_date, location_lat, location_long) %>%
  sf::st_as_sf(coords=c("location_long", "location_lat"), remove = F, crs = "WGS84")

linked_tojoin_deg <- linked %>%
  select(Nili_id, seasonUnique, space_use, movement, roost_div, season, year, type, normDegree, n) %>%
  pivot_wider(id_cols = c("Nili_id", "seasonUnique", "space_use", "movement", "roost_div", "season", "year"), names_from = "type", values_from = "normDegree", names_prefix = "deg_")
linked_tojoin_str <- linked %>%
  select(Nili_id, seasonUnique, space_use, movement, roost_div, season, year, type, normStrength, n) %>%
  pivot_wider(id_cols = c("Nili_id", "seasonUnique", "space_use", "movement", "roost_div", "season", "year"), names_from = "type", values_from = "normStrength", names_prefix = "str_")
linked_tojoin <- left_join(linked_tojoin_deg, linked_tojoin_str)
linked_tojoin %>% group_by(Nili_id, seasonUnique) %>% filter(n()>1) # should be 0 rows, good.

roosts_joined <- roosts %>%
  left_join(linked_tojoin, by = c("Nili_id", "seasonUnique")) %>%
  mutate(seasonUnique = factor(seasonUnique, levels = season_names))

roosts_joined %>%
  ggplot(aes(x = movement, y = location_long, col = season))+
  geom_point(alpha = 0.2)+
  theme_classic()+
  scale_color_manual(values = c(cc$breedingColor, cc$summerColor, cc$fallColor))+
  facet_wrap(~seasonUnique, scales = "free")+
  theme(legend.position = "none")+
  geom_smooth(col = "black", alpha = 0.5)+
  geom_smooth(method = "lm", col = "black") # individuals with higher movement scores tended to roost farther west (higher longitude)

roosts_joined %>%
  ggplot(aes(x = space_use, y = location_long, col = season))+
  geom_point(alpha = 0.2)+
  theme_classic()+
  scale_color_manual(values = c(cc$breedingColor, cc$summerColor, cc$fallColor))+
  facet_wrap(~seasonUnique, scales = "free")+
  theme(legend.position = "none")+
  geom_smooth(col = "black", alpha = 0.5)+
  geom_smooth(method = "lm", col = "black") # individuals with higher space use tended to roost farther west (higher longitude)

roosts_joined %>%
  ggplot(aes(x = roost_div, y = location_long, col = season))+
  geom_point(alpha = 0.2)+
  theme_classic()+
  scale_color_manual(values = c(cc$breedingColor, cc$summerColor, cc$fallColor))+
  facet_wrap(~seasonUnique, scales = "free")+
  theme(legend.position = "none")+
  geom_smooth(col = "black", alpha = 0.5)+
  geom_smooth(method = "lm", col = "black") # individuals with higher roost diversification tended to roost farther west (higher longitude)--this does indeed hint at the idea that maybe they are roosting with untagged birds.

# Individuals that moved more, had higher roost diversification scores, and used more space also tended to roost farther west, which supports the idea that they could be at the edge of the home area and therefore interacting with untagged individuals.

# Movement ----------------------------------------------------------------
gps <- map(downsampled_10min_forSocial, ~.x %>% select(Nili_id, seasonUnique, timestamp, location_long, location_lat)) %>% purrr::list_rbind() %>% sf::st_as_sf(coords = c("location_long", "location_lat"), crs = "WGS84", remove = F)

gps_joined <- gps %>%
  left_join(linked_tojoin, by = c("Nili_id", "seasonUnique"))

# Get centroids per season
centr <- gps_joined %>%
  group_by(Nili_id, seasonUnique) %>%
  summarize(geometry = st_union(geometry)) %>%
  st_centroid()

centr_joined <- left_join(centr, linked_tojoin, by = c("Nili_id", "seasonUnique"))
centr_joined <- bind_cols(centr_joined, as.data.frame(st_coordinates(centr_joined)))

centr_joined %>%
  ggplot(aes(x = X, y = space_use))+
  geom_point(alpha = 0.3)+
  geom_smooth()+
  facet_wrap(~seasonUnique, scales = "free")

head(centr_joined)

toview <- centr_joined %>% filter(!is.na(space_use), space_use < 4)
mapview(toview, zcol = "space_use")

centr_joined %>%
  filter(!is.na(space_use)) %>%
  ggplot()+
  geom_sf(aes(col = space_use))+
  scale_color_viridis_c()

centr_joined %>%
  ggplot(aes(x = X, y = movement))+
  geom_point(alpha = 0.3)+
  geom_smooth()+
  facet_wrap(~seasonUnique, scales = "free")

centr_joined %>%
  ggplot(aes(x = X, y = roost_div))+
  geom_point(alpha = 0.3)+
  geom_smooth()+
  facet_wrap(~seasonUnique, scales = "free")

centr_joined %>%
  select(-contains("str")) %>%
  pivot_longer(cols = c("deg_feeding", "deg_roosting", "deg_flight"), names_to = "situation", values_to = "normDegree") %>%
  mutate(situation = str_remove(situation, "deg_")) %>%
  ggplot(aes(x = X, y = normDegree, col = situation))+
  facet_wrap(~seasonUnique, scales = "free")+
  geom_point(alpha = 0.2)+
  geom_smooth()+
  scale_color_manual(values = c(cc$feedingColor, cc$flightColor, cc$roostingColor))

centr_joined %>%
  select(-contains("deg")) %>%
  pivot_longer(cols = c("str_feeding", "str_roosting", "str_flight"), names_to = "situation", values_to = "normStrength") %>%
  mutate(situation = str_remove(situation, "str_")) %>%
  ggplot(aes(x = X, y = normStrength, col = situation))+
  facet_wrap(~seasonUnique, scales = "free")+
  geom_point(alpha = 0.2)+
  geom_smooth()+
  scale_color_manual(values = c(cc$feedingColor, cc$flightColor, cc$roostingColor))

# Map ---------------------------------------------------------------------


