library(tidyverse)
library(jtools)
library(sjPlot)
library(ggplot2)
library(ggeffects)

load("data/mods.Rda")
load("data/effects.Rda")
load("data/contrib.Rda")
load("data/linked.Rda")
load("data/seasons_10min.Rda")

# Set season colors
seasonColors <- c("#2FF8CA", "#CA2FF8", "#F8CA2F")
situationColors <- c("dodgerblue2", "olivedrab3", "gold") # flight, roosting, feeding
flightColor <- "dodgerblue2"
roostingColor <- "olivedrab3"
feedingColor <- "gold"

# Effect size plots
# Here are some plots made with jtools, following the vignette here: https://cran.r-project.org/web/packages/jtools/vignettes/summ.html

# Degree
deg.jt <- plot_summs(mods[["fd"]], mods[["ed"]], mods[["rd"]], inner_ci_level = .9,
           model.names = c("Flight", "Feeding", "Roosting"), coefs = c("PC1", "PC2"))+
  scale_color_manual(values = c("Flight" = flightColor, "Feeding" = feedingColor, "Roosting" = roostingColor))+
  ggtitle("Degree (normalized)")
deg.jt

# Strength
str.jt <- plot_summs(mods[["fs"]], mods[["es"]], mods[["rs"]], inner_ci_level = .9,
           model.names = c("Flight", "Feeding", "Roosting"), coefs = c("PC1", "PC2"))+
  scale_color_manual(values = c("Flight" = flightColor, "Feeding" = feedingColor, "Roosting" = roostingColor))+
  ggtitle("Strength (normalized)")
str.jt

# Evenness
eve.jt <- plot_summs(mods[["fe"]], mods[["ee"]], mods[["re"]], inner_ci_level = .9,
           model.names = c("Flight", "Feeding", "Roosting"), coefs = c("PC1", "PC2"))+
  scale_color_manual(values = c("Flight" = flightColor, "Feeding" = feedingColor, "Roosting" = roostingColor))+
  ggtitle("Evenness")
eve.jt

# For the sake of learning, let's do the same thing with sjPlot
# Degree
deg.sj <- sjPlot::plot_models(mods[["fd"]], mods[["ed"]], mods[["rd"]]) # okay, I can't figure out how to specify terms to include (rather than exclude), and this doesn't integrate as well with ggplot...
sjPlot::plot_models(mods[["fd"]], mods[["ed"]], mods[["rd"]]) + scale_color_manual(values = situationColors) # This also doesn't label the colors in a useful way. I think I'm going to stick with plot_summs.

# Let's look at the full plots, not just PC1 and PC2:
# Degree
deg.jt.full <- plot_summs(mods[["fd"]], mods[["ed"]], mods[["rd"]], inner_ci_level = .9,
                     model.names = c("Flight", "Feeding", "Roosting"))+
  scale_color_manual(values = c("Flight" = flightColor, "Feeding" = feedingColor, "Roosting" = roostingColor))+
  ggtitle("Degree (normalized)")
deg.jt.full # year, age, and season have much larger impacts than PC1 and PC2 do.

# Strength
str.jt.full <- plot_summs(mods[["fs"]], mods[["es"]], mods[["rs"]], inner_ci_level = .9,
                     model.names = c("Flight", "Feeding", "Roosting"))+ 
  scale_color_manual(values = c("Flight" = flightColor, "Feeding" = feedingColor, "Roosting" = roostingColor))+
  ggtitle("Strength (normalized)")
str.jt.full

# Evenness
eve.jt.full <- plot_summs(mods[["fe"]], mods[["ee"]], mods[["re"]], inner_ci_level = .9,
                          omit.coefs = c("(Intercept)", "sd__(Intercept)"),
                     model.names = c("Flight", "Feeding", "Roosting"))+
  scale_color_manual(values = c("Flight" = flightColor, "Feeding" = feedingColor, "Roosting" = roostingColor))+
  ggtitle("Evenness")
eve.jt.full

# Which effects are included in the models?
effectsGeneral <- effects %>%
  select(type, response, term) %>%
  distinct() %>%
  mutate(term = case_when(term == "age_groupadult" ~ "age",
                          term %in% c("seasonsummer", "seasonfall") ~ "season",
                          term %in% c("year2021", "year2022") ~ "year",
                          term %in% c("age_groupadult:PC2", "PC2:age_groupadult") ~ "PC2:age",
                          term == "PC1:age_groupadult" ~ "PC1:age",
                          grepl("season", term) & grepl("year", term) ~ "season:year",
                          TRUE ~ term)) %>%
  distinct() %>%
  filter(!(grepl("sd_", term)), term != "(Intercept)") %>%
  mutate(resp = case_when(response == "degreeRelative" ~ "D",
                          response == "strengthRelative" ~ "S",
                          response == "evenness" ~ "E")) %>%
  mutate(term = factor(term, levels = c("PC1", "PC2", "age", "season", "year", "PC1:age", "PC2:age", "season:year")))

# By response variable:
effectsGeneral %>%
  arrange(resp, type) %>%
  mutate(responseType = paste(resp, type, sep = "_")) %>%
  ggplot(aes(x = term, y = responseType))+
  geom_tile(aes(fill = resp), col = "black")+
  ylab("")+
  xlab("")+
  theme(text = element_text(size = 16), legend.position = "none")

# By situation
effectsGeneral %>%
  arrange(type, resp) %>%
  mutate(typeResponse = paste(type, resp, sep = "_")) %>%
  ggplot(aes(x = term, y = typeResponse))+
  geom_tile(color = "black", aes(fill = type))+
  ylab("")+
  xlab("")+
  scale_fill_manual(values = c(feedingColor, flightColor, roostingColor))+
  theme(text = element_text(size = 16), legend.position = "none")
  

# Variable contribution plots

# plots -------------------------------------------------------------------
# Let's look at the PC contributions
contrib <- contrib %>%
  mutate(var = row.names(.)#,
  #        varName = case_when(var == "meanDMD" ~ "Mn. daily max displacement",
  #                            var == "propSwitch" ~"Prop. nights roost-switching",
  #                            var == "shannon" ~"Roost diversity",
  #                            var == "coreArea" ~"50% KDE",
  #                            var == "coreAreaFidelity" ~"50%/95% KDE",
  #                            var == "homeRange"~"95% KDE",
  #                            var == "meanDDT" ~ "Mn. daily distance traveled",
  #                            var == "uniqueRoosts"~"# unique roosts",
  #                            var == "meanDFD" ~"Mn. daily flight time",
  #                            var == "mnDailyMaxAlt" ~"Mn. daily max. altitude",
  #                            var == "mnDailyMedAlt" ~"Mn. daily median altitude",
  #                            var == "mnTort" ~ "Mn. daily tortuosity")
)

# PC1
PC1contrib <- contrib %>%
  ggplot(aes(x = reorder(var, PC1), y = PC1))+
  geom_col()+
  coord_flip()+
  ylab("PC1 % contribution")+
  xlab("")+
  # theme(panel.background = element_rect(fill = "#666666"),
  #       plot.background = element_rect(fill = "#666666"),
  #       text = element_text(color = "white", size = 15),
  #       axis.text = element_text(color = "white"),
  #       axis.ticks = element_line(color = "white"),
  #       axis.line = element_line(color = "white"))+
  theme(text = element_text(size = 18))
PC1contrib

PC2contrib <- contrib %>%
  ggplot(aes(x = reorder(var, PC2), y = PC2))+
  geom_col()+
  coord_flip()+
  ylab("PC2 % contribution")+
  xlab("")+
  # theme(panel.background = element_rect(fill = "#666666"),
  #       plot.background = element_rect(fill = "#666666"),
  #       text = element_text(color = "white", size = 15),
  #       axis.text = element_text(color = "white"),
  #       axis.ticks = element_line(color = "white"),
  #       axis.line = element_line(color = "white"))+
  theme(text = element_text(size = 18))
PC2contrib

# Check that the response variable distributions look normal--nothing too weird here.

linked %>%
  mutate(season = factor(season, levels = c("breeding", "summer", "fall"))) %>%
  ggplot(aes(x = degreeRelative))+
  geom_density(aes(col = season), linewidth = 1.5)+
  facet_wrap(~type, scales = "free_y")+
  scale_color_manual(name = "Season", values = seasonColors)+
  xlab("Degree (normalized)")+
  ylab("")+
  theme(text = element_text(size = 16))

linked %>%
  mutate(season = factor(season, levels = c("breeding", "summer", "fall"))) %>%
  ggplot(aes(x = strengthRelative))+
  geom_density(aes(col = season), linewidth = 1.5)+
  facet_wrap(~type, scales = "free_y")+
  scale_color_manual(name = "Season", values = seasonColors)+
  xlab("Strength (normalized)")+
  ylab("")+
  theme(text = element_text(size = 16))

linked %>%
  mutate(season = factor(season, levels = c("breeding", "summer", "fall"))) %>%
  ggplot(aes(x = evenness))+
  geom_density(aes(col = season), linewidth = 1.5)+
  facet_wrap(~type, scales = "free_y")+
  scale_color_manual(name = "Season", values = seasonColors)+
  xlab("Evenness")+
  ylab("")+
  theme(text = element_text(size = 16))

# Prediction plots with ggeffects -----------------------------------------
pc1_preds <- map(mods, ~ggpredict(.x, terms = "PC1")) %>%
  map2(., names(mods), ~.x %>% mutate(mod = .y, type = substr(mod, 1, 1), resp = substr(mod, 2, 2))) %>% 
  list_rbind()

pc1_preds %>%
  filter(!is.na(resp)) %>%
  mutate(resp = case_when(resp == "d" ~ "Degree (rel)",
                          resp == "s" ~ "Strength (rel)",
                          resp == "e" ~ "Evenness"),
         resp = factor(resp, levels = c("Degree (rel)", "Strength (rel)", "Evenness"))) %>%
  mutate(type = case_when(type == "f" ~ "flight",
                          type == "e" ~ "feeding",
                          type == "r" ~ "roosting"),
         type = factor(type, levels = c("flight", "feeding", "roosting"))) %>%
  ggplot(aes(x = x, y = predicted, fill = type))+
  geom_line(size = 1.5, aes(col = type))+
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.1)+
  scale_fill_manual(name = "Situation", values = c(flightColor, feedingColor, roostingColor))+
  scale_color_manual(name = "Situation", values = c(flightColor, feedingColor, roostingColor))+
  ylab("Predicted value")+
  xlab("PC1")+
  facet_wrap(~resp, scales = "free_y")

pc2_preds <- map(mods, ~ggpredict(.x, terms = "PC2")) %>%
  map2(., names(mods), ~.x %>% mutate(mod = .y, type = substr(mod, 1, 1), resp = substr(mod, 2, 2))) %>% 
  list_rbind()

pc2_preds %>%  
  filter(!is.na(resp)) %>%
  mutate(resp = case_when(resp == "d" ~ "Degree (rel)",
                          resp == "s" ~ "Strength (rel)",
                          resp == "e" ~ "Evenness"),
         resp = factor(resp, levels = c("Degree (rel)", "Strength (rel)", "Evenness"))) %>%
  mutate(type = case_when(type == "f" ~ "flight",
                          type == "e" ~ "feeding",
                          type == "r" ~ "roosting"),
         type = factor(type, levels = c("flight", "feeding", "roosting"))) %>%
  ggplot(aes(x = x, y = predicted, fill = type))+
  geom_line(size = 1.5, aes(col = type))+
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.1)+
  scale_fill_manual(name = "Situation", values = c(flightColor, feedingColor, roostingColor))+
  scale_color_manual(name = "Situation", values = c(flightColor, feedingColor, roostingColor))+
  ylab("Predicted value")+
  xlab("PC2")+
  facet_wrap(~resp, scales = "free_y")

pc2_preds <- map(mods, ~ggpredict(.x, terms = "PC2"))
age_preds <- map(mods, ~ggpredict(.x, terms = "age_group"))
season_preds <- map(mods, ~ggpredict(.x, terms = "season"))
year_preds <- map(mods, ~ggpredict(.x, terms = "year"))

# Visualize PC extremes ---------------------------------------------------
minPC1 <- linked %>% filter(PC1 == min(PC1, na.rm = T)) %>%
  dplyr::select(Nili_id, year, bnb) %>%
  distinct() %>%
  mutate(seasonName = paste(year, bnb, sep ="_"))
maxPC1 <- linked %>% filter(PC1 == max(PC1, na.rm = T)) %>%
  dplyr::select(Nili_id, year, bnb) %>%
  distinct() %>%
  mutate(seasonName = paste(year, bnb, sep ="_"))
minPC2 <- linked %>% filter(PC2 == min(PC2, na.rm = T)) %>%
  dplyr::select(Nili_id, year, bnb) %>%
  distinct() %>%
  mutate(seasonName = paste(year, bnb, sep ="_"))
maxPC2 <- linked %>% filter(PC2 == max(PC2, na.rm = T)) %>%
  dplyr::select(Nili_id, year, bnb) %>%
  distinct() %>%
  mutate(seasonName = paste(year, bnb, sep ="_"))

names(seasons_10min) <- seasonNames
minPC1Data <- seasons_10min[[minPC1$seasonName]] %>%
  filter(Nili_id == minPC1$Nili_id)
maxPC1Data <- seasons_10min[[maxPC1$seasonName]] %>%
  filter(Nili_id == maxPC1$Nili_id)
minPC2Data <- seasons_10min[[minPC2$seasonName]] %>%
  filter(Nili_id == minPC2$Nili_id)
maxPC2Data <- seasons_10min[[maxPC2$seasonName]] %>%
  filter(Nili_id == maxPC2$Nili_id)

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
  ggspatial::annotation_map_tile("cartodark", zoom = 9) + # this works well; only problem is that it takes a little while to fetch the tiles.
  geom_sf(aes(col = minmax), size = 0.7) +
  ggspatial::annotation_scale()+
  geom_path(data = pc1, 
            aes(x = location_long, y = location_lat, col = minmax),
            size = 0.5, alpha = 0.5)+
  scale_color_manual(name = "PC1", values = c("#E3809C", "#C70039"))+
  ylab("") + xlab("")+
  facet_wrap(~minmax)

# PC2 map--points and lines
pc2plot <- ggplot(data = pc2) +
  ggspatial::annotation_map_tile("cartodark", zoom = 9) + # this works well; only problem is that it takes a little while to fetch the tiles.
  geom_sf(aes(col = minmax), size = 0.7) +
  ggspatial::annotation_scale()+
  geom_path(data = pc2, 
            aes(x = location_long, y = location_lat, col = minmax),
            size = 0.5, alpha = 0.5)+
  scale_color_manual(name = "PC2", values = c("#C1A5FC", "#6E2EF9"))+
  ylab("") + xlab("")+
  facet_wrap(~minmax)

mapview(pc1, zcol = "minmax", lwd = 0, cex = 2, alpha = 0.5, col.regions = c("red", "blue"))

mapview(pc2, zcol = "minmax", lwd = 0, cex = 2, alpha = 0.5, col.regions = c("red", "blue"))


