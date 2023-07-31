# In lieu of tidyverse
library(ggplot2)
library(tidyr)
library(stringr)
library(readr)
library(dplyr)
library(tibble)
library(purrr)
#
library(jtools)
library(sjPlot)
library(ggplot2)
library(ggeffects)

load("data/mods.Rda")
load("data/contrib.Rda")
load("data/linked.Rda")
load("data/seasons_10min.Rda")

# Set season colors
seasonColors <- c("#2FF8CA", "#CA2FF8", "#F8CA2F")
breedingColor <- "#2FF8CA"
summerColor <- "#CA2FF8"
fallColor <- "#F8CA2F"
situationColors <- c("dodgerblue2", "olivedrab3", "gold") # flight, roosting, feeding
flightColor <- "dodgerblue2"
roostingColor <- "olivedrab3"
feedingColor <- "gold"

# Examine covariates ------------------------------------------------------
# SEX
# ## differences in PC values by sex
# long <- linked %>%
#   filter(sex %in% c("f", "m")) %>%
#   dplyr::select(Nili_id, season, age, age_group, sex, PC1, PC2, PC3) %>%
#   pivot_longer(cols = c("PC1", "PC2", "PC3"), values_to = "value", 
#                names_to = "PC")
# stat.test <- long %>%
#   group_by(PC) %>%
#   t_test(value ~ sex) %>%
#   adjust_pvalue(method = "bonferroni") %>%
#   add_significance() %>% 
#   add_xy_position(x = "sex")
# 
# bxp <- ggboxplot(
#   long, x = "sex", y = "value", 
#   fill = "sex", 
#   palette = c("orange", "skyblue3"),
#   facet.by = "PC"
# ) + stat_pvalue_manual(stat.test)
# bxp
# 
# linked %>%
#   dplyr::select(sex, type, Nili_id, degreeRelative, strengthRelative, evenness) %>%
#   pivot_longer(cols = c("degreeRelative", "strengthRelative", "evenness"), 
#                names_to = "socialPositionMeasure", values_to = "value") %>%
#   filter(!is.na(sex)) %>%
#   ggplot(aes(x = sex, y = value, fill = sex))+
#   geom_boxplot()+
#   theme_minimal()+
#   scale_fill_manual(values = c("orange", "skyblue3"))+
#   theme(legend.position = "none")+
#   facet_wrap(facets = vars(type, socialPositionMeasure), 
#              nrow = 3, ncol = 3, scales= "free")+
#   ylab("Value")+
#   xlab("Sex")
# 
# # AGE
# ## differences in PC values by age
# long %>%
#   ggplot(aes(x = age, y = value, col = PC))+
#   geom_point(size = 1.5, alpha = 0.7)+
#   geom_smooth(method = "lm")+
#   scale_color_viridis_d(option = "D")+
#   theme(text = element_text(size = 18))+
#   ylab("Value")+
#   xlab("Age")
# 
# stat.test_age <- long %>%
#   group_by(PC) %>%
#   t_test(value ~ age_group) %>%
#   adjust_pvalue(method = "bonferroni") %>%
#   add_significance() %>% 
#   add_xy_position(x = "age_group")
# 
# bxp_age <- ggboxplot(
#   long, x = "age_group", y = "value", 
#   fill = "age_group", 
#   palette = c("red", "red4"),
#   facet.by = "PC"
# ) + stat_pvalue_manual(stat.test_age)
# bxp_age
# 
# ## differences in degree/strength by age 
# head(linked)
# linked %>%
#   dplyr::select(age_group, type, Nili_id, degreeRelative, strengthRelative, evenness) %>%
#   pivot_longer(cols = c("degreeRelative", "strengthRelative", "evenness"), names_to = "socialPositionMeasure", values_to = "value") %>%
#   filter(!is.na(age_group)) %>%
#   ggplot(aes(x = age_group, y = value, fill = age_group))+
#   geom_boxplot()+
#   theme_minimal()+
#   scale_fill_manual(values = c("red", "red4"))+
#   theme(legend.position = "none")+
#   facet_wrap(facets = vars(type, socialPositionMeasure), 
#              nrow = 3, ncol = 3, scales= "free")+
#   ylab("Value")+
#   xlab("Age group")
# 
# # SEASON
# linked %>%
#   dplyr::select(type, Nili_id, degreeRelative, strengthRelative, evenness, season) %>%
#   pivot_longer(cols = c("degreeRelative", "strengthRelative", "evenness"), names_to = "socialPositionMeasure", values_to = "value") %>%
#   filter(!is.na(season)) %>%
#   ggplot(aes(x = season, y = value, fill = season))+
#   geom_boxplot()+
#   theme_minimal()+
#   scale_fill_manual(values = seasonColors)+
#   theme(legend.position = "none")+
#   facet_wrap(facets = vars(type, socialPositionMeasure), 
#              nrow = 3, ncol = 3, scales= "free")+
#   ylab("Value")+
#   xlab("Season") 
# 
# ## differences in PC values by season
# long <- linked %>%
#   dplyr::select(Nili_id, season, age, age_group, sex, PC1, PC2, PC3) %>%
#   pivot_longer(cols = c("PC1", "PC2", "PC3"), values_to = "value", 
#                names_to = "PC")
# stat.test <- long %>%
#   group_by(PC) %>%
#   t_test(value ~ season) %>%
#   adjust_pvalue(method = "bonferroni") %>%
#   add_significance() %>% 
#   add_xy_position(x = "season")
# 
# bxp <- ggboxplot(
#   long, x = "season", y = "value", 
#   fill = "season", 
#   palette = seasonColors,
#   facet.by = "PC"
# ) + stat_pvalue_manual(stat.test)
# bxp # we definitely have seasonal differences in movement! As expected.

# Effect size plots
# Here are some plots made with jtools, following the vignette here: https://cran.r-project.org/web/packages/jtools/vignettes/summ.html

# Degree
plot_model(mods[["d"]])
deg_pc1 <- ggpredict(mods[["d"]], terms = c("PC1", "season", "situ"))
ggplot(deg_pc1, aes(x, predicted))+
  geom_line(linewidth = 1.5, aes(col = group))+
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.1)+
  facet_wrap(~facet)+
  scale_color_manual(name = "Season", values = c(breedingColor, summerColor, fallColor))+
  scale_fill_manual(name = "Season", values = c(breedingColor, summerColor, fallColor))+
  ylab("Degree")+
  xlab("Movement (PC1)")+
  theme(text = element_text(size = 16))

deg_pc2 <- ggpredict(mods[["d"]], terms = c("PC2", "season", "situ"))
ggplot(deg_pc2, aes(x, predicted))+
  geom_line(linewidth = 1.5, aes(col = group))+
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.1)+
  facet_wrap(~facet)+
  scale_color_manual(name = "Season", values = c(breedingColor, summerColor, fallColor))+
  scale_fill_manual(name = "Season", values = c(breedingColor, summerColor, fallColor))+
  ylab("Degree")+
  xlab("Exploration (PC2)")+
  theme(text = element_text(size = 16))

# Strength
str_pc1 <- ggpredict(mods[["s"]], terms = c("PC1", "season", "situ"))
ggplot(str_pc1, aes(x, predicted))+
  geom_line(linewidth = 1.5, aes(col = group))+
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.1)+
  facet_wrap(~facet)+
  scale_color_manual(name = "Season", values = c(breedingColor, summerColor, fallColor))+
  scale_fill_manual(name = "Season", values = c(breedingColor, summerColor, fallColor))+
  ylab("Strength")+
  xlab("Movement (PC1)")+
  theme(text = element_text(size = 16))

str_pc2 <- ggpredict(mods[["s"]], terms = c("PC2", "season", "situ"))
ggplot(str_pc2, aes(x, predicted))+
  geom_line(linewidth = 1.5, aes(col = group))+
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.1)+
  facet_wrap(~facet)+
  scale_color_manual(name = "Season", values = c(breedingColor, summerColor, fallColor))+
  scale_fill_manual(name = "Season", values = c(breedingColor, summerColor, fallColor))+
  ylab("Strength")+
  xlab("Exploration (PC2)")+
  theme(text = element_text(size = 16))

# Evenness
eve_pc1 <- ggpredict(mods[["e"]], terms = c("PC1", "season", "situ"))
ggplot(eve_pc1, aes(x, predicted))+
  geom_line(linewidth = 1.5, aes(col = group))+
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.1)+
  facet_wrap(~facet)+
  scale_color_manual(name = "Season", values = c(breedingColor, summerColor, fallColor))+
  scale_fill_manual(name = "Season", values = c(breedingColor, summerColor, fallColor))+
  ylab("Evenness")+
  xlab("Movement (PC1)")+
  theme(text = element_text(size = 16))

eve_pc2 <- ggpredict(mods[["e"]], terms = c("PC2", "season", "situ"))
ggplot(eve_pc2, aes(x, predicted))+
  geom_line(linewidth = 1.5, aes(col = group))+
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.1)+
  facet_wrap(~facet)+
  scale_color_manual(name = "Season", values = c(breedingColor, summerColor, fallColor))+
  scale_fill_manual(name = "Season", values = c(breedingColor, summerColor, fallColor))+
  ylab("Evenness")+
  xlab("Exploration (PC2)")+
  theme(text = element_text(size = 16))


# STRENGTH: log-transformed y variable
# Here is how I would make a plot of that using ggeffects
# mydf <- ggpredict(mods[["s"]], terms = c("PC1", "season", "situ"))
# ggplot(mydf, aes(x, exp(predicted), col = group, fill = group)) + # the "group" term is the second one listed in the ggpredict call.
#   geom_line() +
#   geom_ribbon(aes(ymin = exp(conf.low), ymax = exp(conf.high)), alpha = 0.1)+
#   facet_wrap(~facet) # the "facet" term is the third one listed in the ggpredict call

# Which effects are included in the models?
# effectsGeneral <- effects %>%
#   dplyr::select(type, response, term) %>%
#   distinct() %>%
#   mutate(term = case_when(term == "age_groupadult" ~ "age",
#                           term %in% c("seasonsummer", "seasonfall") ~ "season",
#                           term %in% c("year2021", "year2022") ~ "year",
#                           term %in% c("age_groupadult:PC2", "PC2:age_groupadult") ~ "PC2:age",
#                           term == "PC1:age_groupadult" ~ "PC1:age",
#                           grepl("season", term) & grepl("year", term) ~ "season:year",
#                           TRUE ~ term)) %>%
#   distinct() %>%
#   filter(!(grepl("sd_", term)), term != "(Intercept)") %>%
#   mutate(resp = case_when(response == "degreeRelative" ~ "D",
#                           response == "strengthRelative" ~ "S",
#                           response == "evenness" ~ "E")) %>%
#   mutate(term = factor(term, levels = c("PC1", "PC2", "age", "season", "year", "PC1:age", "PC2:age", "season:year")))
# 
# # By response variable:
# effectsGeneral %>%
#   arrange(resp, type) %>%
#   mutate(responseType = paste(resp, type, sep = "_")) %>%
#   ggplot(aes(x = term, y = responseType))+
#   geom_tile(aes(fill = resp), col = "black")+
#   ylab("")+
#   xlab("")+
#   theme(text = element_text(size = 16), legend.position = "none")
# 
# # By situation
# effectsGeneral %>%
#   arrange(type, resp) %>%
#   mutate(typeResponse = paste(type, resp, sep = "_")) %>%
#   ggplot(aes(x = term, y = typeResponse))+
#   geom_tile(color = "black", aes(fill = type))+
#   ylab("")+
#   xlab("")+
#   scale_fill_manual(values = c(feedingColor, flightColor, roostingColor))+
#   theme(text = element_text(size = 16), legend.position = "none")
#   

# Variable contribution plots

# plots -------------------------------------------------------------------
# Let's look at the PC contributions
contrib <- contrib %>%
  mutate(var = row.names(.),
         varName = case_when(var == "meanDMD" ~ "Mn. daily max displacement",
                             var == "propSwitch" ~"Prop. nights roost-switching",
                             var == "shannon" ~"Roost diversity",
                             var == "coreArea" ~"Core area",
                             var == "coreAreaFidelity" ~"50%/95% KDE",
                             var == "homeRange"~"Home range",
                             var == "meanDDT" ~ "Mn. daily distance traveled",
                             var == "uniqueRoosts"~"# unique roosts",
                             var == "meanDFD" ~"Mn. daily flight time",
                             var == "mnDailyMaxAlt" ~"Mn. daily max. altitude",
                             var == "mnDailyMedAlt" ~"Mn. daily median altitude",
                             var == "mnTort" ~ "Mn. daily tortuosity")
)

# PC1
PC1contrib <- contrib %>%
  ggplot(aes(x = reorder(varName, PC1), y = PC1))+
  geom_col(fill = "#7A695A")+
  coord_flip()+
  ylab("PC1 % contribution")+
  xlab("")+
  theme(panel.background = element_rect(fill = "#FFFCF6"),
        plot.background = element_rect(fill = "#FFFCF6"),
        text = element_text(color = "#7A695A", size = 18),
        axis.text = element_text(color = "#7A695A"),
        axis.ticks = element_line(color = "#7A695A"),
        axis.line = element_line(color = "#7A695A"))+
  NULL
PC1contrib

PC2contrib <- contrib %>%
  ggplot(aes(x = reorder(varName, PC2), y = PC2))+
  geom_col(fill = "#7A695A")+
  coord_flip()+
  ylab("PC2 % contribution")+
  xlab("")+
  theme(panel.background = element_rect(fill = "#FFFCF6"),
        plot.background = element_rect(fill = "#FFFCF6"),
        text = element_text(color = "#7A695A", size = 18),
        axis.text = element_text(color = "#7A695A"),
        axis.ticks = element_line(color = "#7A695A"),
        axis.line = element_line(color = "#7A695A"))+
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
# pc1_preds <- map(mods, ~ggpredict(.x, terms = "PC1")) %>%
#   map2(., names(mods), ~.x %>% mutate(mod = .y, type = substr(mod, 1, 1), resp = substr(mod, 2, 2))) %>% 
#   list_rbind()
# 
# pc1_preds %>%
#   filter(!is.na(resp)) %>%
#   mutate(resp = case_when(resp == "d" ~ "Degree (rel)",
#                           resp == "s" ~ "Strength (rel)",
#                           resp == "e" ~ "Evenness"),
#          resp = factor(resp, levels = c("Degree (rel)", "Strength (rel)", "Evenness"))) %>%
#   mutate(type = case_when(type == "f" ~ "flight",
#                           type == "e" ~ "feeding",
#                           type == "r" ~ "roosting"),
#          type = factor(type, levels = c("flight", "feeding", "roosting"))) %>%
#   ggplot(aes(x = x, y = predicted, fill = type))+
#   geom_line(size = 1.5, aes(col = type))+
#   geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.1)+
#   scale_fill_manual(name = "Situation", values = c(flightColor, feedingColor, roostingColor))+
#   scale_color_manual(name = "Situation", values = c(flightColor, feedingColor, roostingColor))+
#   ylab("Predicted value")+
#   xlab("PC1")+
#   facet_wrap(~resp, scales = "free_y")
# 
# pc2_preds <- map(mods, ~ggpredict(.x, terms = "PC2")) %>%
#   map2(., names(mods), ~.x %>% mutate(mod = .y, type = substr(mod, 1, 1), resp = substr(mod, 2, 2))) %>% 
#   list_rbind()
# 
# pc2_preds %>%  
#   filter(!is.na(resp)) %>%
#   mutate(resp = case_when(resp == "d" ~ "Degree (rel)",
#                           resp == "s" ~ "Strength (rel)",
#                           resp == "e" ~ "Evenness"),
#          resp = factor(resp, levels = c("Degree (rel)", "Strength (rel)", "Evenness"))) %>%
#   mutate(type = case_when(type == "f" ~ "flight",
#                           type == "e" ~ "feeding",
#                           type == "r" ~ "roosting"),
#          type = factor(type, levels = c("flight", "feeding", "roosting"))) %>%
#   ggplot(aes(x = x, y = predicted, fill = type))+
#   geom_line(size = 1.5, aes(col = type))+
#   geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.1)+
#   scale_fill_manual(name = "Situation", values = c(flightColor, feedingColor, roostingColor))+
#   scale_color_manual(name = "Situation", values = c(flightColor, feedingColor, roostingColor))+
#   ylab("Predicted value")+
#   xlab("PC2")+
#   facet_wrap(~resp, scales = "free_y")
# 
# pc2_preds <- map(mods, ~ggpredict(.x, terms = "PC2"))
# age_preds <- map(mods, ~ggpredict(.x, terms = "age_group"))
# season_preds <- map(mods, ~ggpredict(.x, terms = "season"))
# year_preds <- map(mods, ~ggpredict(.x, terms = "year"))

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


