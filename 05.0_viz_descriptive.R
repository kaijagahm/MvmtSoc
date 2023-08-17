# Descriptive visualizations
library(tidyverse)
load("data/linked.Rda")

# Set colors
cc <- list("breedingColor" = "#2FF8CA", "summerColor" = "#CA2FF8", "fallColor" = "#F8CA2F", flightColor = "dodgerblue", roostingColor = "olivedrab4", "feedingColor" = "gold")
save(cc, file = "data/cc.Rda")
load("data/cc.Rda")

# Check response variable distributions -----------------------------------
linked %>%
  mutate(season = factor(season, levels = c("breeding", "summer", "fall"))) %>%
  ggplot(aes(x = evenness))+
  geom_density(aes(col = season), linewidth = 1.5)+
  facet_wrap(~type, scales = "free_y")+
  scale_color_manual(name = "Season", values = c(cc[["breedingColor"]], cc[["summerColor"]], cc[["fallColor"]]))+
  xlab("Evenness")+
  ylab("")+
  theme_classic()+
  theme(text = element_text(size = 16))

linked %>%
  mutate(season = factor(season, levels = c("breeding", "summer", "fall"))) %>%
  ggplot(aes(x = degreeRelative))+
  geom_density(aes(col = season), linewidth = 1.5)+
  facet_wrap(~type, scales = "free_y")+
  scale_color_manual(name = "Season", values = c(cc[["breedingColor"]], cc[["summerColor"]], cc[["fallColor"]]))+
  xlab("Degree (normalized)")+
  ylab("")+ 
  theme_classic()+
  theme(text = element_text(size = 16))

linked %>%
  mutate(season = factor(season, levels = c("breeding", "summer", "fall"))) %>%
  ggplot(aes(x = degree))+
  geom_density(aes(col = season), linewidth = 1.5)+
  facet_wrap(~type*year, scales = "free_y")+
  scale_color_manual(name = "Season", values = c(cc[["breedingColor"]], cc[["summerColor"]], cc[["fallColor"]]))+
  xlab("Degree")+
  ylab("")+
  theme(text = element_text(size = 16))

linked %>%
  mutate(season = factor(season, levels = c("breeding", "summer", "fall"))) %>%
  ggplot(aes(x = strengthRelative))+
  geom_density(aes(col = season), linewidth = 1.5)+
  facet_wrap(~type, scales = "free_y")+
  scale_color_manual(name = "Season", values = c(cc[["breedingColor"]], cc[["summerColor"]], cc[["fallColor"]]))+
  xlab("Strength (normalized)")+
  ylab("")+
  theme(text = element_text(size = 16))

# Number of vultures (all and focal) --------------------------------------

focals <- linked %>%
  filter(!is.na(PC1)) %>%
  group_by(season, year, type, n) %>%
  summarize(nFocal = length(unique(Nili_id))) %>%
  arrange(year, season, type) %>%
  mutate(seasonUnique = paste(year, season, sep = "_")) %>%
  mutate(seasonUnique =factor(seasonUnique, levels = c("2020_fall", "2021_breeding", "2021_summer", "2021_fall", "2022_breeding", "2022_summer", "2022_fall", "2023_breeding"))) %>%
  mutate(seasonU = str_replace(seasonUnique, "breeding", "B")) %>%
  mutate(seasonU = str_replace(seasonU, "fall", "F")) %>%
  mutate(seasonU = str_replace(seasonU, "summer", "S")) %>%
  mutate(seasonU = str_remove(seasonU, "_")) %>%
  mutate(seasonU = str_remove(seasonU, "(?<=^)20")) %>%
  mutate(seasonU = factor(seasonU, levels = c("20F", "21B", "21S", "21F", "22B", "22S", "22F", "23B")))  %>%
  pivot_longer(cols = c(n, nFocal), names_to = "vultures", values_to = "n") %>%
  mutate(count = case_when(vultures == "n" ~ "all",
                           vultures == "nFocal" ~ "focal")) %>%
  ungroup()

focals %>%
  mutate(n = case_when(type == "feeding" ~ n + 1, 
                       TRUE ~ n)) %>%
  ggplot(aes(x = seasonU, y = n, col = type))+
  geom_line(aes(group = interaction(type, vultures), linetype = vultures))+
  geom_point(size = 4)+
  theme_classic()+
  ylab("# vultures")+
  xlab("Season")+
  scale_color_manual(values = c(cc[["feedingColor"]], cc[["flightColor"]], cc[["roostingColor"]]))+
  theme(text = element_text(size = 16))
