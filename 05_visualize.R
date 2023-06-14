library(tidyverse)
library(jtools)
library(sjPlot)
library(ggplot2)

load("data/mods.Rda")
load("data/effects.Rda")
load("data/contrib.Rda")
load("data/linked.Rda")

# Set season colors
seasonColors <- c("#2FF8CA", "#CA2FF8", "#F8CA2F")
situationColors <- c("dodgerblue2", "olivedrab3", "gold")

# Effect size plots
# Here are some plots made with jtools, following the vignette here: https://cran.r-project.org/web/packages/jtools/vignettes/summ.html

# Degree
deg.jt <- plot_summs(mods[["fd"]], mods[["ed"]], mods[["rd"]], inner_ci_level = .9,
           model.names = c("Flight", "Feeding", "Roosting"), coefs = c("PC1", "PC2"))+
  scale_color_manual(values = situationColors)+
  ggtitle("Degree (normalized)")
deg.jt

# Strength
str.jt <- plot_summs(mods[["fs"]], mods[["es"]], mods[["rs"]], inner_ci_level = .9,
           model.names = c("Flight", "Feeding", "Roosting"), coefs = c("PC1", "PC2"))+
  scale_color_manual(values = situationColors)+
  ggtitle("Strength (normalized)")
str.jt

# Evenness
eve.jt <- plot_summs(mods[["fe"]], mods[["ee"]], mods[["re"]], inner_ci_level = .9,
           model.names = c("Flight", "Feeding", "Roosting"), coefs = c("PC1", "PC2"))+
  scale_color_manual(values = situationColors)+
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
  scale_color_manual(values = situationColors)+
  ggtitle("Degree (normalized)")
deg.jt.full # year, age, and season have much larger impacts than PC1 and PC2 do.

# Strength
str.jt.full <- plot_summs(mods[["fs"]], mods[["es"]], mods[["rs"]], inner_ci_level = .9,
                     model.names = c("Flight", "Feeding", "Roosting"))+ 
  scale_color_manual(values = situationColors)+
  ggtitle("Strength (normalized)")
str.jt.full

# Evenness
eve.jt.full <- plot_summs(mods[["fe"]], mods[["ee"]], mods[["re"]], inner_ci_level = .9,
                     model.names = c("Flight", "Feeding", "Roosting"))+
  scale_color_manual(values = situationColors)+
  ggtitle("Evenness")
eve.jt.full

# Which effects are included in the models?
# By response variable:
effects %>%
  select(type, response, term, p.value) %>%
  mutate(response = case_when(response == "degreeRelative" ~ "D",
                              response == "strengthRelative" ~ "S",
                              response == "evenness" ~ "E")) %>%
  filter(!is.na(p.value)) %>%
  mutate(sig = ifelse(p.value < 0.05, T, F)) %>%
  filter(!(term %in% c("(Intercept)", "sd__(Intercept)", "sd__Observation"))) %>%
  mutate(term = case_when(term == "age_groupadult" ~ "ageA",
                          term == "seasonsummer" ~ "summer",
                          term == "seasonfall" ~ "fall",
                          term == "year2021" ~ "2021",
                          term == "year2022" ~ "2022",
                          term == "PC1:age_groupadult" ~ "PC1:ageA",
                          term == "age_groupadult:PC2" ~ "PC2:ageA",
                          term == "seasonsummer:year2021" ~ "summer:2021",
                          term == "seasonfall:year2021" ~ "fall:2021",
                          term == "seasonsummer:year2022" ~ "summer:2022",
                          term == "seasonfall:year2022" ~ "fall:2022",
                          term == "PC2:age_groupadult" ~ "PC2:ageA",
                          TRUE ~ term)) %>%
  mutate(term = factor(term, levels = c("PC1", "PC2", "ageA", "PC1:ageA", "PC2:ageA", "summer", "fall", "2021", "2022", "summer:2021", "fall:2021", "summer:2022", "fall:2022"))) %>%
  arrange(response, type) %>%
  mutate(responseType = paste(response, type, sep = "_")) %>%
  ggplot(aes(x = term, y = responseType))+
  geom_tile(aes(fill = response), col = "black")+
  ylab("")+
  xlab("")+
  theme(text = element_text(size = 16), legend.position = "none")

# By situation
effects %>%
  select(type, response, term, p.value) %>%
  mutate(response = case_when(response == "degreeRelative" ~ "D",
                              response == "strengthRelative" ~ "S",
                              response == "evenness" ~ "E")) %>%
  filter(!is.na(p.value)) %>%
  mutate(sig = ifelse(p.value < 0.05, T, F)) %>%
  filter(!(term %in% c("(Intercept)", "sd__(Intercept)", "sd__Observation"))) %>%
  mutate(term = case_when(term == "age_groupadult" ~ "ageA",
                          term == "seasonsummer" ~ "summer",
                          term == "seasonfall" ~ "fall",
                          term == "year2021" ~ "2021",
                          term == "year2022" ~ "2022",
                          term == "PC1:age_groupadult" ~ "PC1:ageA",
                          term == "age_groupadult:PC2" ~ "PC2:ageA",
                          term == "seasonsummer:year2021" ~ "summer:2021",
                          term == "seasonfall:year2021" ~ "fall:2021",
                          term == "seasonsummer:year2022" ~ "summer:2022",
                          term == "seasonfall:year2022" ~ "fall:2022",
                          term == "PC2:age_groupadult" ~ "PC2:ageA",
                          TRUE ~ term)) %>%
  mutate(term = factor(term, levels = c("PC1", "PC2", "ageA", "PC1:ageA", "PC2:ageA", "summer", "fall", "2021", "2022", "summer:2021", "fall:2021", "summer:2022", "fall:2022"))) %>%
  arrange(type, response) %>%
  mutate(typeResponse = paste(type, response, sep = "_")) %>%
  ggplot(aes(x = term, y = typeResponse))+
  geom_tile(color = "black", aes(fill = type))+
  scale_fill_manual(values = c("olivedrab3", "gold", "dodgerblue2"))+
  ylab("")+
  xlab("")+
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

# Some plots of the response variables
linked %>%
  mutate(season = factor(season, levels = c("breeding", "summer", "fall"))) %>%
  ggplot(aes(x = evenness))+
  geom_density(aes(col = season), linewidth = 1.5)+
  facet_wrap(~type, scales = "free_y")+
  scale_color_manual(name = "Season", values = seasonColors)+
  xlab("Evenness")+
  ylab("")+
  theme(text = element_text(size = 16))

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
