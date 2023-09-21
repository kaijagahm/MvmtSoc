library(tidyverse)
library(jtools)
library(sjPlot)
library(ggplot2)
library(ggeffects)
library(extrafont)
font_import()

source("ggplot_themes.R")

#load("data/linked.Rda") # DO NOT LOAD THIS!!!
load("data/forModeling.Rda") # loading this instead so we can run ggpredict properly.
load("data/mods.Rda")
load("data/cc.Rda")
load("data/scaled_d.Rda") # for un-scaling the degree model

# Response variable distributions -----------------------------------------
forModeling %>%
  pivot_longer(cols = c("degree", "strength", "evenness"), names_to = "measure", values_to = "value") %>%
  ggplot(aes(x = value, col = season, group = interaction(season, year)))+
  geom_density()+
  facet_wrap(~measure, scales = "free")+
  theme_classic()+
  scale_color_manual(name = "Season", values = c(cc[["breedingColor"]], cc[["fallColor"]], cc[["summerColor"]]))+
  ylab("")+xlab("")+theme(text = element_text(size = 16))

forModeling %>%
  pivot_longer(cols = c("degree_scl", "strength_scl", "evenness_scl"), names_to = "measure", values_to = "value") %>%
  ggplot(aes(x = value, col = season, group = interaction(season, year)))+
  geom_density()+
  facet_grid(~measure, scales = "free")+
  theme_classic()+
  scale_color_manual(name = "Season", values = c(cc[["breedingColor"]], cc[["fallColor"]], cc[["summerColor"]]))+
  ylab("")+xlab("")+theme(text = element_text(size = 16))

# Degree ------------------------------------------------------------------
d <- mods[["d"]]
scale_d <- attributes(scaled_d)$`scaled:scale`
center_d <- attributes(scaled_d)$`scaled:center` # follow the rest of Marta's code to figure out how to un-scale. But right now it's just in units of standard deviation.

# What is the relationship between PC1 and degree?
d_eff_pc1 <- as.data.frame(ggeffect(d, terms = c("PC1"))) %>%
  mutate(across(c("predicted", "conf.low", "conf.high"), ~ .x*scale_d+center_d))
ggplot(d_eff_pc1, aes(x, predicted))+
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
              alpha = 0.2, linewidth = 0.6, show.legend = F)+
  geom_line(linewidth = 1)+
  geom_point(data = forModeling, aes(x = PC1, y = degree), alpha = 0.5, size = 0.7)+
  ylab("Degree")+
  xlab("Movement (PC1)")+
  ggtitle("")

# What is the relationship between PC1 and degree, by situation? (We do have a significant interaction term between PC1 and situation)
d_eff_pc1_situ <- as.data.frame(ggeffect(d, terms = c("PC1", "situ"))) %>%
  mutate(across(c("predicted", "conf.low", "conf.high"), ~.x*scale_d+center_d)) %>%
  mutate(group = case_when(group == "Fe" ~ "Feeding", group == "Fl" ~ "Flight", group == "Ro" ~ "Roosting"))
d_pc1_situ <- ggplot(d_eff_pc1_situ, aes(x, predicted)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group),
              alpha = 0.2, linewidth = 0.6, show.legend = F)+
  geom_line(aes(col = group), linewidth = 1)+
  #geom_point(data = forModeling, aes(x = PC1, y = degree, col = situ), alpha = 0.5)+
  scale_color_manual(name = "Situation", values = c(cc[["feedingColor"]], cc[["flightColor"]], cc[["roostingColor"]]))+
  scale_fill_manual(name = "Situation", values = c(cc[["feedingColor"]], cc[["flightColor"]], cc[["roostingColor"]]))+
  ylab("Degree")+
  xlab("Movement (PC1)")+
  ggtitle("")+theme_quals()
ggsave(d_pc1_situ, file = "fig/d_pc1_situ.png", width = 9, height = 7)

pc1_situ_emt <- emmeans::emtrends(d, "situ", var = "PC1")
pc1_situ_emt # There is a slightly negative relationship between PC1 and degree in the co-feeding network (significant). There are significant positive relationships between PC1 and degree in co-flight and co-roosting networks. 
pairs(pc1_situ_emt) # There is no difference between the trends for flight and roosting. The relationship between PC1 and degree is significantly different in co-feeding networks than in co-roosting and co-flight networks. 

# What is the relationship between PC1 and degree, by situation and season?
d_eff_pc1_situ_season <- as.data.frame(ggeffect(d, terms = c("PC1", "situ", "season"))) %>%
  mutate(across(c("predicted", "conf.low", "conf.high"), ~.x*scale_d+center_d))
ggplot(d_eff_pc1_situ_season, aes(x, predicted)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group),
              alpha = 0.2, linewidth = 0.6, show.legend = F)+
  geom_line(aes(col = group), linewidth = 1)+
  scale_color_manual(name = "Situation", values = c(cc[["feedingColor"]], cc[["flightColor"]], cc[["roostingColor"]]))+
  scale_fill_manual(name = "Situation", values = c(cc[["feedingColor"]], cc[["flightColor"]], cc[["roostingColor"]]))+
  facet_wrap(~facet)+
  ylab("Degree")+
  xlab("Movement (PC1)")+
  ggtitle("") # I don't know how to tell if any of these are significant...

summary(d)

# What is the relationship between PC2 and degree?
d_eff_pc2 <- as.data.frame(ggeffect(d, terms = c("PC2"))) %>%
  mutate(across(c("predicted", "conf.low", "conf.high"), ~ .x*scale_d+center_d))
ggplot(d_eff_pc2, aes(x, predicted))+
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
              alpha = 0.2, linewidth = 0.6, show.legend = F)+
  geom_line(linewidth = 1)+
  geom_point(data = forModeling, aes(x = PC1, y = degree), alpha = 0.5, size = 0.7)+
  ylab("Degree")+
  xlab("Exploration (PC2)")+
  ggtitle("") # There is no significant marginal effect of PC2 on degree. 
tidy(d, effects = "fixed", conf.int = T) # to get the 95% CI for PC2 on degree. 

# What is the relationship between PC2 and degree, by situation?
d_eff_pc2_situ <- as.data.frame(ggeffect(d, terms = c("PC2", "situ"))) %>%
  mutate(across(c("predicted", "conf.low", "conf.high"), ~.x*scale_d+center_d)) %>%
  mutate(group = case_when(group == "Fe" ~ "Feeding", group == "Fl" ~ "Flight", group == "Ro" ~ "Roosting"))
d_pc2_situ <- ggplot(d_eff_pc2_situ, aes(x, predicted)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group),
              alpha = 0.2, linewidth = 0.6, show.legend = F)+
  geom_line(aes(col = group), linewidth = 1)+
  #geom_point(data = forModeling, aes(x = PC1, y = degree, col = situ), alpha = 0.5)+
  scale_color_manual(name = "Situation", values = c(cc[["feedingColor"]], cc[["flightColor"]], cc[["roostingColor"]]))+
  scale_fill_manual(name = "Situation", values = c(cc[["feedingColor"]], cc[["flightColor"]], cc[["roostingColor"]]))+
  ylab("Degree")+
  xlab("Exploration (PC2)")+
  ggtitle("")+theme_quals()
ggsave(d_pc2_situ, file = "fig/d_pc2_situ.png", width = 9, height = 7)

pc2_situ_emt <- emtrends(d, "situ", var = "PC2")
pc2_situ_emt # There is no relationship between exploration and degree in co-roosting networks. In both co-flight and co-feeding networks, there are significant negative relationships between exploration and PC2.
pairs(pc2_situ_emt) # as expected visually, feeding and flight are each different from roosting, but they are not different from each other.

summary(d) # no significant season interaction 
 
# Strength ----------------------------------------------------------------
s <- mods[["s"]]

# What is the effect of PC1 on strength?
s_eff_pc1 <- as.data.frame(ggeffect(s, terms = "PC1")) %>%
  mutate(across(c("predicted", "conf.low", "conf.high"), ~exp(.x)))
ggplot(s_eff_pc1, aes(x, predicted)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
              alpha = 0.2, linewidth = 0.6, show.legend = F)+
  #geom_point(data = forModeling, aes(x = PC1, y = strength, col = situ), alpha = 0.5, size = 0.7)+
  geom_line(linewidth = 1)+
  ylab("Strength")+
  xlab("Movement (PC1)")+
  ggtitle("")+theme_quals()

# What is the effect of PC1 on strength, by situation?
s_eff_pc1_situ <- as.data.frame(ggeffect(s, terms = c("PC1", "situ"))) %>%
  mutate(across(c("predicted", "conf.low", "conf.high"), ~exp(.x)))
ggplot(s_eff_pc1_situ, aes(x, predicted)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group),
              alpha = 0.2, linewidth = 0.6, show.legend = F)+
  geom_line(aes(col = group), linewidth = 1)+
  scale_color_manual(name = "Situation", values = c(cc[["feedingColor"]], cc[["flightColor"]], cc[["roostingColor"]]))+
  scale_fill_manual(name = "Situation", values = c(cc[["feedingColor"]], cc[["flightColor"]], cc[["roostingColor"]]))+
  facet_wrap(~group, scales = "free_y")+
  ylab("Strength")+
  xlab("Movement (PC1)")+
  ggtitle("")

# Non-exponentiated plot
s_eff_pc1_situ <- as.data.frame(ggeffect(s, terms = c("PC1", "situ"))) %>%
  mutate(group = case_when(group == "Fe" ~ "Feeding", group == "Fl" ~ "Flight", group == "Ro" ~ "Roosting"))
s_pc1_situ <- ggplot(s_eff_pc1_situ, aes(x, predicted)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group),
              alpha = 0.2, linewidth = 0.6, show.legend = F)+
  geom_line(aes(col = group), linewidth = 1)+
  scale_color_manual(name = "Situation", values = c(cc[["feedingColor"]], cc[["flightColor"]], cc[["roostingColor"]]))+
  scale_fill_manual(name = "Situation", values = c(cc[["feedingColor"]], cc[["flightColor"]], cc[["roostingColor"]]))+
  #facet_wrap(~group, scales = "free_y")+
  ylab("log(Strength)")+
  xlab("Movement (PC1)")+
  ggtitle("")+theme_quals()
ggsave(s_pc1_situ, file = "fig/s_pc1_situ.png", width = 9, height = 7)
pc1_situ_emt <- emtrends(s, "situ", var = "PC1")
pc1_situ_emt # 
pairs(pc1_situ_emt) # 

# in terms of percent decreases:
as.data.frame(pc1_situ_emt) %>% mutate(across(c("PC1.trend", "lower.CL", "upper.CL"), 
                                              ~round(100*(1-exp(.x)), 2)))

summary(s) # there is a very very slightly significant three-way interaction with season, so let's look at that:

# What about by season? (non-exponentiated)
s_eff_pc1_situ_season <- as.data.frame(ggeffect(s, terms = c("PC1", "situ", "season")))%>%
  mutate(group = case_when(group == "Fe" ~ "Feeding", 
                          group == "Fl" ~ "Flight",
                          group == "Ro" ~ "Roosting",
                          TRUE ~ group)) %>%
  mutate(facet = case_when(facet == "breeding" ~ "Breeding",
                           facet == "summer" ~ "Summer",
                           facet == "fall" ~ "Fall",
                           TRUE ~ facet))
s_pc1_situ_season <- ggplot(s_eff_pc1_situ_season, aes(x, predicted)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group),
              alpha = 0.2, linewidth = 0.6, show.legend = F)+
  geom_line(aes(col = group), linewidth = 1)+
  scale_color_manual(name = "Situation", values = c(cc[["feedingColor"]], cc[["flightColor"]], cc[["roostingColor"]]))+
  scale_fill_manual(name = "Situation", values = c(cc[["feedingColor"]], cc[["flightColor"]], cc[["roostingColor"]]))+
  facet_wrap(~facet)+
  ylab("Strength (log-transformed)")+
  xlab("Movement (PC1)")+
  ggtitle("")+
  theme_quals()
ggsave(s_pc1_situ_season, file = "fig/s_pc1_situ_season.png", width = 9, height = 7)

pc1_situ_season_emt <- emtrends(s, specs = c("situ", "season"), var = "PC1")
pc1_situ_season_emt # There is no significant trend in any season for co-flight. For both co-roosting and co-feeding, there is a significant negative trend in the breeding season and the fall, but not in the summer. So, in summer, there is no significant relationship between movement and strength at all.
pairs(pc1_situ_season_emt)

# in terms of percent decreases:
as.data.frame(pc1_situ_season_emt) %>% mutate(across(c("PC1.trend", "lower.CL", "upper.CL"), 
                                              ~round(100*(1-exp(.x)), 2)))
# percent decrease ranges 6.68 (fe breeding), 9.32 (ro breeding), 9.22 (fe fall), 5.11 (ro fall)

pairs(pc1_situ_season_emt)
# Are the situations different from each other in breeding and fall?
# Fe breeding - Ro breeding: n.s.
# Fe fall - Ro fall: n.s.
# There is no difference between feeding and roosting in the seasons in which they each show negative trends.

# Are the seasons different from each other for roosting and feeding?
# Ro breeding - Ro fall: n.s.
# Fe breeding - Fe fall: n.s.
# There is no difference between breeding and fall seasons within either the feeding or roosting situations.

# So basically, there is one type of negative relationship, present in breeding and fall, for feeding and roosting, with no further distinctions. 

# What is the effect of PC2 on strength?
s_eff_pc2 <- as.data.frame(ggeffect(s, terms = "PC2")) %>%
  mutate(across(c("predicted", "conf.low", "conf.high"), ~exp(.x)))
s_pc2 <- ggplot(s_eff_pc2, aes(x, predicted)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
              alpha = 0.2, linewidth = 0.6, show.legend = F)+
  #geom_point(data = forModeling, aes(x = PC1, y = strength, col = situ), alpha = 0.5, size = 0.7)+
  geom_line(linewidth = 1)+
  ylab("Strength")+
  xlab("Exploration (PC2)")+
  ggtitle("")+theme_quals()
ggsave(s_pc2, file = "fig/s_pc2.png", width = 9, height = 7)

# There was no significant interaction of PC2 with either season or situation (or both)
tidy(s, effects = "fixed", conf.int = T) # to get the 95% CI for PC2 on strength: [-0.106, -0.0400]
tidy(s, effects = "fixed", conf.int = T) %>% mutate(across(c("estimate", "conf.low", "conf.high"), ~100*(1-exp(.x)))) # 7.04% decrease, 95% CI [3.92, 10.1]%
