library(car) # for p-values in mixed models?
library(tidyverse)
library(sf)
library(factoextra)
library(factoextra)
library(ggfortify)
library(ggpmisc) # for more details on a linear regression plot
library(lme4) # for mixed-effects models
library(ggpubr) # for adding p-values to boxplots
library(rstatix) # for adding p-values to boxplots
library(easystats) # for modeling and comparisons
library(performance) # for modeling and comparisons
library(lmerTest) # for p-values in mixed models?
library(glmmTMB) # more complicated than lme4

load("data/linked.Rda")

# Remove individuals that don't have movement information
linked <- linked %>%
  filter(!is.na(PC1)) # note that because I joined the sex/age information along with the movement information (instead of along with the social information), if I ever want to *not* filter out those individuals, I'll have to go back and add sex/age info for the additional birds. Don't need this for now, so I'm skipping it. 
colSums(is.na(linked)) # okay now we have no more NA's except in sex

# Examine covariates ------------------------------------------------------
# SEX
## differences in PC values by sex
long <- linked %>%
  filter(sex %in% c("f", "m")) %>%
  dplyr::select(Nili_id, season, age, age_group, sex, PC1, PC2, PC3) %>%
  pivot_longer(cols = c("PC1", "PC2", "PC3"), values_to = "value", 
               names_to = "PC")
stat.test <- long %>%
  group_by(PC) %>%
  t_test(value ~ sex) %>%
  adjust_pvalue(method = "bonferroni") %>%
  add_significance() %>% 
  add_xy_position(x = "sex")

bxp <- ggboxplot(
  long, x = "sex", y = "value", 
  fill = "sex", 
  palette = c("orange", "skyblue3"),
  facet.by = "PC"
) + stat_pvalue_manual(stat.test)
bxp

linked %>%
  dplyr::select(sex, type, Nili_id, degreeRelative, strengthRelative, sbd) %>%
  pivot_longer(cols = c("degreeRelative", "strengthRelative", "sbd"), 
               names_to = "socialPositionMeasure", values_to = "value") %>%
  filter(!is.na(sex)) %>%
  ggplot(aes(x = sex, y = value, fill = sex))+
  geom_boxplot()+
  theme_minimal()+
  scale_fill_manual(values = c("orange", "skyblue3"))+
  theme(legend.position = "none")+
  facet_wrap(facets = vars(type, socialPositionMeasure), 
             nrow = 3, ncol = 3, scales= "free")+
  ylab("Value")+
  xlab("Sex")

# AGE
## differences in PC values by age
long %>%
  ggplot(aes(x = age, y = value, col = PC))+
  geom_point(size = 1.5, alpha = 0.7)+
  geom_smooth(method = "lm")+
  theme_classic()+
  scale_color_viridis_d(option = "D")+
  theme(text = element_text(size = 18))+
  ylab("Value")+
  xlab("Age")

stat.test_age <- long %>%
  group_by(PC) %>%
  t_test(value ~ age_group) %>%
  adjust_pvalue(method = "bonferroni") %>%
  add_significance() %>% 
  add_xy_position(x = "age_group")

bxp_age <- ggboxplot(
  long, x = "age_group", y = "value", 
  fill = "age_group", 
  palette = c("red", "red4"),
  facet.by = "PC"
) + stat_pvalue_manual(stat.test_age)
bxp_age

## differences in degree/strength by age 
head(linked)
linked %>%
  dplyr::select(age_group, type, Nili_id, degreeRelative, strengthRelative, sbd) %>%
  pivot_longer(cols = c("degreeRelative", "strengthRelative", "sbd"), names_to = "socialPositionMeasure", values_to = "value") %>%
  filter(!is.na(age_group)) %>%
  ggplot(aes(x = age_group, y = value, fill = age_group))+
  geom_boxplot()+
  theme_minimal()+
  scale_fill_manual(values = c("red", "red4"))+
  theme(legend.position = "none")+
  facet_wrap(facets = vars(type, socialPositionMeasure), 
             nrow = 3, ncol = 3, scales= "free")+
  ylab("Value")+
  xlab("Age group")

# SEASON
linked %>%
  dplyr::select(type, Nili_id, degreeRelative, strengthRelative, sbd, season) %>%
  pivot_longer(cols = c("degreeRelative", "strengthRelative", "sbd"), names_to = "socialPositionMeasure", values_to = "value") %>%
  filter(!is.na(season)) %>%
  ggplot(aes(x = season, y = value, fill = season))+
  geom_boxplot()+
  theme_minimal()+
  scale_fill_manual(values = c("black", "gray"))+
  theme(legend.position = "none")+
  facet_wrap(facets = vars(type, socialPositionMeasure), 
             nrow = 3, ncol = 3, scales= "free")+
  ylab("Value")+
  xlab("Season") # no immediate obvious differences, but there probably are some.

## differences in PC values by season
long <- linked %>%
  dplyr::select(Nili_id, season, age, age_group, sex, PC1, PC2, PC3) %>%
  pivot_longer(cols = c("PC1", "PC2", "PC3"), values_to = "value", 
               names_to = "PC")
stat.test <- long %>%
  group_by(PC) %>%
  t_test(value ~ season) %>%
  adjust_pvalue(method = "bonferroni") %>%
  add_significance() %>% 
  add_xy_position(x = "season")

bxp <- ggboxplot(
  long, x = "season", y = "value", 
  fill = "season", 
  palette = c("black", "gray"),
  facet.by = "PC"
) + stat_pvalue_manual(stat.test)
bxp

# Mixed models -------------------------------------------------------------------
# Using the `performance` package that Noa showed me
# Starting with CO-FLIGHT

# Response variables:
# Relative degree
# Relative strength
# Strength by degree

# Predictors:
# PC1 (continuous)
# PC2 (continuous)
# age (cat: adult/juv_sub)
# sex (cat: m/f)
# season (cat: b/nb)
# year (cat)
# Nili_id (random effect) (1|Nili_id)
flight <- linked %>%
  filter(type == "flight")
feeding <- linked %>%
  filter(type == "feeding")
roosting <- linked %>%
  filter(type == "roosting")

# FLIGHT: RELATIVE DEGREE
# I thought we couldn't include random effects if they had fewer than 5 levels, but maybe you can? https://www.ncbi.nlm.nih.gov/pmc/articles/PMC8784019/

# include everything, with interactions between plausible things to interact
fd_max <- lmer(degreeRelative ~ age_group*PC1 + age_group*PC2 + age_group*season + season*PC1 + season*PC2 + age_group + season + (1|Nili_id) + (1|year), data = flight)
check_model(fd_max)
summary(fd_max)

# Remove interaction between age group and season
fd_2 <- lmer(degreeRelative ~ age_group*PC1 + age_group*PC2 + season*PC1 + season*PC2 + age_group + season + (1|Nili_id) + (1|year), data = flight)
check_model(fd_2)
summary(fd_2)

# Remove interaction between season and PC2
fd_3 <- lmer(degreeRelative ~ age_group*PC1 + age_group*PC2 + season*PC1 + age_group + season + (1|Nili_id) + (1|year), data = flight)
check_model(fd_3)
summary(fd_3)

fd_comp <- compare_performance(fd_max, fd_2, fd_3, fd_4, fd_5, fd_7, rank = TRUE, verbose = FALSE)
fd_comp
summary(fd_4)

# FLIGHT: RELATIVE STRENGTH

# include everything, with interactions between plausible things to interact
fs_max <- lmer(strengthRelative ~ age_group*PC1 + age_group*PC2 + age_group*season + season*PC1 + season*PC2 + age_group + season + (1|Nili_id) + (1|year), data = flight)
check_model(fs_max) # beautiful!
summary(fs_max) # oh hello significance!! We love that.

# FLIGHT: STRENGTH BY DEGREE

# include everything, with interactions between plausible things to interact
fb_max <- lmer(sbd ~ age_group*PC1 + age_group*PC2 + age_group*season + season*PC1 + season*PC2 + age_group + season + (1|Nili_id) + (1|year), data = flight)
check_model(fb_max) # also gorgeous!
summary(fb_max) # yay significance!
