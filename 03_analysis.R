# Analysis script for movement/social position

# Setup -------------------------------------------------------------------
## Load packages -----------------------------------------------------------
library(vultureUtils)
library(sf)
library(tidyverse)
library(igraph)
library(mapview) # for quick maps
library(factoextra)
library(corrplot)
library(ggfortify)
library(ggpmisc) # for more details on a linear regression plot
library(lme4) # for mixed-effects models
library(ggpubr) # for adding p-values to boxplots
library(rstatix) # for adding p-values to boxplots
library(easystats) # for modeling and comparisons
library(performance) # for modeling and comparisons
library(lmerTest)

feedingColor = "gold3";
roostingColor = "brown1";
flightColor = "skyblue3";

## Load data ------------------------------------------------------------
load("data/movementBehavior.Rda")
load("data/movementBehaviorScaled.Rda")
load("data/networkMetrics.Rda")
seasonNames <- map_chr(movementBehavior, ~.x$seasonUnique[1])
all(networkMetrics$season %in% seasonNames) # check that season names match up--good.
load("data/seasons_10min.Rda")

# Create a dataset with all seasons combined
allMovementBehavior <- purrr::list_rbind(movementBehavior)
# Scale relative to all the seasons, not just one season (movementBehaviorScaled has vars scaled per season)
allMovementBehavior_scaledAll <- allMovementBehavior %>%
  mutate(across(-c(Nili_id, seasonUnique, birth_year, sex), function(x){as.numeric(as.vector(scale(x)))}))

# Multivariate index of movement behavior (PCA) ------------------------------
# Decided to create one single PCA for the full time range, and then apply those loadings to individual seasons as needed. Currently, the time range is from "2021-12-01 00:00" to "2023-03-31 11:59".

# Variables available:
glimpse(movementBehavior)
# HR
# coreArea [keep]
# homeRange [keep]
# coreAreaFidelity [keep]

# Altitude
# mnDailyMaxAlt [keep]
# mnDailyMnAlt [remove -- redundant with mnDailyMedAlt]
# mnDailyPropOver1km [remove--redundant with mnDailyMaxAlt]
# mnDailyMedAlt [keep, but need justification for why we're keeping means for other traits but median here]

# Roosts
# propSwitch [keep]
# shannon [keep]
# mostUsedRoostProp [remove--redundant with Shannon]

# Flight duration
# meanDFD
# minDFD [remove to be consistent with movement metrics]
# maxDFD [remove to be consistent with movement metrics]
# medDFD [remove to be consistent with movement metrics]

# Movement
# meanDMD
# meanDDT
# mnTort

# Variables to use in the PCA
# coreArea [HR]
# homeRange [HR]
# coreAreaFidelity [HR]
# mnDailyMaxAlt [altitude]
# mnDailyMedAlt [altitude]
# propSwitch [roosts]
# shannon [roosts]
# meanDFD [movement]
# meanDMD [movement]
# meanDDT [movement]
# mnTort [movement]

forOverallPCA <- allMovementBehavior_scaledAll %>%
  dplyr::select(Nili_id, coreArea, homeRange, coreAreaFidelity, mnDailyMaxAlt, mnDailyMedAlt, propSwitch, shannon, meanDFD, meanDMD, meanDDT, mnTort)

pca_all <- prcomp(x = forOverallPCA[,-1])
autoplot(pca_all, loadings = T, loadings.label = T)+
  theme_classic()+
  ggtitle("Overall PCA: Dec 2021-Mar 2023") # figure out how to make an autoplot of PC1 vs. PC3 with arrows. 
contrib <- get_pca_var(pca_all)$contrib
fviz_screeplot(pca_all, addLabels = T)+
  ggtitle("Overall PCA: Dec 2021-Mar 2023")

mvmtPCVals <- allMovementBehavior_scaledAll %>%
  dplyr::select(Nili_id, seasonUnique) %>%
  bind_cols(as.data.frame(pca_all$x[,1:3]))
round(contrib[,1:3], 3) # these should sometimes be negative; what's going on?
# try another package and see if it gives the same results.

# Interpretation of PC contributions
# PC1
# PC2: high altitudes and large 50/95
# PC3: large home range, large 50/95, large tortuosity

# join the raw movement behavior (just the metrics that we used for the PCA) to the PC scores, in case we want to do individual regressions later.
mvmt <- allMovementBehavior %>%
  dplyr::select(Nili_id, birth_year, sex, seasonUnique, coreArea, homeRange, coreAreaFidelity, mnDailyMaxAlt, mnDailyMedAlt, propSwitch, shannon, meanDFD, meanDMD, meanDDT, mnTort) %>%
  left_join(mvmtPCVals, by = c("Nili_id", "seasonUnique"))

# COMBINE movement and social data ---------------------------------------------
linked <- networkMetrics %>%
  left_join(mvmt, by = c("Nili_id", "season" = "seasonUnique")) %>%
  mutate(year = factor(as.numeric(str_extract(season, "[0-9]{4}"))),
         bnb = factor(str_extract(season, "[a-z]+")),
         age = as.numeric(as.character(year)) - birth_year) %>%
  mutate(age_group = case_when(age < 5 ~ "juv_sub", age >= 5 ~ "adult", TRUE ~ NA)) %>%
  mutate(age_group = factor(age_group, levels = c("juv_sub", "adult")),
         year = factor(str_extract(season, "[0-9]+")),
         season = str_extract(season, "[a-z]+"))
linked$sex[linked$sex == "NA"] <- NA


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

# FLIGHT: RELATIVE DEGREE

# include everything, with interactions between age and sex, season and year
fd_max <- lmer(degreeRelative ~ PC1 + PC2 + age_group*sex + season*year + (1|Nili_id), 
               data = flight)
check_model(fd_max)

# remove interaction between age and sex
fd_2 <- lmer(degreeRelative ~ PC1 + PC2 + age_group + sex + season*year + (1|Nili_id), 
               data = flight)
check_model(fd_2) # also good, but still have a few outliers. 

# remove interaction between season and year
fd_3 <- lmer(degreeRelative ~ PC1 + PC2 + age_group + sex + season + year + (1|Nili_id), 
             data = flight)
check_model(fd_3) # no apparent difference from the previous one...

# remove sex
fd_4 <- lmer(degreeRelative ~ PC1 + PC2 + age_group + season + year + (1|Nili_id), 
             data = flight)
check_model(fd_4) # this one looks worse than the previous one

# remove year
fd_5 <- lmer(degreeRelative ~ PC1 + PC2 + age_group + season + (1|Nili_id), 
             data = flight)
check_model(fd_5) # we still have some weird stubborn outliers

# remove age
fd_6 <- lmer(degreeRelative ~ PC1 + PC2 + season + (1|Nili_id), 
             data = flight)
check_model(fd_6)

# remove season
fd_7 <- lmer(degreeRelative ~ PC1 + PC2 + (1|Nili_id), 
             data = flight)
check_model(fd_7)

# in all of these models, we see some stubborn outliers. Not sure what's up with that...
fd_comp <- compare_performance(fd_max, fd_2, fd_3, fd_4, fd_5, fd_6, fd_7, rank = TRUE, verbose = FALSE)
summary(fd_4)

# FLIGHT: RELATIVE STRENGTH

# include everything, with interactions between age and sex, season and year
fs_max <- lmer(strengthRelative ~ PC1 + PC2 + age_group*sex + season*year + (1|Nili_id), 
               data = flight)
check_model(fs_max) # oh this looks GOOD!

# remove interaction between age and sex
fs_2 <- lmer(strengthRelative ~ PC1 + PC2 + age_group + sex + season*year + (1|Nili_id), 
             data = flight)
check_model(fs_2)

# remove interaction between season and year
fs_3 <- lmer(strengthRelative ~ PC1 + PC2 + age_group + sex + season + year + (1|Nili_id), 
             data = flight)
check_model(fs_3) 

# remove sex
fs_4 <- lmer(strengthRelative ~ PC1 + PC2 + age_group + season + year + (1|Nili_id), 
             data = flight)
check_model(fs_4)

# remove year
fs_5 <- lmer(strengthRelative ~ PC1 + PC2 + age_group + season + (1|Nili_id), 
             data = flight)
check_model(fs_5) 

# remove age
fs_6 <- lmer(strengthRelative ~ PC1 + PC2 + season + (1|Nili_id), 
             data = flight)
check_model(fs_6)

# remove season
fs_7 <- lmer(strengthRelative ~ PC1 + PC2 + (1|Nili_id), 
             data = flight)
check_model(fs_7)

# in all of these models, we see some stubborn outliers. Not sure what's up with that...
fs_comp <- compare_performance(fs_max, fs_2, fs_3, fs_4, fs_5, fs_6, fs_7, rank = TRUE, verbose = FALSE)
summary(fs_4)

# FLIGHT: STRENGTH BY DEGREE

# include everything, with interactions between age and sex, season and year
fb_max <- lmer(sbd ~ PC1 + PC2 + age_group*sex + season*year + (1|Nili_id), 
               data = flight)
check_model(fb_max) # looks pretty good!

# remove interaction between age and sex
fb_2 <- lmer(sbd ~ PC1 + PC2 + age_group + sex + season*year + (1|Nili_id), 
             data = flight)
check_model(fb_2)

# remove interaction between season and year
fb_3 <- lmer(sbd ~ PC1 + PC2 + age_group + sex + season + year + (1|Nili_id), 
             data = flight)
check_model(fb_3) 

# remove sex
fb_4 <- lmer(sbd ~ PC1 + PC2 + age_group + season + year + (1|Nili_id), 
             data = flight)
check_model(fb_4)

# remove year
fb_5 <- lmer(sbd ~ PC1 + PC2 + age_group + season + (1|Nili_id), 
             data = flight)
check_model(fb_5) 

# remove age
fb_6 <- lmer(sbd ~ PC1 + PC2 + season + (1|Nili_id), 
             data = flight)
check_model(fb_6)

# remove season
fb_7 <- lmer(sbd ~ PC1 + PC2 + (1|Nili_id), 
             data = flight)
check_model(fb_7)

# in all of these models, we see some stubborn outliers. Not sure what's up with that...
fb_comp <- compare_performance(fb_max, fb_2, fb_3, fb_4, fb_5, fb_6, fb_7, rank = TRUE, verbose = FALSE)
summary(fb_4)
