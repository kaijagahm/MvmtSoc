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

## Load data ------------------------------------------------------------
load("data/movementBehavior.Rda")
load("data/movementBehaviorScaled.Rda")

## Multivariate index of movement behavior ------------------------------
## Testing this just on breeding season 2022
movementBehaviorScaled_forPCA <- movementBehaviorScaled %>%
  dplyr::select(-c(mostUsedRoostProp, sdTort, mnDailyMnAlt, sdDMD, meanDFD, coreArea, propSwitch, mnDailyPropOver1km))
M <- cor(movementBehaviorScaled_forPCA[,-1])
corrplot(M, order = "AOE", type = "lower", diag = FALSE)

# Mfull <- cor(movementBehaviorScaled[,-1])
# corrplot(Mfull, order = "AOE", type = "lower", diag = FALSE)

#PCAs
pca_br2022 <- prcomp(x = movementBehaviorScaled_forPCA[,-1])

autoplot(pca_br2022, loadings = T, loadings.label = T)+theme_classic()+ggtitle("Breeding season 2022")

contrib_br2022 <- get_pca_var(pca_br2022)$contrib
fviz_screeplot(pca_br2022, addlabels = T)
contrib_br2022[,1:3]


# Linking movement and social ---------------------------------------------

# Analysis -------------------------------------------------------
## For now, just looking at 2022 breeding season, which is the fourth season out of 6
seasonNames <- map_chr(seasons, ~.x$seasonUnique[1])

# 2022 breeding season
## Daily Max Displacement vs. Degree
all %>%
  filter(timePeriod == "season", value == 2) %>%
  ggplot(aes(x = meanDMD, y = degree, col = type))+
  geom_point()+
  geom_smooth(method = "lm")+
  facet_wrap(~type)+
  ylab("Degree centrality")+
  xlab("Mean daily max displacement")+
  theme_classic()+
  theme(legend.position = "none")

## Daily Max Displacement vs. Strength
all %>%
  filter(timePeriod == "season", value == 2) %>%
  ggplot(aes(x = meanDMD, y = strength, col = type))+
  geom_point()+
  geom_smooth(method = "lm")+
  facet_wrap(~type)+
  ylab("Strength centrality")+
  xlab("Mean daily max displacement")+
  theme_classic()+
  theme(legend.position = "none")

## Daily Max Displacement vs. Strength by Degree
all %>%
  filter(timePeriod == "season", value == 2) %>%
  ggplot(aes(x = meanDMD, y = sbd, col = type))+
  geom_point()+
  geom_smooth(method = "lm")+
  facet_wrap(~type)+
  ylab("Strength by degree")+
  xlab("Mean daily max displacement")+
  theme_classic()+
  theme(legend.position = "none")

## Daily Distance Traveled vs. Degree
all %>%
  filter(timePeriod == "season", value == 2) %>%
  ggplot(aes(x = meanDDT, y = degree, col = type))+
  geom_point()+
  geom_smooth(method = "lm")+
  facet_wrap(~type)+
  ylab("Degree centrality")+
  xlab("Mean daily distance traveled")+
  theme_classic()+
  theme(legend.position = "none")

## Daily Distance Traveled vs. Strength
all %>%
  filter(timePeriod == "season", value == 2) %>%
  ggplot(aes(x = meanDDT, y = strength, col = type))+
  geom_point()+
  geom_smooth(method = "lm")+
  facet_wrap(~type)+
  ylab("Strength centrality")+
  xlab("Mean daily distance traveled")+
  theme_classic()+
  theme(legend.position = "none")

## Daily Distance Traveled vs. Strength by Degree
all %>%
  filter(timePeriod == "season", value == 2) %>%
  ggplot(aes(x = meanDDT, y = sbd, col = type))+
  geom_point()+
  geom_smooth(method = "lm")+
  facet_wrap(~type)+
  ylab("Strength by degree")+
  xlab("Mean daily distance traveled")+
  theme_classic()+
  theme(legend.position = "none")

## Log Home Range Size vs. Degree
all %>%
  filter(timePeriod == "season", value == 2) %>%
  ggplot(aes(x = log(homeRange), y = degree, col = type))+
  geom_point()+
  geom_smooth(method = "lm")+
  facet_wrap(~type, scales = "free_y")+
  ylab("Degree centrality")+
  xlab("Log home range size")+
  theme_classic()+
  theme(legend.position = "none")

## Log Home Range Size vs. Strength
all %>%
  filter(timePeriod == "season", value == 2) %>%
  ggplot(aes(x = log(homeRange), y = strength, col = type))+
  geom_point()+
  geom_smooth(method = "lm")+
  facet_wrap(~type, scales = "free_y")+
  ylab("Strength centrality")+
  xlab("Log home range size")+
  theme_classic()+
  theme(legend.position = "none")

## Log Home Range Size vs. Strength
all %>%
  filter(timePeriod == "season", value == 2) %>%
  ggplot(aes(x = log(homeRange), y = sbd, col = type))+
  geom_point()+
  geom_smooth(method = "lm")+
  facet_wrap(~type, scales = "free_y")+
  ylab("Strength by degree")+
  xlab("Log home range size")+
  theme_classic()+
  theme(legend.position = "none")

## Daily Distance Traveled vs. Degree, 2021 and 2022 breeding seasons
all %>%
  filter(timePeriod == "season", value %in% 1:2) %>%
  ggplot(aes(x = meanDDT, y = degreeRelative, col = factor(value)))+
  geom_point()+
  geom_smooth(method = "lm")+
  scale_color_manual(name = "Year", values = c("red", "blue"))+
  theme_classic()+
  theme(text = element_text(size = 18))+
  facet_wrap(~type, scales = "free_y")+
  ylab("Relative degree")+
  xlab("Mean daily distance traveled")

## Daily Distance Traveled vs. Strength, 2021 and 2022 breeding seasons
all %>%
  filter(timePeriod == "season", value %in% 1:2) %>%
  ggplot(aes(x = meanDDT, y = strengthRelative, col = factor(value)))+
  geom_point()+
  geom_smooth(method = "lm")+
  scale_color_manual(name = "Year", values = c("red", "blue"))+
  theme_classic()+
  theme(text = element_text(size = 18))+
  facet_wrap(~type, scales = "free_y")+
  ylab("Relative strength")+
  xlab("Mean daily distance traveled")

## Daily Distance Traveled vs. Strength-by-degree, 2021 and 2022 breeding seasons
all %>%
  filter(timePeriod == "season", value %in% 1:2) %>%
  ggplot(aes(x = meanDDT, y = sbd, col = factor(value)))+
  geom_point()+
  geom_smooth(method = "lm")+
  scale_color_manual(name = "Year", values = c("red", "blue"))+
  theme_classic()+
  theme(text = element_text(size = 18))+
  facet_wrap(~type, scales = "free_y")+
  ylab("Strength by degree")+
  xlab("Mean daily distance traveled")










all %>%
  filter(timePeriod == "season", value %in% 1:2, type == "feeding") %>%
  ggplot(aes(x = meanDDT, y = degreeRelative, col = factor(value)))+
  geom_point()+
  geom_smooth(method = "lm")+
  scale_color_manual(name = "Year", values = c("red", "blue"))+
  theme_classic()+
  ggtitle("Co-feeding (breeding)")+
  theme(text = element_text(size = 18))+
  ylim(c(0, 1))+
  ylab("Relative degree")+
  xlab("Mean daily distance traveled")

all %>%
  filter(timePeriod == "season", value %in% 1:2, type == "flight") %>%
  ggplot(aes(x = meanDDT, y = strengthRelative, col = factor(value)))+
  geom_point()+
  geom_smooth(method = "lm")+
  scale_color_manual(name = "Year", values = c("red", "blue"))+
  theme_classic()+
  ggtitle("Co-flight (breeding)")+
  theme(text = element_text(size = 18))+
  ylab("Relative strength (strength/n)")+
  xlab("Mean daily distance traveled")

all %>%
  filter(timePeriod == "season", value %in% 1:2, type == "feeding") %>%
  ggplot(aes(x = meanDDT, y = strengthRelative, col = factor(value)))+
  geom_point()+
  geom_smooth(method = "lm")+
  scale_color_manual(name = "Year", values = c("red", "blue"))+
  theme_classic()+
  ggtitle("Co-feeding (breeding)")+
  theme(text = element_text(size = 18))+
  ylab("Relative strength (strength/n)")+
  xlab("Mean daily distance traveled")
