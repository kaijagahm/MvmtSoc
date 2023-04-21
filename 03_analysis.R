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

feedingColor = "gold3";
roostingColor = "brown1";
flightColor = "skyblue3";

## Load data ------------------------------------------------------------
load("data/movementBehavior.Rda")
load("data/movementBehaviorScaled.Rda")
load("data/networkMetrics.Rda")
seasonNames <- map_chr(movementBehavior, ~.x$seasonUnique[1])
all(networkMetrics$season %in% seasonNames) # check that season names match up--good.

# Create a dataset with all seasons combined
allMovementBehavior <- as.data.frame(data.table::rbindlist(movementBehavior))
# Scale relative to all the seasons, not just one season (movementBehaviorScaled has vars scaled per season)
allMovementBehavior_scaledAll <- allMovementBehavior %>%
  mutate(across(-c(Nili_id, seasonUnique), function(x){as.numeric(as.vector(scale(x)))}))

# Multivariate index of movement behavior ------------------------------
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
  ggtitle("Overall PCA: Dec 2021-Mar 2023")
contrib <- get_pca_var(pca_all)$contrib
fviz_screeplot(pca_all, addLabels = T)+
  ggtitle("Overall PCA: Dec 2021-Mar 2023")

mvmtPCVals <- allMovementBehavior_scaledAll %>%
  dplyr::select(Nili_id, seasonUnique) %>%
  bind_cols(as.data.frame(pca_all$x[,1:3]))

# join the raw movement behavior (just the metrics that we used for the PCA) to the PC scores, in case we want to do individual regressions later.
mvmt <- allMovementBehavior %>%
  dplyr::select(Nili_id, seasonUnique, coreArea, homeRange, coreAreaFidelity, mnDailyMaxAlt, mnDailyMedAlt, propSwitch, shannon, meanDFD, meanDMD, meanDDT, mnTort) %>%
  left_join(mvmtPCVals, by = c("Nili_id", "seasonUnique"))

# COMBINE movement and social data ---------------------------------------------
linked <- networkMetrics %>%
  left_join(mvmt, by = c("Nili_id", "season" = "seasonUnique")) %>%
  mutate(year = factor(as.numeric(str_extract(season, "[0-9]{4}"))),
         bnb = factor(str_extract(season, "[a-z]+")))

# Regressions -------------------------------------------------------------
## Doing this just with the two previous seasons, for now
linked_2seasons <- linked %>%
  filter(season %in% c("2022_b", "2022_nb"))

# PC1
linked_2seasons %>%
  ggplot(aes(x = PC1, y = degreeRelative, col = type, lty = season))+
  geom_point(aes(shape = season))+
  geom_smooth(method = "lm")+
  scale_color_manual(values = c(feedingColor, flightColor, roostingColor))+
  facet_wrap(~type, scales = "free")+
  theme_classic()+
  theme(text = element_text(size = 20))+
  ylab("Relative degree")

linked_2seasons %>%
  ggplot(aes(x = PC1, y = strengthRelative, col = type, lty = season))+
  geom_point(aes(shape = season))+
  geom_smooth(method = "lm")+
  scale_color_manual(values = c(feedingColor, flightColor, roostingColor))+
  facet_wrap(~type, scales = "free")+
  theme_classic()+
  theme(text = element_text(size = 20))+
  ylab("Relative strength")

# PC2
linked_2seasons %>%
  ggplot(aes(x = PC2, y = degreeRelative, col = type, lty = season))+
  geom_point(aes(shape = season))+
  geom_smooth(method = "lm")+
  scale_color_manual(values = c(feedingColor, flightColor, roostingColor))+
  facet_wrap(~type, scales = "free")+
  theme_classic()+
  theme(text = element_text(size = 20))+
  ylab("Relative degree")

linked_2seasons %>%
  ggplot(aes(x = PC2, y = strengthRelative, col = type, lty = season))+
  geom_point(aes(shape = season))+
  geom_smooth(method = "lm")+
  scale_color_manual(values = c(feedingColor, flightColor, roostingColor))+
  facet_wrap(~type, scales = "free")+
  theme_classic()+
  theme(text = element_text(size = 20))+
  ylab("Relative strength")

linked %>%
  ggplot(aes(x = PC1, y = degreeRelative, col = year, lty = season))+
  geom_point(aes(shape = season))+
  geom_smooth(method = "lm")+
  scale_color_manual(values = c("orange2", "firebrick3"))+
  facet_wrap(~type, scales = "free")+
  theme_classic()+
  theme(text = element_text(size = 20))+
  ylab("Relative strength")
