# Analysis script for movement/social position

# Setup -------------------------------------------------------------------
## Load packages -----------------------------------------------------------
library(vultureUtils)
library(sf)
library(tidyverse)
library(igraph)
library(mapview) # for quick maps
library(factoextra)

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
colSums(is.na(allMovementBehavior))
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
# uniqueRoosts [keep]

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
# uniqueRoosts [roosts]
# meanDFD [movement]
# meanDMD [movement]
# meanDDT [movement]
# mnTort [movement]

forOverallPCA <- allMovementBehavior_scaledAll %>%
  dplyr::select(Nili_id, coreArea, homeRange, coreAreaFidelity, mnDailyMaxAlt, mnDailyMedAlt, propSwitch, shannon, uniqueRoosts, meanDFD, meanDMD, meanDDT, mnTort)

pca_all <- prcomp(x = forOverallPCA[,-1])
pca_all_princomp <- princomp(x = forOverallPCA[,-1])
pca_all_dudi <- dudi.pca(df = forOverallPCA[,-1], nf = 3, scannf = FALSE)
fviz_pca_biplot(pca_all, axes = c(1,2), 
                geom = c("point"), alpha.ind = 0.5)+
  theme_classic()+
  ggtitle("Overall PCA: Sep 2020-Nov 2022") # at some point will want to reverse the direction of the PC1 loadings.
contrib <- round(pca_all$rotation[,1:3]*100, 2) %>%
  as.data.frame() %>% 
  mutate(PC1_rev = -1*PC1) # reversed PC1 so it's not so annoying
contrib

fviz_screeplot(pca_all, addLabels = T)+
  ggtitle("Overall PCA: Sep 2020-Nov 2022")

mvmtPCVals <- allMovementBehavior_scaledAll %>%
  dplyr::select(Nili_id, seasonUnique) %>%
  bind_cols(as.data.frame(pca_all$x[,1:3]))

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
colSums(is.na(linked))

save(linked, file = "data/linked.Rda")