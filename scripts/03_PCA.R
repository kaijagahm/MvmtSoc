# Analysis script for movement/social position

# Setup -------------------------------------------------------------------
## Load packages -----------------------------------------------------------
library(vultureUtils)
library(sf)
library(tidyverse)
library(igraph)
library(mapview) # for quick maps
library(factoextra)
library(ade4)

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

# The summer season is mid-May through mid-September, but in 2020 we only have data starting September 4th. So I'm going to exclude summer 2020, since I don't think it's well represented.
movementBehavior <- movementBehavior[seasonNames != "2020_summer"]

# Create a dataset with all seasons combined
allMovementBehavior <- purrr::list_rbind(movementBehavior)
allMovementBehavior$sex[allMovementBehavior$sex == "NA"] <- NA
colSums(is.na(allMovementBehavior))
# Scale relative to all the seasons, not just one season (movementBehaviorScaled has vars scaled per season)
allMovementBehavior_scaledAll <- allMovementBehavior %>%
  mutate(across(-c(Nili_id, seasonUnique, birth_year, sex, dataset), function(x){as.numeric(as.vector(scale(x)))}))
allMovementBehavior_scaledAll <- allMovementBehavior_scaledAll[complete.cases(allMovementBehavior_scaledAll),] # remove a couple NA values
colSums(is.na(allMovementBehavior_scaledAll))

# Multivariate index of movement behavior (PCA) ------------------------------
# Decided to create one single PCA for the full time range, and then apply those loadings to individual seasons as needed. 

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
biplot <- fviz_pca_biplot(pca_all, axes = c(1,2), 
                geom = c("point"), 
                alpha.ind = 0.3,
                # col.ind = "white",
                # col.var = "white",
                repel = T)+
  theme_classic()+
  ggtitle("")+
  # theme(panel.background = element_rect(fill = "#666666"),
  #       plot.background = element_rect(fill = "#666666"),
  #       text = element_text(color = "white", size = 18),
  #       axis.text = element_text(color = "white"))+
  NULL
ggsave(biplot, filename = "fig/biplot.png", width = 6, height = 4)
contrib <- round(pca_all$rotation[,1:3]*100, 2) %>%
  as.data.frame()
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

# Flip PC1 so it's more interpretable
contrib # almost all the PC1 values are negative

linked$PC1 <- linked$PC1*(-1)
linked$PC2 <- linked$PC2*(-1) # flipping PC2 as well so it can be labeled as exploration
save(linked, file = "data/linked.Rda")
contrib$PC1 <- contrib$PC1*(-1)
contrib$PC2 <- contrib$PC2*(-1)
save(contrib, file = "data/derived/contrib.Rda")

# Exploring ---------------------------------------------------------------
# Orr wants to know whether higher tortuosity is correlated to a higher ratio between DDT and DMD. It should be.
linked %>%
  ggplot(aes(x = meanDDT/meanDMD, y = mnTort))+
  geom_point()+
  geom_smooth()+
  theme_classic()+
  facet_wrap(~season) # makes sense! As the DDT/DMD ratio increases, so does tortuosity. As expected. Good thing we fixed the tortuosity.

linked %>% group_by(year, season, type) %>% summarize(n = length(unique(Nili_id))) # looks like the right number of individuals
