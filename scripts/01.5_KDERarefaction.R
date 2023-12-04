# Side quest: rarefaction plots for the KDE's
library(tidyverse)
library(adehabitatHR)
library(sf)
library(sp)
library(raster)

load("data/hrList_indivs.Rda")
load("data/seasons.Rda")
s <- map_chr(seasons, ~.x$seasonUnique[1])

# For starters, let's see what href values we get when we run home range KUDs naively
getH <- function(df){
  sp <- SpatialPoints(df[,c("x", "y")])
  k <- kernelUD(sp, h = "href", grid = 100, extent = 1)
  h <- k@h$h
}

hs <- map_dbl(hrList_indivs[[3]], getH)
hist(hs) # looks like our mean is a little over 5000. That gives me an idea of what ballpark to be in.
summary(hs)

# Some sample plots to visualize data
## test one individual
df <- hrList_indivs[[2]][[25]]
sp <- SpatialPoints(df[,c("x", "y")])
k <- kernelUD(sp, h = "href", grid = 100, extent = 1)
j <- kernelUD(sp, h = 10000, grid = 100, extent = 1)

image(k)
poly <- getverticeshr(k, percent = 95) 
plot(poly, add = TRUE)
plot(sp, add = TRUE, cex = 0.01) # with image, this looks like a really bad representation, but with the 95% kernel it's a lot better.

# okay, this is always going to be somewhat arbitrary. But maybe let's just go with 4000 for now?
load("data/seasons_10min.Rda")
load("data/seasons_20min.Rda")
load("data/seasons_30min.Rda")
load("data/seasons_60min.Rda")
load("data/seasons_120min.Rda")

# Function to calculate home range size
calcHR <- function(df, pct, h = 4000, idCol = "Nili_id"){
  id <- df[[idCol]][1]
  sp <- SpatialPoints(df[,c("x", "y")])
  k <- kernelUD(sp, h = h, grid = 100, extent = 1)
  hr <- kernel.area(k, percent = pct)
  n <- nrow(df)
  return(data.frame(id = id, n = n, pct = pct, area = hr))
}

getHRs <- function(list){
  cat("Preparing data\n")
  indivs <- map(list, ~.x %>%
                  dplyr::select(Nili_id) %>%
                  st_transform(32636) %>%
                  mutate(x = st_coordinates(.)[,1], 
                         y = st_coordinates(.)[,2]) %>%
                  st_drop_geometry() %>%
                  group_by(Nili_id) %>%
                  group_split(.keep = T))
  cat("Calculating 95% home ranges\n")
  hr95 <- map(indivs, 
              ~map_dfr(.x, ~calcHR(.x, pct = 95), .progress = T)) %>%
    map2_dfr(., .y = s, ~.x %>% mutate(seasonUnique = .y))
  cat("Calculating 50% home ranges\n")
  hr50 <- map(indivs, 
              ~map_dfr(.x, ~calcHR(.x, pct = 50), .progress = T)) %>%
    map2_dfr(., .y = s, ~.x %>% mutate(seasonUnique = .y))
  
  out <- bind_rows(hr95, hr50)
  return(out)
}

# none <- getHRs(list = seasons) %>% mutate(rarefaction = "none")
# r10min <- getHRs(list = seasons_10min) %>% mutate(rarefaction = "10min")
# r20min <- getHRs(list = seasons_20min) %>% mutate(rarefaction = "20min")
# r30min <- getHRs(list = seasons_30min) %>% mutate(rarefaction = "30min")
# r60min <- getHRs(list = seasons_60min) %>% mutate(rarefaction = "60min")
# r120min <- getHRs(list = seasons_120min) %>% mutate(rarefaction = "120min")
# 
# allHRs <- bind_rows(none, r10min, r20min, r30min, r60min, r120min) %>%
#   mutate(rarefaction = factor(rarefaction, levels = c("120min", "60min", "30min", "20min", "10min", "none"))) %>%
#   mutate(areaRel = area/n) %>%
#   group_by(seasonUnique, id, pct) %>%
#   arrange(desc(rarefaction), .by_group = T) %>%
#   mutate(areaVsAll = area/area[1],
#          areaRelVsAll = areaRel/areaRel[1])
# save(allHRs, file = "data/allHRs.Rda")
load("data/allHRs.Rda")

(kdeRarefactions <- allHRs %>% 
  filter(areaVsAll < 1.5, areaVsAll > 0.6) %>% # eliminate some outliers
  ggplot(aes(x = rarefaction, y = areaVsAll, fill = factor(pct)))+
  geom_boxplot()+
  theme_classic()+
  scale_fill_manual(values = c("blue3", "darkorange2"))+
  theme(legend.position = "none")+
  facet_grid(rows = vars(pct), cols = vars(seasonUnique), scales = "free")+
  ylab("Area (rarefied vs. all pts)")+
  xlab("Rarefaction level")+
  theme(text = element_text(size = 18)))
ggsave(kdeRarefactions, file = "fig/kdeRarefactions.png", width = 13, height = 7, dpi = 800)

# does number of points affect things?
allHRs %>%
  group_by(seasonUnique, rarefaction, pct) %>%
  mutate(nPts_scaled = scale(n)) %>%
  filter(area < 10000000, rarefaction %in% c("10min", "30min", "120min")) %>%
  filter(pct == 95) %>%
  ggplot(aes(x = nPts_scaled, y = area, col = rarefaction))+
  geom_point()+
  geom_smooth(method = "lm")+
  theme_minimal()+
  facet_grid(rows = vars(rarefaction), cols = vars(seasonUnique))
# no matter what rarefaction level we use, the number of points itself doesn't seem to really affect the KDE area. This is good! Means we don't actually have to scale area by number of points.

# Any difference when we look at core areas (50%) instead of home ranges (95%)?
allHRs %>% 
  filter(pct == 50) %>%
  filter(areaVsAll < 1.5, areaVsAll > 0.6) %>%
  ggplot(aes(x = rarefaction, y = areaVsAll))+
  geom_line(aes(group = id))+
  theme_classic()+
  theme(legend.position = "none")+
  facet_wrap(~seasonUnique)

allHRs %>%
  group_by(seasonUnique, rarefaction, pct) %>%
  mutate(nPts_scaled = scale(n)) %>%
  filter(area < 10000000, rarefaction %in% c("10min", "30min", "120min")) %>%
  filter(pct == 50) %>%
  ggplot(aes(x = nPts_scaled, y = area, col = rarefaction))+
  geom_point()+
  geom_smooth(method = "lm")+
  theme_minimal()+
  facet_grid(rows = vars(rarefaction), cols = vars(seasonUnique))

# Using h = 4000 vs the automatic h value doesn't seem to make a big difference.
