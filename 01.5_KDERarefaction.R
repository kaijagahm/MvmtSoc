# Side quest: rarefaction plots for the KDE's
library(tidyverse)
library(adehabitatHR)
library(sf)
library(sp)
library(raster)

load("data/hrList_indivs.Rda")

# For starters, let's see what href values we get when we run home range KUDs naively
getH <- function(df){
  sp <- SpatialPoints(df[,c("x", "y")])
  k <- kernelUD(sp, h = "href", grid = 100, extent = 1)
  h <- k@h$h
}

hs <- map_dbl(hrList_indivs[[2]], getH)
hist(hs) # looks like our mean is a little over 5000. That gives me an idea of what ballpark to be in.
summary(hs)

# Some sample plots to visualize data
## test individual
df <- hrList_indivs[[2]][[25]]
sp <- SpatialPoints(df[,c("x", "y")])
k <- kernelUD(sp, h = "href", grid = 100, extent = 1)
j <- kernelUD(sp, h = 10000, grid = 100, extent = 1)

image(k)
poly <- getverticeshr(k, percent = 95) 
plot(poly, add = TRUE) # for some reason it won't let me plot this. What's going on?
plot(sp, add = TRUE, cex = 0.01) # with image, this looks like a really bad representation, but with the 95% kernel it's a lot better.

polyj <- getverticeshr(j, percent = 95)
image(j)
plot(polyj, add = T)
plot(sp, add = TRUE, cex = 0.01)

# okay, this is always going to be somewhat arbitrary. But maybe let's just go with 5000 for now?

### XXX MARTA'S SUBSAMPLING CODE
AllGV <- AllGV[order(AllGV$Nili_id, AllGV$timestamp),]

AllGV$burst <- seq(from = 1, to = nrow(AllGV))

temp.dif <- difftime(AllGV$timestamp[2:nrow(AllGV)], AllGV$timestamp[(1:(nrow(AllGV)-1))], units = "mins")
AllGV$dif <- c(1, temp.dif)
AllGV$dif <- if_else(duplicated(as.character(AllGV$Nili_id))==FALSE, 1, AllGV$dif)

AllGV.10min = AllGV[1,]

for(i in 1:(nrow(AllGV)-1)) {
  if(AllGV$burst[i] <= tail(AllGV.10min$burst, n=1)) {next}
  print(i)
  if(AllGV$dif[i] > 9){
    AllGV.10min <-  rbind(AllGV.10min, AllGV[i,])
  } else {
    ix <- length(which(cumsum(AllGV$dif[i:nrow(AllGV)]) <= 9))
    AllGV.10min <-  rbind(AllGV.10min , AllGV[i+ix,])
    if (sum(AllGV$dif[i:nrow(AllGV)]) <= 9) { break }
  }
}


## Rarefaction test: random subsampling at different levels
## generate data
inds <- hrList_indivs[[2]][1:10] # just use one season for this, and reduce to 10 individuals for speed
props <- seq(from = 1, to = 0.1, by = -0.2)
reps <- 25 # reduced from 50, for speed

indsData <- vector(mode = "list", length = length(inds))
for(i in 1:length(inds)){
  cat("individual", i, "\n")
  ind <- inds[[i]]
  propsData <- vector(mode = "list", length = length(props))
  for(p in 1:length(props)){
    prop <- props[p]
    repsList <- vector(mode = "list", length = reps)
    for(r in 1:reps){
      subsampledData <- slice_sample(ind, prop = prop)
      repsList[[r]] <- subsampledData
    }
    propsData[[p]] <- repsList
  }
  indsData[[i]] <- propsData
}

# Now we have a data structure: 69 individuals, each of which has 5 lists (different proportions), each of which has 50 replicates at that proportion level.
length(indsData) # 69
length(indsData[[1]]) # 10
length(indsData[[1]][[1]]) # 50

# Now, calculate the observed 95% home ranges
sps <- map(inds, ~SpatialPoints(.x[,c("x", "y")]))
kObs <- map(sps, ~kernelUD(.x, h = 5000, grid = 100, extent = 1))
obsHRs_95 <- map_dbl(kObs, ~kernel.area(.x, percent = 95))

# Function to calculate home range
calcHR <- function(df, pct){
  sp <- SpatialPoints(df[,c("x", "y")])
  k <- kernelUD(sp, h = 5000, grid = 100, extent = 1)
  hr95 <- kernel.area(k, percent = pct)
  return(hr95)
}

#Use map to apply that function to the crazy complex data structure
indsHRSizes_95 <- map(indsData, ~map_dfc(.x, ~map_dbl(.x, ~calcHR(.x, pct = 95)),
                                      .progress = T),
                   .progress = T)

indsHRSizes_50 <- map(indsData, ~map_dfc(.x, ~map_dbl(.x, ~calcHR(.x, pct = 50)),
                                         .progress = T),
                      .progress = T)

save(indsHRSizes_95, file = "data/indsHRSizes_95.Rda")
save(indsHRSizes_50, file = "data/indsHRSizes_50.Rda")
load("data/indsHRSizes_95.Rda")
load("data/indsHRSizes_50.Rda")

areaData_95 <- map2(.x = indsHRSizes_95, .y = map_chr(inds, ~.x$Nili_id[1]), ~{
  df <- .x
  names(df) <- paste0("prop_", as.character(props))
  df$Nili_id <- .y
  return(df)
}) %>% data.table::rbindlist() %>% 
  as.data.frame() %>%
  mutate(rep = 1:nrow(.)) %>%
  pivot_longer(cols = -c("Nili_id", "rep"), names_to = "prop", values_to = "area") %>%
  mutate(prop = as.numeric(str_remove(prop, "prop_")),
         hrProp = 0.95)

areaData_50 <- map2(.x = indsHRSizes_50, .y = map_chr(inds, ~.x$Nili_id[1]), ~{
  df <- .x
  names(df) <- paste0("prop_", as.character(props))
  df$Nili_id <- .y
  return(df)
}) %>% data.table::rbindlist() %>% 
  as.data.frame() %>%
  mutate(rep = 1:nrow(.)) %>%
  pivot_longer(cols = -c("Nili_id", "rep"), names_to = "prop", values_to = "area") %>%
  mutate(prop = as.numeric(str_remove(prop, "prop_")),
         hrProp = 0.50)

areaData <- bind_rows(areaData_95, areaData_50)

# Summarize the data for each individual
areaData <- areaData %>%
  group_by(Nili_id, hrProp, rep) %>%
  mutate(prop_of_1 = area/area[1]) %>%
  ungroup()
rarefactionSummary <- areaData %>%
  group_by(Nili_id, prop) %>%
  mutate(mnProp_of_1 = mean(prop_of_1)) %>%
  ungroup()

# rarefaction curves
areaData %>%
  ggplot(aes(x = jitter(prop), y = prop_of_1, group = Nili_id))+
  geom_point(alpha = 0.1)+
  geom_smooth(se = F, linewidth = 0.5)+
  theme_classic()+
  ylab("HR size (vs. with 100% of points)")+
  xlab("Proportion of points retained")+
  geom_hline(aes(yintercept = 1), lty = 2)+
  facet_wrap(~hrProp) # interesting! This looks fairly unbiased, though it varies a lot for different individuals. Looks like if we take 50-100% of the data, we get approximately the same home range as if we use 100% of the data, and the variability lessens. 

# Looking at absolute instead of relative sizes:
areaData %>%
  filter(hrProp == 0.95) %>%
  ggplot(aes(x = prop, y = area, group = Nili_id))+
  geom_point(alpha = 0.1)+
  geom_smooth(se = T, linewidth = 0.5)+
  theme_classic()+
  ylab("HR size")+
  ggtitle("95% hr")+
  xlab("Proportion of points retained")+
  facet_wrap(~Nili_id, scales = "free_y") # no consistent patterns here, though note that a bit of weirdness is created by having so many points at 100% of the data (since they will all be the same, by definition)

areaData %>%
  filter(hrProp == 0.50) %>%
  ggplot(aes(x = prop, y = area, group = Nili_id))+
  geom_point(alpha = 0.1)+
  geom_smooth(se = T, linewidth = 0.5)+
  theme_classic()+
  ylab("HR size")+
  ggtitle("50% hr")+
  xlab("Proportion of points retained")+
  facet_wrap(~Nili_id, scales = "free_y")

# I think I'm going to go ahead and use a consistent parameter of h = 5000 across individuals, but then try this rarefaction again with downsampling the data frequency after I get Marta's code for that.