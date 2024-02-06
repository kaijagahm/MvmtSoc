# Testing out ctmm

library(ctmm)
library(tidyverse)
library(furrr)
library(purrr)

## Load data ---------------------------------------------------------------
base::load("data/dataPrep/downsampled_10min.Rda") # data rarefied to 10 minute intervals. Going to use that for everything.

# ctmm_season_test <- downsampled_10min[[2]] %>%
#   dplyr::select("ID" = Nili_id, timestamp, "longitude" = location_long, "latitude" = location_lat)
# ctmm_season_test_animals <- ctmm_season_test %>%
#   group_by(ID) %>%
#   group_split()
# save(ctmm_season_test_animals, file = "data/ctmm_season_test_animals.Rda")
load("data/ctmm_season_test_animals.Rda")
base::load("data/dataPrep/season_names.Rda")

# Testing out AKDE --------------------------------------------------------
animals <- map(downsampled_10min, ~unique(.x$Nili_id))
seasons_split <- map(downsampled_10min, ~.x %>%
                       dplyr::select("ID" = Nili_id,
                                     timestamp,
                                     "longitude" = location_long,
                                     "latitude" = location_lat) %>%
                       group_by(ID) %>%
                       group_split())
telems_list <- map(seasons_split, ~map(.x, as.telemetry))
# check out a few examples
dt.plot(telems_list[[1]][[3]])
dt.plot(telems_list[[1]][[4]])
dt.plot(telems_list[[1]][[2]])# the minimum time interval shouldn't be this small; I'm not sure what's going on. Need to go back and figure out why this didn't get downsampled properly. # XXX WHY ARE THERE SHORT SAMPLING INTERVALS?

# Check the range-residency assumption
variograms_list <- map(telems_list, ~map(.x, variogram, .progress = T))
walk(variograms_list[[1]], plot) # just get a brief glimpse of the different individuals. They generally look okay; there's a fair bit of periodicity that I will have to look into (i.e. does it affect the ability of the package to handle the data?) # XXX DOES PERIODICITY MATTER?
# AAA: from looking at the ctmm google group, I don't think that periodicity matters too much--people seem to see this frequently with animals returning to a roost site.

# XXX QUESTION: what do we do if some of the animals don't seem to be range-resident? Is it okay if they do, in general, reach asymptotes to assume that the other individuals are too? Or should those be excluded? How do you automatically check for an asymptote?
# AAA in the ctmm google group, it seems like people check for range-residency and dispersal movements for every animal. I don't think that is practical here, and I could go down basically an infinite rabbit hole trying to do that, so since most of the animals are range resident I am going to assume they all are, generally, and hope that variation in home range sizes basically gets washed out.

# Selecting the best-fit movement model through model selection
guesses_list <- map(telems_list, ~map(.x, ~ctmm.guess(.x, interactive = FALSE), .progress = T))

# Now we do the fits, which I suspect will take a long time. I'm going to do these in a for loop so they will save along the way.
fits_list <- vector(mode = "list", length = length(telems_list))
for(i in 1:length(fits_list)){
  cat("processing season", i, "\n")
  fits <- map2(.x = telems_list[[i]], .y = guesses_list[[i]], ~ctmm.select(.x, .y, method = "pHREML"), .progress = TRUE)
  fits_list[[i]] <- fits
}
save(fits_list, file = "data/fits_list.Rda")

# XXX start here
# Feed the fitted movement models into the home range estimator
dt <- 10 %#% 'min'
uds_uw <- map2(telems, fits, ~akde(.x, .y, dt = dt), .progress = T)
uds_w <- map2(telems, fits, ~akde(.x, .y, dt = dt, weights = TRUE), .progress = T)

uw_stats <- map(uds_uw, ~summary(.x)$CI %>% as.data.frame()) %>% purrr::list_rbind() %>%
  bind_cols(map(uds_uw, ~summary(.x)$DOF %>% t() %>% as.data.frame()) %>% purrr::list_rbind()) %>%
  mutate(ID = animals) %>% relocate(ID) %>% rename("n_eff_area" = "area",
                                                   "dof_bandwidth" = "bandwidth") %>%
  mutate(Weighted = F)
row.names(uw_stats) <- NULL

w_stats <- map(uds_w, ~summary(.x)$CI %>% as.data.frame()) %>% purrr::list_rbind() %>%
  bind_cols(map(uds_w, ~summary(.x)$DOF %>% t() %>% as.data.frame()) %>% purrr::list_rbind()) %>%
  mutate(ID = animals) %>% relocate(ID) %>% rename("n_eff_area" = "area",
                                                   "dof_bandwidth" = "bandwidth") %>%
  mutate(Weighted = T)
row.names(w_stats) <- NULL

stats <- bind_rows(uw_stats, w_stats) %>%
  mutate(n_abs_area = rep(map_dbl(telems, nrow), 2)) %>%
  mutate(eff_ss_prop = round(n_eff_area/n_abs_area, 2))
  
stats %>%
  mutate(ID = factor(ID),
         ID = forcats::fct_reorder(ID, eff_ss_prop)) %>%
  ggplot(aes(x = ID, y = est, col = Weighted))+
  geom_point(position = position_dodge(width = 0.4))+
  theme_minimal()+
  geom_errorbar(aes(x = ID, ymin = low, ymax = high),
                position = position_dodge(width = 0.4),
                width = 0)+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())+
  ylab("Home range (km^2)")+
  xlab("Individual")




# XXX start here
# Evaluating additional biases, applying mitigation measures
summary(ud0_ml)$DOF["area"] # effective sample size of animal
nrow(telem) # absolute sample size
# if the fix rate is not constant, then the animal is well suited to weighted akde
ud0w_ml <- akde(telem, fit0_ml, weights = TRUE)
summary(ud0w_ml)$CI # home range area estimation (weighted)

# Plot home range estimates (weighted and unweighted)
ext <- extent(list(ud0_ml, ud0w_ml), level = 0.95)

# plotting ml with and without weights side by side
par(mfrow = c(1, 2))
plot(telem, ud = ud0_ml, ext = ext)
title(expression("ML AKDE"["C"]))
plot(telem, ud = ud0w_ml, ext = ext)
title(expression("ML wAKDE"["C"]))