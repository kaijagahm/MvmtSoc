# Making AKDE home ranges using the ctmm package
library(ctmm)
library(tidyverse)
library(furrr)
library(purrr)
library(future)

## Load data ---------------------------------------------------------------
base::load("data/dataPrep/downsampled_10min.Rda") # data rarefied to 10 minute intervals. Going to use that for everything.

# ctmm_season_test <- downsampled_10min[[2]] %>%
#   dplyr::select("ID" = Nili_id, timestamp, "longitude" = location_long, "latitude" = location_lat)
# ctmm_season_test_animals <- ctmm_season_test %>%
#   group_by(ID) %>%
#   group_split()
# save(ctmm_season_test_animals, file = "data/ctmm_season_test_animals.Rda")
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
save(telems_list, file = "data/telems_list.Rda")
# check out a few examples
dt.plot(telems_list[[1]][[3]])
dt.plot(telems_list[[1]][[4]])
dt.plot(telems_list[[1]][[2]])# Because I subsampled instead of aggregating, we end up with some short time intervals (e.g. if the point kept for time interval A was at the end of the interval and the point kept for time interval B was at the beginning of the interval, you can have a <10min difference between them.)

# Check the range-residency assumption
variograms_list <- map(telems_list, ~map(.x, variogram, .progress = T))
save(variograms_list, file = "data/akde/variograms_list.Rda")
load("data/akde/variograms_list.Rda")
walk(variograms_list[[1]], plot) # just get a brief glimpse of the different individuals. They generally look okay; there's a fair bit of periodicity that I will have to look into (i.e. does it affect the ability of the package to handle the data?) # XXX DOES PERIODICITY MATTER?
# AAA: from looking at the ctmm google group, I don't think that periodicity matters too much--people seem to see this frequently with animals returning to a roost site.

# XXX QUESTION: what do we do if some of the animals don't seem to be range-resident? Is it okay if they do, in general, reach asymptotes to assume that the other individuals are too? Or should those be excluded? How do you automatically check for an asymptote?
# AAA in the ctmm google group, it seems like people check for range-residency and dispersal movements for every animal. I don't think that is practical here, and I could go down basically an infinite rabbit hole trying to do that, so since most of the animals are range resident I am going to assume they all are, generally, and hope that variation in home range sizes basically gets washed out.

# Selecting the best-fit movement model through model selection
guesses_list <- map(telems_list, ~map(.x, ~ctmm.guess(.x, interactive = FALSE), .progress = T))
save(guesses_list, file = "data/akde/guesses_list.Rda")
load("data/akde/guesses_list.Rda")

# Now we do the fits, which I suspect will take a long time. I'm going to do these in a for loop so they will save along the way.
# for(i in 1:length(telems_list)){
#   cat("processing season", i, "\n")
#   fits <- map2(.x = telems_list[[i]], .y = guesses_list[[i]], ~ctmm.select(.x, .y, method = "pHREML"), .progress = TRUE)
#   filename <- paste0("data/akde/pHREML_fits_", season_names[i], ".Rda")
#   save(fits, file = filename)
# }
# Load each of the fits objects in. Because I stupidly saved them all as "fits", need to rename them after loading them in.
load("data/akde/pHREML_fits_2020_fall.Rda")
fits_2020_fall <- fits
load("data/akde/pHREML_fits_2021_breeding.Rda")
fits_2021_breeding <- fits
load("data/akde/pHREML_fits_2021_summer.Rda")
fits_2021_summer <- fits
load("data/akde/pHREML_fits_2021_fall.Rda")
fits_2021_fall <- fits
load("data/akde/pHREML_fits_2022_breeding.Rda")
fits_2022_breeding <- fits
load("data/akde/pHREML_fits_2022_summer.Rda")
fits_2022_summer <- fits
load("data/akde/pHREML_fits_2022_fall.Rda")
fits_2022_fall <- fits
load("data/akde/pHREML_fits_2023_breeding.Rda")
fits_2023_breeding <- fits
load("data/akde/pHREML_fits_2023_summer.Rda")
fits_2023_summer <- fits

fits_list <- list(fits_2020_fall, fits_2021_breeding, fits_2021_summer, fits_2021_fall, fits_2022_breeding, fits_2022_summer, fits_2022_fall, fits_2023_breeding, fits_2023_summer)
names(fits_list) <- season_names

# Now get the uds for each of these, which will also take a long time. We're going to use weighted akdes, even though they take longer to run, because we have variable sampling rates.
dt <- 10 %#% "min" # set the time difference for the uds
future::plan(future::multisession, workers = 15)
tictoc::tic()
uds_w <- vector(mode = "list", length = length(fits_list))
for(i in 1:length(fits_list)){
  uds_w[[i]] <- furrr::future_map2(telems_list[[i]], fits_list[[i]],
                                   ~akde(.x, .y, dt = dt, weights = T),
                                   .progress = T)
}
tictoc::toc()
save(uds_w, file = "data/akde/uds_w.Rda")

stats_w_95 <- vector(mode = "list", length = length(uds_w))
for(i in 1:length(stats_w_95)){
  stats_weighted <- map(uds_w[[i]],
                          ~summary(.x, level = 0.95, level.UD = 0.95)$CI %>% as.data.frame()) %>%
    purrr::list_rbind() %>%
    bind_cols(map(uds_w[[i]],
                  ~summary(.x, level = 0.95, level.UD = 0.95)$DOF %>% t() %>% as.data.frame()) %>%
                purrr::list_rbind()) %>%
    mutate(ID = animals[[i]],
           weighted = T) %>%
    rename("n_eff_area" = "area",
           "dof_bandwidth" = "bandwidth") %>%
    mutate(n_abs_area = map_dbl(telems_list[[i]], nrow),
           eff_ss_prop = round(n_eff_area/n_abs_area, 2),
           seasonUnique = season_names[i])
  stats_w_95[[i]] <- stats_weighted
}
stats_w_95_df <- purrr::list_rbind(stats_w_95) %>% mutate(level = 0.95)
save(stats_w_95_df, file = "data/akde/stats_w_95_df.Rda")
row.names(stats_w_95_df) <- NULL

stats_w_50 <- vector(mode = "list", length = length(uds_w))
for(i in 1:length(stats_w_50)){
  stats_weighted <- map(uds_w[[i]],
                        ~summary(.x, level = 0.95, level.UD = 0.50)$CI %>% as.data.frame()) %>%
    purrr::list_rbind() %>%
    bind_cols(map(uds_w[[i]],
                  ~summary(.x, level = 0.95, level.UD = 0.50)$DOF %>% t() %>% as.data.frame()) %>%
                purrr::list_rbind()) %>%
    mutate(ID = animals[[i]],
           weighted = T) %>%
    rename("n_eff_area" = "area",
           "dof_bandwidth" = "bandwidth") %>%
    mutate(n_abs_area = map_dbl(telems_list[[i]], nrow),
           eff_ss_prop = round(n_eff_area/n_abs_area, 2),
           seasonUnique = season_names[i])
  stats_w_50[[i]] <- stats_weighted
}
stats_w_50_df <- purrr::list_rbind(stats_w_50) %>% mutate(level = 0.5)
save(stats_w_50_df, file = "data/akde/stats_w_50_df.Rda")
row.names(stats_w_50_df) <- NULL

plothr <- function(season, animal, level = 0.95){
  plot(telems_list[[season]][[animal]], UD = uds_w[[season]][[animal]], level.UD = level)
  season_name <- season_names[season]
  title(paste(stringr::str_to_title(animals[[season]][animal]), season_name))
}
