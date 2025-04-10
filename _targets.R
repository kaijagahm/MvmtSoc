# This is the targets pipeline script that does all the data cleaning for this project. Running this with tar_make() makes it quicker to modify and re-run parts of the pipeline without having to re-create enormous data objects every time.

library(targets)

# Load packages that the functions here will need in order to complete their tasks
tar_option_set(
  packages = c("vultureUtils", "sf", "tidyverse", "dplyr", "move", "feather", "readxl", "here", "furrr", "future", "ctmm", "purrr", "igraph", "mapview", "adehabitatHR", "sp", "raster", "parallel", "car", "factoextra", "ggfortify", "ggpmisc", "lme4", "ggpubr", "rstatix", "easystats", "performance", "lmerTest", "glmmTMB", "DHARMa", "sjPlot", "broom.mixed", "jtools", "emmeans", "ggeffects", "gtsummary", "ade4", "extrafont", "ggplot2", "ggspatial", "grid", "ggmap", "Polychrome", "ggraph", "tidygraph")
)

# The functions needed for data cleaning are defined in the script(s) in the R/ folder. Let's source all those scripts so we have access to the functions.
lapply(list.files("R", full.names = TRUE), source) 

list(
  # Prepare data
  ## Color palettes needed later for plotting--these are defined in functions.R
  tar_target(cc, get_cc()),
  tar_target(situcolors, get_situcolors(cc)),
  tar_target(seasoncolors, get_seasoncolors(cc)), 
  ## Get movebank credentials, which will allow us to download data from movebank
  tar_target(pw, "credentials/pw.Rda", format = "file"),
  tar_target(loginObject, get_loginObject(pw)),
  ## Download the vulture data from movebank
  tar_target(inpa, get_inpa(loginObject)),
  tar_target(ornitela, get_ornitela(loginObject)),
  ## Join together the inpa and ornitela datases
  tar_target(joined0, join_inpa_ornitela(inpa, ornitela)),
  ## Load the who's who file, which has additional information about the vultures
  tar_target(ww_file, "data/raw/whoswho_vultures_20230920_new.xlsx", format = "file"),
  ## Fix the names
  tar_target(fixed_names, fix_names(joined0, ww_file)),
  ## Remove hospital/invalid periods
  tar_target(removed_periods, remove_periods(ww_file, fixed_names)),
  ## Clean the data with the various steps in the vultureUtils::cleanData function.
  tar_target(cleaned, clean_data(removed_periods)),
  ## Attach age and sex information from the who's who file.
  tar_target(with_age_sex, attach_age_sex(removed_captures, ww_file)),
  ## Mask data with the israel region mask
  tar_target(mask, "data/raw/CutOffRegion.kml", format = "file"),
  tar_target(data_masked, mask_data(with_age_sex, mask)),
  ## Split the data into seasons and extract the season names
  tar_target(seasons_list, split_seasons(data_masked)),
  tar_target(season_names, get_season_names(seasons_list)),
  ## Remove northern birds
  tar_target(removed_northern, remove_northern(seasons_list)),
  ## Remove any vultures that have too low a fix rate for any given season
  tar_target(removed_lfr, remove_lfr(removed_northern)),
  ## If any vultures have too *high* a fix rate, downsample it to every 10 minutes so it's easier to work with.
  tar_target(downsampled_10min_forSocial, downsample_10min(removed_lfr)),
  ## Get roost locations for each vulture on each night.
  tar_target(roosts, get_roosts(downsampled_10min_forSocial)),
  ## Remove nighttime points
  tar_target(removed_nighttime, remove_nighttime(removed_lfr)),
  ## Remove vulture days that don't have enough points (unless battery was full enough all day) 
  tar_target(removed_lowppd, remove_lowppd(removed_nighttime)),
  ## Remove any vultures that don't have enough points in a given season
  tar_target(removed_too_few_days, remove_too_few_days(removed_lowppd)),
  ## Downsample the new dataset to every 10 minutes too, for use in movement calculations
  tar_target(downsampled_10min, downsample_10min(removed_too_few_days)),
  ## AKDE--calculate home ranges. This process takes forever to run!
  tar_target(animals, get_animals(downsampled_10min)),
  tar_target(telems_list, get_telems(downsampled_10min, animals)),
  tar_target(variograms_list, get_variograms(telems_list)),
  tar_target(guesses_list, get_guesses(telems_list)),
  tar_target(fits_list, get_fits(telems_list, guesses_list)),
  tar_target(uds_w, get_uds_w(telems_list, fits_list)),
  tar_target(stats_w_95_df, get_akde_stats(uds_w, 0.95, animals, season_names, telems_list)),
  tar_target(stats_w_50_df, get_akde_stats(uds_w, 0.5, animals, season_names, telems_list)),
  tar_target(sfs_est_centroids, get_akde_centroids(uds_w, season_names, downsampled_10min)),
  ## Get how many days each individual was tracked per season XXX remove?
  tar_target(daysTracked_seasons, get_daystracked(downsampled_10min, season_names)),
  ## Load roost polygons (will use this later for the social networks)
  tar_target(roostPolygons, "data/raw/roosts50_kde95_cutOffRegion.kml", format = "file"),
  ## Convert to sf object
  tar_target(downsampled_10min_sf, convertsf(downsampled_10min)),
  ## Get home range and core area sizes
  tar_target(areas_list, compile_areas(stats_w_95_df, stats_w_50_df)),
  ## Compile and label area data
  tar_target(movementBehavior, compile_movement_behavior(areas_list,  daysTracked_seasons, season_names, downsampled_10min_sf)),
  # Categorize movement (PCAs)
  ## Scale movement behavior
  tar_target(movementBehaviorScaled, scale_movement_behavior(movementBehavior)),
  ## Get demographic information
  tar_target(demo, get_demo(movementBehaviorScaled)),
  ## Calculate space use
  tar_target(space_use, get_space_use(movementBehaviorScaled)),
  tar_target(new_movement_vars, get_new_movement_vars(demo, space_use)),
  tar_target(all_movement_vars, get_all_movement_vars(demo, space_use)),
  
  # Calculate social networks
  tar_target(sfdata, convertsf(downsampled_10min_forSocial)),
  tar_target(flight, get_flight(sfdata, roostPolygons)),
  tar_target(feeding, get_feeding(sfdata, roostPolygons)),
  tar_target(roosting, get_roosting(roosts)),
  tar_target(flightEdges, get_list_element(flight, "edges")),
  tar_target(feedingEdges, get_list_element(feeding, "edges")),
  tar_target(roostingEdges, get_list_element(roosting, "edges")),
  tar_target(flightSRI, get_list_element(flight, "sri")),
  tar_target(feedingSRI, get_list_element(feeding, "sri")),
  tar_target(roostingSRI, get_list_element(roosting, "sri")),
  tar_target(flightGraphs, get_graphs(flightSRI)),
  tar_target(feedingGraphs, get_graphs(feedingSRI)),
  tar_target(roostingGraphs, get_graphs(roostingSRI)),
  tar_target(networkMetrics, get_network_metrics(flightGraphs, feedingGraphs, roostingGraphs, season_names)),
  # Wrap around method--permutations
  ## Create a column with explicit times (necessary for the wraparound method)
  tar_target(with_times, prepare_times(downsampled_10min_forSocial)),
  tar_target(metrics_wrapped, get_metrics_wrapped(with_times, roosts, season_names, roostPolygons, 100, 1)),
  tar_target(allMetrics, combine_metrics(networkMetrics, metrics_wrapped)),
  tar_target(metrics_summary, get_metrics_summary(allMetrics)),
  tar_target(ns, get_n_in_network(season_names, flightGraphs, feedingGraphs, roostingGraphs)),
  # Data for mixed models
  tar_target(linked, join_movement_soc(new_movement_vars, metrics_summary, season_names, ns))
)