# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline

# Load packages required to define the pipeline:
library(targets)
# library(tarchetypes) # Load other packages as needed.

# Set target options:
tar_option_set(
  packages = c("vultureUtils", "sf", "tidyverse", "move", "feather", "readxl", "elevatr", "here", "furrr", "future", "ctmm", "purrr", "igraph", "mapview", "adehabitatHR", "sp", "raster", "parallel", "car", "factoextra", "ggfortify", "ggpmisc", "lme4", "ggpubr", "rstatix", "easystats", "performance", "lmerTest", "glmmTMB", "DHARMa", "sjPlot", "broom.mixed", "jtools", "emmeans", "ggeffects", "gtsummary", "ade4", "extrafont", "ggplot2", "ggspatial", "grid", "ggmap", "Polychrome", "ggraph", "tidygraph") # Packages that your targets need for their tasks.
  # format = "qs", # Optionally set the default storage format. qs is fast.
  #
  # Pipelines that take a long time to run may benefit from
  # optional distributed computing. To use this capability
  # in tar_make(), supply a {crew} controller
  # as discussed at https://books.ropensci.org/targets/crew.html.
  # Choose a controller that suits your needs. For example, the following
  # sets a controller that scales up to a maximum of two workers
  # which run as local R processes. Each worker launches when there is work
  # to do and exits if 60 seconds pass with no tasks to run.
  #
  #   controller = crew::crew_controller_local(workers = 2, seconds_idle = 60)
  #
  # Alternatively, if you want workers to run on a high-performance computing
  # cluster, select a controller from the {crew.cluster} package.
  # For the cloud, see plugin packages like {crew.aws.batch}.
  # The following example is a controller for Sun Grid Engine (SGE).
  # 
  #   controller = crew.cluster::crew_controller_sge(
  #     # Number of workers that the pipeline can scale up to:
  #     workers = 10,
  #     # It is recommended to set an idle time so workers can shut themselves
  #     # down if they are not running tasks.
  #     seconds_idle = 120,
  #     # Many clusters install R as an environment module, and you can load it
  #     # with the script_lines argument. To select a specific verison of R,
  #     # you may need to include a version string, e.g. "module load R/4.3.2".
  #     # Check with your system administrator if you are unsure.
  #     script_lines = "module load R"
  #   )
  #
  # Set other options as needed.
)

# Run the R scripts in the R/ folder with your custom functions:
lapply(list.files("R", full.names = TRUE), source) # source all scripts in the R directory
# tar_source("other_functions.R") # Source other scripts as needed.

# Replace the target list below with your own:
list(
  # Prepare data
  tar_target(cc, get_cc()),
  tar_target(pw, "movebankCredentials/pw.Rda", format = "file"),
  tar_target(loginObject, get_loginObject(pw)),
  tar_target(inpa, get_inpa(loginObject)),
  tar_target(ornitela, get_ornitela(loginObject)),
  tar_target(joined0, join_inpa_ornitela(inpa, ornitela)),
  tar_target(ww_file, "data/whoswho_vultures_20230920_new.xlsx", format = "file"),
  tar_target(fixed_names, fix_names(joined0, ww_file)),
  tar_target(removed_periods, remove_periods(ww_file, fixed_names)),
  tar_target(cleaned, clean_data(removed_periods)),
  tar_target(capture_sites, "data/capture_sites.csv", format = "file"),
  tar_target(carmel, "data/all_captures_carmel_2010-2021.csv", format = "file"),
  tar_target(removed_captures, remove_captures(capture_sites, carmel, cleaned)),
  tar_target(with_age_sex, attach_age_sex(removed_captures, ww_file)),
  tar_target(mask, "data/CutOffRegion.kml", format = "file"),
  tar_target(data_masked, mask_data(with_age_sex, mask)),
  tar_target(seasons_list, split_seasons(data_masked)),
  tar_target(season_names, get_season_names(seasons_list)),
  tar_target(removed_northern, remove_northern(seasons_list)),
  tar_target(removed_lfr, remove_lfr(removed_northern)),
  tar_target(downsampled_10min_forSocial, downsample_10min(removed_lfr)),
  tar_target(roosts, get_roosts(downsampled_10min_forSocial)),
  tar_target(removed_nighttime, remove_nighttime(removed_lfr)),
  tar_target(removed_lowppd, remove_lowppd(removed_nighttime)),
  tar_target(removed_too_few_days, remove_too_few_days(removed_lowppd)),
  tar_target(with_altitudes, attach_altitudes(removed_too_few_days)),
  tar_target(downsampled_10min, downsample_10min(with_altitudes)),
  # AKDE
  tar_target(animals, get_animals(downsampled_10min)),
  tar_target(telems_list, get_telems(downsampled_10min, animals)),
  tar_target(variograms_list, get_variograms(telems_list)),
  tar_target(guesses_list, get_guesses(telems_list)),
  tar_target(fits_list, get_fits(telems_list, guesses_list)),
  tar_target(uds_w, get_uds_w(telems_list, fits_list)),
  tar_target(stats_w_95_df, get_akde_stats(uds_w, 0.95, animals, season_names, telems_list)),
  tar_target(stats_w_50_df, get_akde_stats(uds_w, 0.5, animals, season_names, telems_list)),
  tar_target(sfs_est_centroids, get_akde_centroids(uds_w, season_names, downsampled_10min)),
  # CalcMovement
  tar_target(daysTracked_seasons, get_daystracked(downsampled_10min, season_names)),
  tar_target(roostPolygons, "data/roosts50_kde95_cutOffRegion.kml", format = "file"),
  tar_target(roostsPrepped, prep_roosts(roosts, roostPolygons)),
  tar_target(downsampled_10min_sf, convertsf(downsampled_10min)),
  tar_target(areas_list, compile_areas(stats_w_95_df, stats_w_50_df)),
  tar_target(roostSwitches, get_roost_switches(roostsPrepped)),
  tar_target(shannon, get_shannon(roostsPrepped)),
  tar_target(dfdSumm, get_dfd(downsampled_10min_sf, season_names)),
  tar_target(mnMvmt, get_daily_movements(downsampled_10min_sf)),
  tar_target(dailyAltitudesSumm, get_altitude_stats(downsampled_10min_sf, season_names)),
  tar_target(movementBehavior, compile_movement_behavior(areas_list, dailyAltitudesSumm, roostSwitches, shannon, dfdSumm, mnMvmt, daysTracked_seasons, season_names, downsampled_10min_sf)),
  # Categorize movement (PCAs)
  tar_target(movementBehaviorScaled, scale_movement_behavior(movementBehavior)),
  tar_target(demo, get_demo(movementBehaviorScaled)),
  tar_target(space_use, get_space_use(movementBehaviorScaled)),
  tar_target(movement, get_movement(movementBehaviorScaled)),
  tar_target(roost_behavior, get_roost_behavior(movementBehaviorScaled)),
  tar_target(new_movement_vars, get_new_movement_vars(demo, space_use, movement, roost_behavior)),
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
  # Wrap around
  tar_target(with_times, prepare_times(downsampled_10min_forSocial)),
  tar_target(metrics_wrapped, get_metrics_wrapped(with_times, roosts, season_names, roostPolygons, 100, 1)),
  tar_target(allMetrics, combine_metrics(networkMetrics, metrics_wrapped)),
  #tar_target(sample_deviations_plot, get_deviations_plot(allMetrics)),
  tar_target(metrics_summary, get_metrics_summary(allMetrics)),
  # Mixed models
  tar_target(centrs, get_centrs(sfs_est_centroids)),
  tar_target(linked, join_movement_soc(new_movement_vars, metrics_summary, centrs, season_names))
)



