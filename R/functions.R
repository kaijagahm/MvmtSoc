# Themes ------------------------------------------------------------------
theme_quals <- function(){ 
  font <- "Calibri"   #assign font family up front
  
  theme_classic() %+replace%    #replace elements we want to change
    theme(text = element_text(size = 18, family = "Calibri"))
}

theme_abs_2023 <- function(){
  theme_classic() %+replace%
    theme(panel.background = element_rect(fill = "#FFFCF6"),
          plot.background = element_rect(fill = "#FFFCF6"),
          text = element_text(color = "#7A695A", size = 18),
          axis.text = element_text(color = "#7A695A"),
          axis.ticks = element_line(color = "#7A695A"),
          axis.line = element_line(color = "#7A695A"))
}

get_cc <- function(){
  cc <- list("breedingColor" = "#2FF8CA", "summerColor" = "#CA2FF8", "fallColor" = "#F8CA2F", flightColor = "dodgerblue", roostingColor = "olivedrab4", "feedingColor" = "gold")
  return(cc)
}

get_situcolors <- function(cc){
  situcolors <- c(cc$feedingColor, cc$flightColor, cc$roostingColor)
  return(situcolors)
}

get_seasoncolors <- function(cc){
  seasoncolors <- c(cc$breedingColor, cc$summerColor, cc$fallColor)
  return(seasoncolors)
}

# Data prep ---------------------------------------------------------------
get_loginObject <- function(pw){
  load(pw)
  loginObject <- move::movebankLogin(username = "kaijagahm", password = pw)
  rm(pw)
  return(loginObject)
}

get_inpa <- function(loginObject){
  inpa <- move::getMovebankData(study = 6071688, 
                                login = loginObject, 
                                removeDuplicatedTimestamps = TRUE,
                                timestamp_start = "2020090100000",
                                timestamp_end = "2023091500000")
  inpa <- methods::as(inpa, "data.frame")
  inpa <- inpa %>%
    mutate(dateOnly = lubridate::ymd(substr(timestamp, 1, 10)),
           year = as.numeric(lubridate::year(timestamp)))
  
  # Remove irrelevant cols
  inpa <- inpa %>%
    select(c("tag_id", "heading", "battery_charge_percent", "gps_satellite_count", "gps_time_to_fix", "external_temperature", "barometric_height", "ground_speed", "height_above_msl", "location_lat", "location_long", "timestamp", "tag_local_identifier", "trackId", "individual_id", "local_identifier", "sex", "dateOnly", "year"))
  return(inpa)
}

get_ornitela <- function(loginObject){
  minDate <- "2020-09-01 00:00"
  maxDate <- "2023-09-15 11:59"
  ornitela <- vultureUtils::downloadVultures(loginObject = loginObject, 
                                             removeDup = T, dfConvert = T, 
                                             quiet = T, 
                                             dateTimeStartUTC = minDate, 
                                             dateTimeEndUTC = maxDate)
  
  ornitela <- ornitela %>% mutate(dateOnly = lubridate::ymd(substr(timestamp, 1, 10)),
                                  year = as.numeric(lubridate::year(timestamp)))
  
  ornitela <- ornitela %>%
    select(c("tag_id", "heading", "battery_charge_percent", "gps_satellite_count", "gps_time_to_fix", "external_temperature", "barometric_height", "ground_speed", "height_above_msl", "location_lat", "location_long", "timestamp", "tag_local_identifier", "trackId", "individual_id", "local_identifier", "sex", "dateOnly", "year"))
  return(ornitela)
}

join_inpa_ornitela <- function(inpa, ornitela){
  # Add the dataset names so we can keep track of where the data comes from
  inpa_tojoin <- inpa[,names(ornitela)] %>%
    mutate(dataset = "inpa")
  ornitela <- ornitela %>%
    mutate(dataset = "ornitela")
  
  # Join the two datasets
  joined0 <- bind_rows(inpa_tojoin, ornitela)
  return(joined0)
}

fix_names <- function(joined0, ww_file){
  ww <- read_excel(ww_file, sheet = "all gps tags")
  # pull out just the names columns, nothing else, and remove any duplicates
  ww_tojoin <- ww %>% dplyr::select(Nili_id, Movebank_id) %>% distinct() 
  
  # Prepare for join: are there any individuals in the `local_identifier` column of `joined0` that don't appear in the `Movebank_id` column of `ww_tojoin`?
  problems <- joined0 %>% filter(!(local_identifier %in% ww_tojoin$Movebank_id)) %>% pull(local_identifier) %>% unique()
  problems #let's check these against the who's who and see if we can make some reasonable changes.
  
  ## Fixes:
  # Typo in the Movebank_id column of the who's who:
  ww_tojoin <- ww_tojoin %>% mutate(Movebank_id = case_when(Movebank_id == "A65 Whiite" ~ "A65 White",
                                                            .default = Movebank_id))
  # Fixes to joined0:
  joined0 <- joined0 %>%
    mutate(local_identifier = case_when(local_identifier == "E86 White" ~ "E86",
                                        local_identifier == "E88 White" ~ "E88w",
                                        .default = local_identifier))
  
  ## Look for any remaining problems:
  problems <- joined0 %>% filter(!(local_identifier %in% ww_tojoin$Movebank_id)) %>% pull(local_identifier) %>% unique()
  problems #going to fix both of these afterward. E66 isn't listed in the Who's who at all, so we'll just call it "E66" in the Nili_id. The other one, Y01>T60 W, I've manually determined is Nili_id "tammy".
  
  # join by movebank ID
  joined <- left_join(joined0, ww_tojoin, 
                      by = c("local_identifier" = "Movebank_id"))
  joined <- joined %>%
    mutate(Nili_id = case_when(is.na(Nili_id) & local_identifier == "E66 White" ~ "E66",
                               is.na(Nili_id) & local_identifier == "B01w (Y01>T60 W)" ~ "tammy", # I had this in there but they changed how they encoded it in the who's who
                               .default = Nili_id))
  
  # Are there any remaining NA's for Nili_id?
  nas <- joined %>% filter(is.na(Nili_id)) %>% pull(local_identifier) %>% unique()
  length(nas) 
  
  # There are still NAs remaining 
  joined %>%
    filter(is.na(Nili_id)) %>%
    select(local_identifier, tag_id, tag_local_identifier) %>%
    distinct()
  
  joined <- joined %>%
    mutate(Nili_id = case_when(is.na(Nili_id) & local_identifier == "B36w (T51w)" ~ "lima",
                               is.na(Nili_id) & local_identifier == "E97w (A67>T40>Y14)" ~ "kfir",
                               is.na(Nili_id) & local_identifier == "E98 W (T42w>L05>A92>Y09)" ~ "yagur",
                               is.na(Nili_id) & local_identifier == "Y11>T98 W>E99 w" ~ "richard",
                               is.na(Nili_id) & local_identifier == "B38w (T61w)" ~ "cochav",
                               is.na(Nili_id) & local_identifier == "B41w (A73>Y31>T08 B>J62 W)" ~ "uri",
                               is.na(Nili_id) & local_identifier == "B43w (T35 White)" ~ "tyra",
                               is.na(Nili_id) & local_identifier == "B55w" ~ "UNK1",
                               is.na(Nili_id) & local_identifier == "J29w" ~ "chegovar",
                               is.na(Nili_id) & local_identifier == "J52w" ~ "juno",
                               is.na(Nili_id) & local_identifier == "T24b" ~ "levy",
                               .default = Nili_id))
  
  joined %>%
    filter(is.na(Nili_id)) %>%
    select(local_identifier, tag_id, tag_local_identifier) %>%
    distinct() # no more left
  
  return(joined)
}

remove_periods <- function(ww_file, fixed_names){
  periods_to_remove <- read_excel(ww_file, sheet = "periods_to_remove")
  removed_periods <- vultureUtils::removeInvalidPeriods(dataset = fixed_names, periodsToRemove = periods_to_remove)
  return(removed_periods)
}

clean_data <- function(removed_periods){
  cleaned <- vultureUtils::cleanData(dataset = removed_periods,
                                     precise = F,
                                     removeVars = F,
                                     longCol = "location_long",
                                     latCol = "location_lat",
                                     idCol = "Nili_id",
                                     report = F)
  return(cleaned)
}

attach_age_sex <- function(cleaned, ww_file){
  age_sex <- read_excel(ww_file, sheet = "all gps tags")[,1:35] %>%
    dplyr::select(Nili_id, birth_year, sex) %>%
    distinct()
  
  with_age_sex <- cleaned %>%
    dplyr::select(-c("sex")) %>%
    left_join(age_sex, by = "Nili_id")
  
  return(with_age_sex)
}

mask_data <- function(with_age_sex, mask){
  msk <- sf::st_read(mask)
  # Region masking
  data_masked <- vultureUtils::inMaskFilter(dataset = with_age_sex, mask = msk, inMaskThreshold = 0.33, crs = "WGS84")$dataset # this takes a long time!!
  
  # Fix time zone so dates make sense ---------------------------------------
  ## Overwrite the dateOnly column from the new times
  data_masked <- data_masked %>%
    mutate(timestampIsrael = lubridate::with_tz(timestamp, tzone = "Israel"),
           dateOnly = lubridate::date(timestampIsrael),
           month = lubridate::month(dateOnly),
           year = lubridate::year(dateOnly))
  return(data_masked)
}

split_seasons <- function(data_masked){
  with_seasons <- data_masked %>% mutate(month = lubridate::month(timestampIsrael),
                                         day = lubridate::day(timestampIsrael),
                                         year = lubridate::year(timestampIsrael)) %>%
    mutate(season = case_when(((month == 12 & day >= 15) | 
                                 (month %in% 1:4) | 
                                 (month == 5 & day < 15)) ~ "breeding",
                              ((month == 5 & day >= 15) | 
                                 (month %in% 6:8) | 
                                 (month == 9 & day < 15)) ~ "summer",
                              .default = "fall")) %>%
    filter(dateOnly != "2023-09-15") %>% # remove September 15 2023, because it turns out the season boundaries are supposed to be non-inclusive and this is the latest one.
    mutate(seasonUnique = case_when(season == "breeding" & month == 12 ~
                                      paste(as.character(year + 1), season, sep = "_"),
                                    .default = paste(as.character(year), season, sep = "_"))) %>%
    mutate(seasonUnique = factor(seasonUnique, levels = c("2020_summer", "2020_fall", "2021_breeding", "2021_summer", "2021_fall", "2022_breeding", "2022_summer", "2022_fall", "2023_breeding", "2023_summer")),
           season = factor(season, levels = c("breeding", "summer", "fall")))
  
  # Add ages (based on season, which is why this goes here rather than above)
  with_seasons <- with_seasons %>%
    mutate(age = year - birth_year) %>%
    mutate(age = ifelse(season == "breeding" & month %in% c(1, 12), age + 1, age)) %>%
    group_by(Nili_id, year, month) %>%
    mutate(age = ifelse(month == 2, min(age) + 1, age)) %>%
    mutate(age_group = case_when(age == 0 ~ "juv",
                                 age >= 1 & age <= 4 ~ "sub",
                                 age >= 5 ~ "adult",
                                 .default = NA)) %>%
    ungroup()
  
  # REMOVE SUMMER 2020--we don't need to use it
  with_seasons <- with_seasons %>%
    filter(seasonUnique != "2020_summer")
  length(unique(with_seasons$seasonUnique)) # should be 9
  
  # Separate the seasons -----------------------------
  seasons_list <- with_seasons %>%
    group_by(seasonUnique) %>%
    group_split(.keep = T)
  return(seasons_list)
}

get_season_names <- function(seasons_list){
  season_names <- map_chr(seasons_list, ~as.character(.x$seasonUnique[1]))
  return(season_names)
}

remove_northern <- function(seasons_list){
  seasons_sf <- map(seasons_list, ~.x %>%
                      sf::st_as_sf(coords = c("location_long", "location_lat"), 
                                   remove = F) %>%
                      sf::st_set_crs("WGS84") %>%
                      sf::st_transform(32636))
  
  ## Get centroids, so we can see who's "southern" for that season.
  centroids <- map(seasons_sf, ~.x %>%
                     group_by(Nili_id) %>%
                     summarize(geometry = st_union(geometry)) %>%
                     st_centroid())
  
  ## Examine a histogram of centroid latitudes 
  #walk(centroids, ~hist(st_coordinates(.x)[,2])) # looks like 3550000 is generally a good cutoff point here.
  
  ## Get southern individuals for each season, so we can filter the data
  southern_indivs <- map(centroids, ~.x %>%
                           filter(st_coordinates(.)[,2] < 3550000) %>%
                           pull(Nili_id))
  
  ## Remove individuals not in the south
  removed_northern <- map2(.x = seasons_list, 
                           .y = southern_indivs, ~.x 
                           %>% filter(Nili_id %in% .y))
  return(removed_northern)
}

remove_lfr <- function(removed_northern){
  Mode <- function(x) { 
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
  }
  
  # Calculate daily modes and add them to the dataset (using the function defined above)
  with_modes <- map(removed_northern, ~.x %>%
                      group_by(dateOnly, Nili_id) %>%
                      mutate(diff = as.numeric(difftime(lead(timestamp), timestamp, units = "mins"))) %>%
                      group_by(dateOnly, Nili_id) %>%
                      mutate(mode = Mode(round(diff))) %>%
                      ungroup())
  
  # Identify individuals that never have a daily mode of 10 minutes
  low_fix_rate_indivs <- map(with_modes, ~.x %>%
                               sf::st_drop_geometry() %>%
                               group_by(Nili_id) %>%
                               summarize(minmode = min(mode, na.rm = T)) %>%
                               filter(minmode > 10) %>%
                               pull(Nili_id) %>%
                               unique())
  
  # Don't need to save with_modes because we've already used this step to identify low fix rate individuals.
  removed_lfr <- map2(removed_northern, low_fix_rate_indivs,                     
                      ~.x %>% filter(!(Nili_id %in% .y)))
  return(removed_lfr)
}

downsample_10min <- function(data){
  # Define function for downsampling 
  down <- function(df, mins){
    df <- dplyr::arrange(df, timestamp)
    howmuch <- paste(mins, "minute", sep = " ")
    rounded <- lubridate::round_date(df$timestamp, howmuch)
    keep <- !duplicated(rounded)
    return(df[keep,])
  }
  
  # Split by individual so we can downsample more efficiently
  lst <- map(data, ~.x %>% group_split(Nili_id))
  
  # Perform downsampling
  downsampled_10min <- map(lst, ~{
    purrr::list_rbind(map(.x, ~down(df = .x, mins = 10), .progress = T))
  }, .progress = T)
  
  # Add information on # of days tracked for each individual
  downsampled_10min <- map(downsampled_10min, ~.x %>% group_by(Nili_id) %>% 
                             mutate(daysTracked = length(unique(dateOnly))))
  return(downsampled_10min)
}

get_roosts <- function(dat){
  roosts <- map(dat, ~vultureUtils::get_roosts_df(df = .x, id = "Nili_id"), 
                .progress = T)
  roosts <- roosts %>%
    map(., ~st_as_sf(.x, crs = "WGS84", 
                     coords = c("location_long", "location_lat"), 
                     remove = F), .progress = T)
  return(roosts)
}

remove_nighttime <- function(removed_lfr){
  removed_nighttime <- map(removed_lfr, ~{
    times <- suncalc::getSunlightTimes(date = unique(lubridate::date(.x$timestamp)),
                                       lat = 31.434306, lon = 34.991889, # coords represent rough spatial centroid of Israel, based on https://wikimapia.org/37549200/Geographic-center-of-Israel
                                       keep = c("sunrise", "sunset")) %>%
      dplyr::select("dateOnly" = date, sunrise, sunset)
    
    .x <- .x %>%
      dplyr::mutate(dateOnly = lubridate::ymd(dateOnly)) %>%
      dplyr::left_join(times, by = "dateOnly") %>%
      dplyr::mutate(daylight = ifelse(timestamp >= sunrise & timestamp <= sunset, "day", "night")) %>%
      dplyr::filter(daylight == "day")
  }, .progress = T)
  return(removed_nighttime)
}

remove_lowppd <- function(removed_nighttime){
  removed_lowppd <- map(removed_nighttime, ~.x %>%
                          group_by(Nili_id, dateOnly) %>%
                          filter(n() >= 10 | (n() < 10 & 
                                                min(battery_charge_percent, 
                                                    na.rm = T) > 50)) %>%
                          ungroup())
  return(removed_lowppd)
}

remove_too_few_days <- function(removed_lowppd){
  indivsToKeep <- map(removed_lowppd, ~.x %>%
                        st_drop_geometry() %>%
                        group_by(Nili_id) %>%
                        summarize(nDaysTracked = 
                                    length(unique(dateOnly))) %>%
                        filter(nDaysTracked >= 30) %>%
                        pull(Nili_id), .progress = T)
  
  ## remove those individuals not tracked for enough days
  removed_too_few_days <- map2(.x = removed_lowppd, 
                               .y = indivsToKeep, ~.x %>% 
                                 filter(Nili_id %in% .y))
  return(removed_too_few_days)
}

# AKDE --------------------------------------------------------------------
get_animals <- function(downsampled_10min){
  animals <- map(downsampled_10min, ~unique(.x$Nili_id))
  return(animals)
}

get_telems <- function(downsampled_10min, animals){
  seasons_split <- map(downsampled_10min, ~.x %>%
                         dplyr::select("ID" = Nili_id,
                                       timestamp,
                                       "longitude" = location_long,
                                       "latitude" = location_lat) %>%
                         group_by(ID) %>%
                         group_split())
  telems_list <- map(seasons_split, ~map(.x, as.telemetry))
  return(telems_list)
}

get_variograms <- function(telems_list){
  variograms_list <- map(telems_list, ~map(.x, variogram, .progress = T))
  return(variograms_list)
}

get_guesses <- function(telems_list){
  guesses_list <- furrr::future_map(telems_list, ~map(.x, ~ctmm.guess(.x, interactive = FALSE), .progress = T))
  return(guesses_list)
}

get_fits_1season <- function(telems_list, guesses_list, season){
  tl <- telems_list[[season]]
  gl <- guesses_list[[season]]
  fits <- map2(.x = tl, .y = gl, ~ctmm.select(.x, .y, method = "pHREML"), .progress = T)
  return(fits)
}

get_fits <- function(telems_list, guesses_list){
  fits_list <- vector(mode = "list", length = length(telems_list))
  future::plan(future::multisession, workers = 15)
  for(i in 1:length(fits_list)){
    fits_list[[i]] <- furrr::future_map2(.x = telems_list[[i]],
                                         .y = guesses_list[[i]],
                                         ~ctmm.select(.x, .y, method = "pHREML"), .progress = T)
  }
  return(fits_list)
}

get_uds_w <- function(telems_list, fits_list){
  dt <- 10 %#% "min" 
  future::plan(future::multisession, workers = 10)
  uds_w <- vector(mode = "list", length = length(fits_list))
  for(i in 1:length(fits_list)){
    uds_w[[i]] <- furrr::future_map2(telems_list[[i]], fits_list[[i]],
                                     ~akde(.x, .y, dt = dt, weights = T),
                                     .progress = T)
  }
  return(uds_w)
}

get_akde_stats <- function(uds_w, pct, animals, season_names, telems_list){
  stats <- vector(mode = "list", length = length(uds_w))
  for(i in 1:length(stats)){
    stats_weighted <- map(uds_w[[i]],
                          ~summary(.x, level = pct, level.UD = pct)$CI %>% as.data.frame()) %>%
      purrr::list_rbind() %>%
      bind_cols(map(uds_w[[i]],
                    ~summary(.x, level = pct, level.UD = pct)$DOF %>% t() %>% as.data.frame()) %>%
                  purrr::list_rbind()) %>%
      mutate(ID = animals[[i]],
             weighted = T) %>%
      rename("n_eff_area" = "area",
             "dof_bandwidth" = "bandwidth") %>%
      mutate(n_abs_area = map_dbl(telems_list[[i]], nrow),
             eff_ss_prop = round(n_eff_area/n_abs_area, 2),
             seasonUnique = season_names[i])
    stats[[i]] <- stats_weighted
  }
  stats_df <- purrr::list_rbind(stats) %>% mutate(level = pct)
  return(stats_df)
}

get_akde_centroids <- function(uds_w, season_names, downsampled_10min){
  uds_w_sf <- map(uds_w, ~map(.x, ~as.sf(.x) %>% sf::st_transform("WGS84"), 
                              .progress = T))
  uds_w_sf_seasons <- map(uds_w_sf, ~do.call(rbind, .x))
  uds_w_sf_seasons <- map2(uds_w_sf_seasons, season_names, ~.x %>% mutate(seasonUnique = .y))
  sfs <- do.call(rbind, uds_w_sf_seasons)
  sfs <- sfs %>%
    mutate(Nili_id = stringr::str_extract(name, "^.*(?=\\s[0-9]+)"),
           type = stringr::str_extract(name, "(?<=95%\\s)[a-z]+(?=$)"),
           pct = as.numeric(stringr::str_extract(name, "[0-9]+(?=%)")))
  sfs_est <- sfs %>%
    filter(type == "est")
  
  sfs_est_centroids <- sfs_est %>%
    sf::st_centroid()
  
  
  # Overall per-season centroids
  allpointssf <- downsampled_10min %>% purrr::list_rbind() %>% sf::st_as_sf(coords = c("location_long", "location_lat"), crs = "WGS84", remove = F)
  
  seasonal_centroids <- allpointssf %>%
    group_by(seasonUnique) %>%
    summarize(geometry = sf::st_union(geometry)) %>%
    sf::st_centroid()
  
  vec <- rep(NA, nrow(sfs_est_centroids))
  for(i in 1:length(vec)){
    dat <- sfs_est_centroids[i,]
    szn <- dat$seasonUnique
    szn_centr <- seasonal_centroids[which(seasonal_centroids$seasonUnique == szn),]
    out <- as.numeric(st_distance(dat, szn_centr))
    vec[i] <- out
  }
  
  sfs_est_centroids$dist_szn_centr <- vec
  
  return(sfs_est_centroids)
}

get_daystracked <- function(downsampled_10min, season_names){
  durs <- map_dbl(downsampled_10min, ~{
    dates <- lubridate::ymd(.x$dateOnly)
    dur <- difftime(max(dates), min(dates), units = "days")
  })
  daysTracked_seasons <- map2(downsampled_10min, durs, ~.x %>%
                                st_drop_geometry() %>%
                                group_by(Nili_id) %>%
                                summarize(daysTracked = length(unique(dateOnly)),
                                          propDaysTracked = daysTracked/.y))
  names(daysTracked_seasons) <- season_names
  return(daysTracked_seasons)
}

convertsf <- function(downsampled_10min){
  downsampled_10min_sf <- map(downsampled_10min, ~.x %>% sf::st_as_sf(coords = c("location_long", "location_lat"), crs = "WGS84", remove = F))
  return(downsampled_10min_sf)
}

compile_areas <- function(stats_w_95_df, stats_w_50_df){
  akde_stats <- bind_rows(stats_w_50_df, stats_w_95_df)
  row.names(akde_stats) <- NULL
  
  areas <- akde_stats %>%
    dplyr::select("Nili_id" = ID,
                  est, level, seasonUnique) %>%
    pivot_wider(id_cols = c(Nili_id, seasonUnique), names_from = level, names_prefix = "level_",
                values_from = "est") %>%
    rename("homeRange" = level_0.95,
           "coreArea" = level_0.5) %>%
    mutate(coreAreaFidelity = coreArea/homeRange)
  areas_list <- areas %>%
    group_by(seasonUnique) %>%
    group_split()
  return(areas_list)
}

compile_movement_behavior <- function(areas_list,
                                      daysTracked_seasons,
                                      season_names,
                                      downsampled_10min_sf){
  movementBehavior <- map2(areas_list, daysTracked_seasons, 
                           ~left_join(.x, .y, by = "Nili_id")) %>%
    map2(., .y = season_names, 
         ~.x %>% mutate(seasonUnique = .y) %>% 
           relocate(seasonUnique, .after = "Nili_id"))
  
  ## Add age and sex
  ageSex <- map(downsampled_10min_sf, ~.x %>% sf::st_drop_geometry() %>% 
                  dplyr::select(Nili_id, birth_year, age_group, sex) %>% 
                  distinct())
  
  movementBehavior <- map2(movementBehavior, ageSex, ~left_join(.x, .y, by = "Nili_id"))
  return(movementBehavior)
}

# Movement vars pca -------------------------------------------------------
scale_movement_behavior <- function(movementBehavior){
  allMovementBehavior <- purrr::list_rbind(movementBehavior)
  movementBehaviorScaled <- allMovementBehavior %>%
    mutate(coreArea_log = log(coreArea),
           homeRange_log = log(homeRange)) %>%
    mutate(across(-c(Nili_id, seasonUnique, birth_year, sex, age_group), 
                  function(x){as.numeric(as.vector(scale(x)))}))
  return(movementBehaviorScaled)
}

get_demo <- function(movementBehaviorScaled){
  demo <- movementBehaviorScaled %>%
    dplyr::select(Nili_id, seasonUnique, daysTracked, propDaysTracked, birth_year, age_group, sex)
  return(distinct(demo))
}

get_space_use <- function(movementBehaviorScaled){
  space_use <- movementBehaviorScaled %>%
    dplyr::select(Nili_id, seasonUnique, coreArea_log, homeRange_log, coreAreaFidelity)
  pca <- prcomp(space_use[,-c(1:3)])
  space_use <- space_use %>%
    mutate(space_pc1 = pca$x[,1]*-1)
  summary(pca) # explains 84% of variance
  return(distinct(space_use))
}

get_space_use_pca <- function(movementBehaviorScaled){
  space_use <- movementBehaviorScaled %>%
    dplyr::select(Nili_id, seasonUnique, coreArea_log, homeRange_log, coreAreaFidelity)
  pca <- prcomp(space_use[,-c(1:3)])
  return(pca)
}

get_new_movement_vars <- function(demo, space_use){
  new_movement_vars <- demo %>%
    left_join(space_use %>% dplyr::select(Nili_id, seasonUnique, space_pc1)) %>%
    rename("space_use" = "space_pc1")
  return(new_movement_vars)
}

get_all_movement_vars <- function(demo, space_use){
  all_movement_vars <- demo %>%
    left_join(space_use) %>%
    rename("space_use" = "space_pc1")
  return(all_movement_vars)
}

# Social networks ---------------------------------------------------------
get_flight <- function(sfdata, roostPolygons){
  rp <- sf::st_read(roostPolygons)
  flight <- vector(mode = "list", length = length(sfdata))
  for(i in 1:length(sfdata)){
    cat("Working on iteration", i, "\n")
    dat <- sfdata[[i]]
    fl <- vultureUtils::getFlightEdges(dat, 
                                       roostPolygons = rp,
                                       distThreshold = 1000,
                                       idCol = "Nili_id",
                                       return = "both",
                                       getLocs = T)
    flight[[i]] <- fl
    rm(fl)
  }
  return(flight)
}

get_feeding <- function(sfdata, roostPolygons){
  rp <- sf::st_read(roostPolygons)
  feeding <- vector(mode = "list", length = length(sfdata))
  for(i in 1:length(sfdata)){
    cat("Working on iteration", i, "\n")
    dat <- sfdata[[i]]
    fe <- vultureUtils::getFeedingEdges(dat, 
                                        roostPolygons = rp,
                                        distThreshold = 50,
                                        idCol = "Nili_id",
                                        return = "both", getLocs = T)
    feeding[[i]] <- fe
    rm(fe)
  }
  return(feeding)
}

get_roosting <- function(roosts){
  roosting <- map(roosts, ~{
    vultureUtils::getRoostEdges(.x, mode = "distance", 
                                distThreshold = 500,
                                return = "both", 
                                latCol = "location_lat", 
                                longCol = "location_long", 
                                idCol = "Nili_id", 
                                dateCol = "roost_date")
  }, .progress = T)
  return(roosting)
}

get_list_element <- function(lst, element){
  out <- map(lst, element)
  return(out)
}

get_graphs <- function(sri){
  g <- map(sri, ~vultureUtils::makeGraph(mode = "sri", data = .x, weighted = T))
  return(g)
}

get_network_metrics <- function(flightGraphs, feedingGraphs, roostingGraphs, season_names){
  networkMetrics <- map2_dfr(flightGraphs, season_names, ~{
    df <- data.frame(degree = igraph::degree(.x),
                     strength = igraph::strength(.x),
                     Nili_id = names(degree(.x))) %>%
      bind_cols(season = .y,
                type = "flight")
    
    return(df)
  }) %>%
    bind_rows(map2_dfr(feedingGraphs, season_names, ~{
      df <- data.frame(degree = igraph::degree(.x),
                       strength = igraph::strength(.x),
                       Nili_id = names(degree(.x))) %>%
        bind_cols(season = .y,
                  type = "feeding")
      
      return(df)
    })) %>%
    bind_rows(map2_dfr(roostingGraphs, season_names, ~{
      df <- data.frame(degree = igraph::degree(.x),
                       strength = igraph::strength(.x),
                       Nili_id = names(degree(.x))) %>%
        bind_cols(season = .y,
                  type = "roosting")
      
      return(df)
    }))
  
  # Add info about number of indivs and relative measures
  networkMetrics <- networkMetrics %>%
    group_by(season, type) %>%
    mutate(n = length(unique(Nili_id)), # CAREFUL HERE! Was getting the wrong degree bc conflating number of rows with number of individuals.
           totalStrength = sum(strength)) %>% 
    ungroup() %>%
    mutate(normDegree = degree/(n-1),
           normStrength = strength/totalStrength) %>%
    dplyr::select(season, type, n, Nili_id, degree, normDegree, strength, normStrength)
  return(networkMetrics)
}

prepare_times <- function(downsampled_10min_forSocial){
  with_times <- map(downsampled_10min_forSocial, ~{
    .x %>% mutate(time = substr(as.character(timestamp), 12, 19))
  })
  return(with_times)
}

# Wrap-around -------------------------------------------------------------
fn <- function(dataset, rst, iter, shiftMax, roostPolygons){
  # rotate data
  #message("rotating")
  rotated <- rotate_data_table(dataset = dataset, shiftMax = shiftMax, 
                               idCol = "Nili_id", dateCol = "dateOnly",
                               timeCol = "time") %>% 
    mutate(timestamp = newdatetime) %>%
    sf::st_as_sf(., coords = c("location_long", "location_lat"), remove = F, crs = "WGS84")
  verts <- unique(rotated$Nili_id)
  
  tojoin_roosts <- rotated %>% st_drop_geometry() %>% 
    select(Nili_id, "roost_date" = dateOnly, newDate) %>% distinct()
  roosts_temp <- rst %>%
    left_join(tojoin_roosts, by = c("Nili_id", "roost_date")) %>%
    mutate(roost_date = newDate)
  vertsr <- unique(roosts_temp$Nili_id)
  
  # calculate sri for rotated data
  #message("calculating sri")
  fl <- vultureUtils::getFlightEdges(rotated, roostPolygons = roostPolygons,
                                     distThreshold = 1000, idCol = "Nili_id",
                                     return = "sri") %>% 
    mutate(sri = replace_na(sri, 0)) %>%
    filter(sri != 0)
  fe <- vultureUtils::getFeedingEdges(rotated, roostPolygons = roostPolygons,
                                      distThreshold = 50, idCol = "Nili_id",
                                      return = "sri") %>%
    mutate(sri = replace_na(sri, 0)) %>%
    filter(sri != 0)
  ro <- vultureUtils::getRoostEdges(roosts_temp, mode = "distance",
                                    distThreshold = 100, latCol = "location_lat",
                                    longCol = "location_long", idCol = "Nili_id",
                                    dateCol = "roost_date", return = "sri") %>%
    mutate(sri = replace_na(sri, 0)) %>%
    filter(sri != 0)
  
  # make graph from sri
  #message("making graph")
  g_fl <- igraph::graph_from_data_frame(d = fl %>% mutate(weight = sri), 
                                        directed = FALSE, vertices = verts)
  g_fe <- igraph::graph_from_data_frame(d = fe %>% mutate(weight = sri), 
                                        directed = FALSE, vertices = verts)
  g_ro <- igraph::graph_from_data_frame(d = ro %>% mutate(weight = sri), 
                                        directed = FALSE, vertices = vertsr)
  
  
  # calculate graph metrics
  #message("calculating metrics")
  graph_metrics <- data.frame(degree = degree(g_fl),
                              strength = strength(g_fl),
                              Nili_id = names(degree(g_fl)),
                              situ = "flight") %>%
    bind_rows(data.frame(degree = degree(g_fe),
                         strength = strength(g_fe),
                         Nili_id = names(degree(g_fe)),
                         situ = "feeding")) %>%
    bind_rows(data.frame(degree = degree(g_ro),
                         strength = strength(g_ro),
                         Nili_id = names(degree(g_ro)),
                         situ = "roosting")) %>%
    mutate(rep = iter)
  row.names(graph_metrics) <- NULL
  
  # save graph metrics to spot in list
  return(graph_metrics)
}

# rotate_data_table ------------------------------------------------------------
# Note: this no longer actually takes a data_table, just keeping the name for consistency with previous.
# Unlike the previous function, this one requires that you separate the dates and times into separate columns beforehand.
rotate_data_table <- function(dataset, shiftMax, idCol = "indiv", dateCol = "date", timeCol = "time"){
  indivList <- dataset %>%
    group_by(.data[[idCol]]) %>%
    group_split(.keep = T)
  joined <- vector(mode = "list", length = length(indivList))
  for(indiv in 1:length(indivList)){
    x <- indivList[[indiv]]
    shift <- sample(-(shiftMax):shiftMax, size = 1)
    #cat(shift, "\n")
    # get all unique days that show up
    days <- sort(unique(x[[dateCol]]))
    
    # get min and max dates to shift around (the "poles" of the conveyor)
    selfMinDate <- min(days, na.rm = T)
    selfMaxDate <- max(days, na.rm = T)
    
    # create a total sequence of dates to select from
    daysFilled <- seq(lubridate::ymd(selfMinDate), lubridate::ymd(selfMaxDate), by = "day")
    # converting to numbers so we can use %%--which dates are the ones we started with?
    vec <- which(daysFilled %in% days)
    shiftedvec <- vec + shift # shift
    new <- (shiftedvec - min(vec)) %% (max(vec)-min(vec)+1)+1 # new dates as numbers
    shiftedDates <- daysFilled[new] # select those dates from the possibilities
    
    # Make a data frame to hold the old and new dates
    daysDF <- bind_cols({{dateCol}} := days, 
                        "newDate" = shiftedDates,
                        shift = shift)
    nw <- left_join(x, daysDF, by = dateCol)
    
    if(!is.null(timeCol)){
      nw$newdatetime <- lubridate::ymd_hms(paste(nw$newDate, nw[[timeCol]]))
    }
    joined[[indiv]] <- nw
  }
  out <- purrr::list_rbind(joined)
  return(out)
}

get_metrics_wrapped_1season <- function(with_times, roosts, season_names, roostPolygons, reps, shiftMax, season){
  rp <- sf::st_read(roostPolygons)
  dat <- with_times[[season]]
  rst <- roosts[[season]]
  metrics <- map(1:reps, ~fn(dataset = dat, rst = rst, iter = .x, shiftMax = shiftMax, roostPolygons = rp), .progress = T)
  
  metrics <- purrr::list_rbind(map2(metrics, 1:reps, ~.x %>% mutate(rep = .y))) %>% mutate(seasonUnique = season_names[season])
  
  return(metrics)
}

compile_metrics_wrapped <- function(lst){
  metrics_wrapped <- purrr::list_rbind(lst)
  metrics_wrapped <- metrics_wrapped %>%
    group_by(seasonUnique, situ, rep) %>%
    mutate(n = length(unique(Nili_id)))
  
  metrics_wrapped <- metrics_wrapped %>%
    group_by(seasonUnique, situ, rep) %>%
    mutate(normDegree = degree/(n-1),
           normStrength = strength/sum(strength, na.rm = T)) %>%
    ungroup()
  return(metrics_wrapped)
}

get_metrics_wrapped <- function(with_times, roosts, season_names, roostPolygons, reps, shiftMax){
  future::plan(future::multisession, workers = 5)
  rp <- sf::st_read(roostPolygons)
  metrics_seasons <- vector(mode = "list", length = length(with_times))
  for(i in 1:length(with_times)){
    cat("Working on season ", i)
    dat <- with_times[[i]]
    rst <- roosts[[i]]
    metrics_seasons[[i]] <- furrr::future_map(1:reps, ~fn(dataset = dat, 
                                                          rst = rst, iter = .x, 
                                                          shiftMax = shiftMax,
                                                          roostPolygons = rp),
                                              .progress = T)
  }
  
  for(i in 1:length(metrics_seasons)){
    metrics_seasons[[i]] <- purrr::list_rbind(map2(metrics_seasons[[i]], 1:reps, ~.x %>% mutate(rep = .y))) %>% mutate(seasonUnique = season_names[i])
  }
  metrics_wrapped <- purrr::list_rbind(metrics_seasons)
  metrics_wrapped <- metrics_wrapped %>%
    group_by(seasonUnique, situ, rep) %>%
    mutate(n = length(unique(Nili_id)))
  
  metrics_wrapped <- metrics_wrapped %>%
    group_by(seasonUnique, situ, rep) %>%
    mutate(normDegree = degree/(n-1),
           normStrength = strength/sum(strength, na.rm = T))
  return(metrics_wrapped)
}

combine_metrics <- function(networkMetrics, metrics_wrapped){
  allMetrics <- networkMetrics %>% select(season, "situ" = type, Nili_id, degree, strength, normDegree, normStrength)%>% left_join(metrics_wrapped %>% select("season" = seasonUnique, situ, Nili_id, "wrapped_strength" = strength, "wrapped_degree" = degree, rep, "wrapped_normDegree" = normDegree, "wrapped_normStrength" = normStrength)) %>%
    mutate(Nili_id = factor(Nili_id))
  return(allMetrics)
}

get_metrics_summary <- function(allMetrics){
  metrics_summary <- allMetrics %>%
    group_by(season, situ, Nili_id) %>%
    summarize(normDegree = normDegree[1],
              normStrength = normStrength[1],
              # Calculate the z-scores for non-normalized values
              degree = degree[1],
              strength = strength[1],
              diff_deg = degree[1]-mean(wrapped_degree, na.rm = T),
              diff_str = strength[1]-mean(wrapped_strength, na.rm = T),
              sd_deg = sd(wrapped_degree, na.rm = T),
              sd_str = sd(wrapped_strength, na.rm = T)) %>%
    mutate(z_deg = diff_deg/sd_deg,
           z_str = diff_str/sd_str) %>%
    rename("seasonUnique" = season)
  return(metrics_summary)
}

# Pre-modeling ------------------------------------------------------------
join_movement_soc <- function(new_movement_vars, metrics_summary, season_names){
  oneage <- new_movement_vars %>%
    distinct() %>%
    group_by(Nili_id, seasonUnique) %>%
    arrange(age_group, .by_group = T) %>%
    slice(1) # taking the first row of each group (the adult row)--this will remove the subadults for the breeding seasons where individuals transition ages.
  # XXX THIS IS REALLY FRAGILE--MAKE IT MORE EXPLICIT!
  linked <- oneage %>% 
    left_join(metrics_summary, 
              by = c("Nili_id", "seasonUnique"))
  
  # Housekeeping
  linked <- linked %>%
    mutate(season = stringr::str_extract(seasonUnique, "[a-z]+"),
           year = as.numeric(stringr::str_extract(seasonUnique, "[0-9]+")),
           seasonUnique = factor(seasonUnique, levels = season_names),
           sex = factor(sex),
           season = factor(season, levels = c("breeding", "summer", "fall")))
  
  # Create a shorter version of "type" for easier interpretability
  linked <- linked %>%
    rename("type" = situ) %>%
    mutate(situ = case_when(type == "flight" ~ "Fl", 
                            type == "feeding" ~ "Fe",
                            type == "roosting" ~ "Ro"))
  
  # Let's examine zeroes for the social network measures. I know that when we calculate the social networks, we had a lot of zeroes for both degree and strength. But most of the individuals that didn't have network connections probably aren't our focal individuals for the movement measures.
  # linked %>% filter(z_deg == 0 | z_str == 0) # nobody
  # linked %>% filter(degree == 0, strength == 0) # just one individual in one season
  # linked %>% filter(is.na(z_deg) | is.na(z_str)) # just one individual in one season
  # linked %>% filter(is.na(z_deg) | is.na(z_str)) # just one individual in one season
  
  # Let's remove that individual in case she poses a problem for modeling
  linked <- linked %>%
    filter(!is.na(z_deg), !is.na(z_str))
  #nrow(linked)
  
  #linked %>% filter(is.infinite(z_deg) | is.infinite(z_str)) # likewise, removing the infinite individual
  linked <- linked %>%
    filter(!is.infinite(z_deg), !is.infinite(z_str))
  
  return(linked)
}

get_n_in_network <- function(season_names, flightGraphs, feedingGraphs, roostingGraphs){
  ns <- data.frame(seasonUnique = season_names,
                   Fl = map_dbl(flightGraphs, ~length(igraph::V(.x))),
                   Fe = map_dbl(feedingGraphs, ~length(igraph::V(.x))),
                   Ro = map_dbl(roostingGraphs, ~length(igraph::V(.x)))) %>%
    pivot_longer(cols = -seasonUnique, names_to = "situ", values_to = "nInNetwork")
  return(ns)
}
