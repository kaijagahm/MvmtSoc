library(tidyverse)
library(vultureUtils)
library(here)

targets::tar_load(downsampled_10min_forSocial)
load(here("cassidy/roosts_2022_seasons.Rda"))
r <- purrr::list_rbind(map(roosts_2022_seasons, sf::st_drop_geometry))

r %>%
  filter(date != roost_date)

test <- downsampled_10min_forSocial %>%
  purrr::list_rbind() %>%
  filter(dateOnly %in% lubridate::ymd(c("2022-05-12", "2022-05-13", "2022-05-14", "2022-05-15")))
nrow(test)

dome <- test %>%
  filter(Nili_id == "dome")
nrow(dome)

df <- dome
id <- "local_identifier"
timestamp <- "timestamp"
x <- "location_long"
y <- "location_lat"
ground_speed <- "ground_speed"
speed_units <- "m/s"
buffer <- 1
twilight <- 61
morning_hours <- c(0:12)
night_hours <- c(13:23)
quiet <- F

get_roosts_df <- function(df, id = "local_identifier", timestamp = "timestamp", x = "location_long", y = "location_lat", ground_speed = "ground_speed", speed_units = "m/s", buffer = 1, twilight = 61, morning_hours = c(0:12), night_hours = c(13:23), quiet = F){
  # setup for time warning
  if(!quiet){
    cat("\nFinding roosts... this may take a while if your dataset is large.\n")
    start <- Sys.time()
  }
  
  # Argument checks
  checkmate::assertDataFrame(df)
  checkmate::assertCharacter(id, len = 1)
  checkmate::assertSubset(id, names(df))
  checkmate::assertCharacter(timestamp, len = 1)
  checkmate::assertSubset(timestamp, names(df))
  checkmate::assertCharacter(x, len = 1)
  checkmate::assertSubset(x, names(df))
  checkmate::assertCharacter(y, len = 1)
  checkmate::assertSubset(y, names(df))
  checkmate::assertCharacter(ground_speed, len = 1)
  checkmate::assertSubset(ground_speed, names(df))
  checkmate::assertCharacter(speed_units, len = 1)
  checkmate::assertSubset(speed_units, c("m/s", "km/h"))
  checkmate::assertNumeric(buffer, len = 1)
  checkmate::assertNumeric(twilight, len = 1)
  checkmate::assertNumeric(morning_hours, upper = 24, lower = 0)
  checkmate::assertNumeric(night_hours, upper = 24, lower = 0)
  checkmate::assertNumeric(df[[x]])
  checkmate::assertNumeric(df[[y]])
  checkmate::assertNumeric(df[[ground_speed]])
  
  # If the data is an sf object, remove the geometry to make it easier to work with. The computations in this function just depend on lat/long columns being present.
  if("sf" %in% class(df)){
    df <- df %>%
      sf::st_drop_geometry()
  }
  
  # Transform the twilight period into seconds
  twilight_secs <- twilight * 60
  
  # If the speed is in km/h transform into m/s
  if(speed_units == "km/h"){
    df <- df %>%
      dplyr::mutate({{ground_speed}} := round(.data[[ground_speed]] / 3.6, 3))
  }
  
  df[[timestamp]] <- as.POSIXct(df[[timestamp]],
                                format = "%Y-%m-%d %H:%M:%S",
                                tz = "UTC")
  
  if(sum(is.na(df[[timestamp]])) > 0){
    stop("Timestamp needs to be defined as.POSIXct (%Y-%m-%d %H:%M:%S)")
  }
  
  df$date <- as.Date(df[[timestamp]])
  
  # Separate into a list of individuals
  indivs <- df %>%
    dplyr::group_by(.data[[id]]) %>%
    dplyr::group_split(.keep = T)
  
  roosts <- purrr::map_dfr(indivs, ~{
    temp.id <- unique(.x[[id]])
    
    id.df <- .x %>%
      dplyr::group_by(date) %>%
      dplyr::arrange({{timestamp}}) %>%
      dplyr::mutate(
        row_id = dplyr::case_when(
          dplyr::row_number() == 1 ~ "first",
          dplyr::row_number() == max(dplyr::row_number()) ~ "last"),
        hour = lubridate::hour(.data[[timestamp]])) %>%
      dplyr::filter(row_id %in% c("first", "last")) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(day_diff = round(difftime(dplyr::lead(date), date, units="days")))
    
    matrix <- as.matrix(id.df[,c(x, y)])
    leadMatrix <- as.matrix(cbind(dplyr::lead(id.df[[x]]),
                                  dplyr::lead(id.df[[y]])))
    distances <- geosphere::distGeo(p1 = matrix, p2 = leadMatrix)*0.001 %>%
      round(., 2)
    id.df$dist_km <- distances
    id.df$dist_km[id.df$day_diff != 1] <- NA
    
    # Ryan's Code: I think maptools::sunriset can be replaced with suncalc::getSunlightTimes since its used in other places
    # SEE: https://cran.r-project.org/web/packages/suncalc/ https://cran.r-project.org/web/packages/suncalc/suncalc.pdf
    
    data <- data.frame(date = as.Date(id.df[[timestamp]]), lat = id.df[[y]], lon = id.df[[x]])
    
    id.df$sunrise <- suncalc::getSunlightTimes(data = data, keep = c("sunrise"))$sunrise
    id.df$sunset <- suncalc::getSunlightTimes(data = data, keep = c("sunset"))$sunset
    
    # Set the twilight
    id.df$sunrise_twilight <- id.df$sunrise + twilight_secs
    id.df$sunset_twilight <- id.df$sunset - twilight_secs
    
    id.df <- id.df %>%
      dplyr::mutate(daylight = ifelse(.data[[timestamp]] >= sunrise_twilight &
                                        .data[[timestamp]] <= sunset_twilight,
                                      "day", "night"))
    
    # Identify the roosts
    id.df <- id.df %>%
      dplyr::mutate(
        is_roost = dplyr::case_when(
          row_id == "last" & daylight == "night" & hour %in% night_hours & ({{ground_speed}} <= 4 |
                                                                              is.na({{ground_speed}})) ~ 1,
          row_id == "first" & daylight == "night" & hour %in% morning_hours & ({{ground_speed}} <= 4 |
                                                                                 is.na({{ground_speed}})) ~ 1,
          dist_km <= buffer ~ 1
        ),
        roost_date = dplyr::case_when(
          is_roost == 1 & row_id == "last" ~ paste(as.character(date)),
          is_roost == 1 & row_id == "first" ~ paste(as.character(date-1))
        ),
        roost_date = as.Date(roost_date)
      )
    
    temp.id.roosts <- dplyr::filter(id.df, is_roost == 1)
    
    # If there is more than 1 roost per day, keep the earliest roost (night roost)
    temp.id.roosts <- temp.id.roosts %>%
      dplyr::group_by(roost_date) %>%
      dplyr::arrange({{timestamp}}) %>%
      dplyr::filter(dplyr::row_number() == 1) %>%
      dplyr::ungroup() %>%
      dplyr::select(-c("row_id", "hour"))
    
    temp.id.roosts <- temp.id.roosts %>%
      dplyr::select({{id}}, date, roost_date, sunrise, sunset, sunrise_twilight, sunset_twilight, daylight, is_roost, location_lat, location_long)
    
    return(temp.id.roosts)
  })
  
  # complete the time message
  if(!quiet){
    end <- Sys.time()
    duration <- difftime(end, start, units = "secs")
    cat(paste0("Roost computation completed in ", duration, " seconds."))
  }
  
  roosts <- roosts %>%
    dplyr::select({{id}}, date, roost_date, sunrise, sunset, sunrise_twilight, sunset_twilight, daylight, is_roost, location_lat, location_long)
  
  # return
  return(roosts)
  
}
