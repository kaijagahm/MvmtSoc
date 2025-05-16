library(tidyverse)
library(here)
library(readxl)
library(targets)

#provide the path to the whoswho Excel file
ww_file <- here("data/raw/whoswho_vultures_20230920_new.xlsx")

# Read the 'whoswho' Excel file, sheet named "all gps tags"
ww <- read_excel(ww_file, sheet = "all gps tags")
ww_fixed <- ww %>%
  mutate(across(c("deploy_on_date", "deploy_off_date"), as.numeric))

load(here("data/created/fixed_names.Rda"))
dim(fixed_names)

#provide the path to the fixed_names datasets 
#tar_load(fixed_names)
before <- fixed_names %>%
  group_by(Nili_id) %>%
  summarize(unique_days_before = length(unique(dateOnly)),
            nrows_before = n())

process_deployments <- function(ww,
                                movebank_dataset,#is the fixed_names_dataset_4
                                default_end_date = as.Date("2023-09-15"),
                                verbose = TRUE) {
  #Packages
  requireNamespace("dplyr",     quietly = TRUE)
  requireNamespace("purrr",     quietly = TRUE)
  requireNamespace("lubridate", quietly = TRUE)
  
  
  #Clean deployment dates
  ww <- ww %>%
    dplyr::mutate(
      deploy_on_date  = as.Date(deploy_on_date, origin = "1899-12-30"),
      deploy_off_date = as.Date(deploy_off_date, origin = "1899-12-30")
    )
  
  ww$deploy_off_date[is.na(ww$deploy_off_date)] <- default_end_date
  
  #Remove rows with identical on/off dates
  equal_dates <- ww %>%
    dplyr::filter(!is.na(deploy_on_date),
                  !is.na(deploy_off_date),
                  deploy_on_date == deploy_off_date)
  
  if (verbose && nrow(equal_dates) > 0) {
    message("Check the data: ",
            nrow(equal_dates),
            " record(s) where deploy_on_date equals deploy_off_date. Removing them.")
  }
  
  ww <- dplyr::anti_join(ww, equal_dates,
                         by = c("Nili_id", "deploy_on_date", "deploy_off_date"))
  
  #Flag deploy_off < deploy_on
  invalid_periods <- ww %>%
    dplyr::filter(!is.na(deploy_on_date),
                  !is.na(deploy_off_date),
                  deploy_off_date < deploy_on_date)
  
  if (verbose && nrow(invalid_periods) > 0) {
    message("Warning: Found ",
            nrow(invalid_periods),
            " deployment period(s) where deploy_off_date < deploy_on_date:")
    print(invalid_periods$Nili_id)
  }
  
  #Expand each deployment period to a daily sequence
  periods_to_keep <- ww %>%
    dplyr::select(Nili_id, deploy_on_date, deploy_off_date) %>%
    dplyr::mutate(
      deploy_on_date  = lubridate::ymd(deploy_on_date),
      deploy_off_date = lubridate::ymd(deploy_off_date),
      deploy_off_date = dplyr::if_else(is.na(deploy_off_date),
                                       default_end_date,
                                       deploy_off_date)
    ) %>%
    dplyr::filter(!is.na(deploy_on_date),
                  deploy_off_date >= deploy_on_date) %>%
    dplyr::group_by(Nili_id) %>%
    dplyr::mutate(dateOnly = purrr::map2(
      deploy_on_date, deploy_off_date,
      ~ seq(.x, .y, by = "1 day"))
    ) %>%
    tidyr::unnest(dateOnly) %>%
    dplyr::ungroup() %>%
    dplyr::select(Nili_id, dateOnly) %>%
    dplyr::distinct() %>%
    dplyr::mutate(keep = TRUE)
  
  #Join back to Movebank data
  valid_periods_to_keep <- movebank_dataset %>%
    dplyr::left_join(periods_to_keep, by = c("Nili_id", "dateOnly")) %>%
    filter(keep == TRUE)
  
  return(valid_periods_to_keep)
}

out <- process_deployments(ww_fixed,
                           fixed_names,
                           default_end_date = as.Date("2023-09-15"),
                           verbose = TRUE)

after <- out %>%
  group_by(Nili_id) %>%
  summarize(unique_days_after = length(unique(dateOnly)),
            nrows_after = n())

both <- left_join(before, after)
both %>% filter(unique_days_after != unique_days_before)
both %>% filter(nrows_after != nrows_before) # no more differences in rows