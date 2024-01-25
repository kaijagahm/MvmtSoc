load("data/dataPrep/removed_lfr.Rda")
library(tidyverse)
library(sf)

interpFun <- function(df, season){
  rowsToAdd <- data.frame()
  whichGap <- which(df$gap)
  n <- length(whichGap)
  for(i in whichGap){
    cat(paste0(season, ": Interpolating gap ", which(whichGap == i), " of ", n,
               "\n"))
    Nili_id <- df$Nili_id[i]
    start <- lubridate::ymd_hms(df$timestamp[i])
    end <- lubridate::ymd_hms(df$timestamp[i+1])
    timediff <- as.numeric(difftime(end, start, units = "mins"))
    nPeriods <- ceiling(timediff/10)
    nTimes <- nPeriods - 1
    times <- seq.POSIXt(from = start, to = end, length.out = 2+nTimes)
    times <- times[-c(1, length(times))] # remove the anchor times because those are already in the dataset
    newrows <- data.frame(Nili_id = Nili_id,
                          timestamp = times)
    rowsToAdd <- bind_rows(rowsToAdd, newrows)
  }
  return(rowsToAdd)
}

# JUST ONE SEASON OF DATA
season <- removed_lfr[[2]]
pp <- season %>%
  select(Nili_id, dateOnly, timestamp, location_long, location_lat) %>%
  sf::st_as_sf(coords = c("location_long", "location_lat"), crs = "WGS84", remove = FALSE) %>%
  arrange(Nili_id, dateOnly, timestamp) %>%
  group_by(Nili_id, dateOnly) %>%
  mutate(timeToNext = as.numeric(difftime(lead(timestamp), timestamp, units = "mins")),
         distToNext = sf::st_distance(geometry, geometry[row_number()+1],
                                      by_element = T),
         distToNext = as.numeric(distToNext))

gp <- pp %>%
  mutate(gap = case_when(timeToNext > 10 &
                           timeToNext <= 60 &
                           !is.na(timeToNext) &
                           !is.na(lead(timeToNext))&
                           distToNext <= 100
                         ~ TRUE,
                         TRUE ~ FALSE))
whichGap <- which(gp$gap)

toInterpolate <- interpFun(gp, season = "test")

out <- gp %>% 
  bind_rows(toInterpolate %>% mutate(interpolated = TRUE)) %>%
  mutate(interpolated = replace_na(interpolated, FALSE))
out <- out %>%
  arrange(Nili_id, timestamp)
# out_sf <- sf::st_as_sf(out, coords = c("location_long",
#                                        "location_lat"),
#                        crs = "WGS84")
out_mt <- move2::mt_as_move2(out_sf,
                             time_column = "timestamp",
                             track_id_column = "Nili_id")
out_mt_interpolated <- move2::mt_interpolate(out_mt)
table(out_mt_interpolated$interpolated) # how many points we interpolated vs. how many were already there
aa <- out_mt_interpolated %>%
  mutate(location_long = sf::st_coordinates(out_mt_interpolated)[,1],
         location_lat = sf::st_coordinates(out_mt_interpolated)[,2]) # have to restore the coordinate columns, since they won't be filled in for the interpolated points

# Join back to the original data--second season (breeding 2021)
bb <- season
cc <- as.data.frame(aa) %>%
  left_join(bb, by = c("Nili_id", "dateOnly", "timestamp", "location_long", "location_lat")) %>%
  fill(dateOnly, .direction = "down") %>% # fill in the dateOnly values
  fill(seasonUnique, .direction = "down") %>%
  fill(season, .direction = "down")
nrow(cc) == nrow(aa)

#r_cc <- vultureUtils::get_roosts_df(cc, id = "Nili_id") # get some roosts

# remove nighttime points
cc_times <- suncalc::getSunlightTimes(date = unique(lubridate::date(cc$timestamp)),
                                      lat = 31.434306, lon = 34.991889,
                                      keep = c("sunrise", "sunset")) %>%
  select("dateOnly"=date, sunrise,sunset)
dd <- cc %>%
  left_join(cc_times, by = "dateOnly") %>%
  dplyr::mutate(daylight = ifelse(timestamp >= sunrise & timestamp <= sunset, "day", "night")) %>%
  dplyr::filter(daylight == "day")
(nrow(cc)-nrow(dd))/nrow(cc) # removed 2.3% of the rows

dd_sf <- dd %>%
  sf::st_as_sf(coords = c("location_long", "location_lat"), remove = F) %>%
  sf::st_set_crs("WGS84") %>%
  sf::st_transform(32636)
dd_centroids <- dd_sf %>%
  group_by(Nili_id) %>%
  summarize(geometry = sf::st_union(geometry)) %>%
  sf::st_centroid()
sthrn <- dd_centroids %>% filter(sf::st_coordinates(.)[,2] < 3550000) %>%
  pull(Nili_id)
ee <- dd %>% filter(Nili_id %in% sthrn)
(nrow(dd)-nrow(ee))/nrow(dd) # removed 4% of rows

ee_ppd <- ee %>%
  group_by(season, seasonUnique, Nili_id, dateOnly) %>%
  summarize(n = n(),
            minBatt = min(battery_charge_percent, na.rm = T))
ff <- ee %>%
  group_by(Nili_id, dateOnly) %>%
  filter(n() >= 10 | (n() < 10 & min(battery_charge_percent, na.rm = T) > 50)) %>%
  ungroup()

tokeep <- ff %>%
  sf::st_drop_geometry() %>%
  group_by(Nili_id) %>%
  summarize(nDaysTracked = length(unique(dateOnly))) %>%
  filter(nDaysTracked >= 30) %>%
  pull(Nili_id)

## remove those individuals not tracked for enough days
gg <- ff %>%
  filter(Nili_id %in% tokeep)

length(tokeep)
length(unique(ff$Nili_id)) # retained 28 out of 33 individuals

df <- data.frame(rows = c(nrow(bb), nrow(cc), nrow(dd), nrow(ee), nrow(ff), nrow(gg)),
                 indivs = c(length(unique(bb$Nili_id)), length(unique(cc$Nili_id)), length(unique(dd$Nili_id)), length(unique(ee$Nili_id)), length(unique(ff$Nili_id)), length(unique(gg$Nili_id))),
                 step = c("1_after removing lfr indivs", "2_after interpolating", "3_after removing nighttime points", "4_after removing southern indivs", "5_after removing days with too few points", "6_after removing indivs not tracked for enough days"),
                 group = "interpolated")

df %>%
  ggplot(aes(x = step, y = indivs))+
  geom_point()+
  geom_line(aes(group = group))+
  scale_x_discrete(labels = label_wrap_gen(width = 20))+
  theme_classic()

  
####################
# Now compare this result to how many individuals were retained in the real dataset
load("data/dataPrep/removed_lfr.Rda")
BB <- removed_lfr[[2]]
load("data/dataPrep/removed_nighttime.Rda")
DD <- removed_nighttime[[2]]
rm(removed_nighttime)
load("data/dataPrep/removed_northern.Rda")
EE <- removed_northern[[2]]
rm(removed_northern)
load("data/dataPrep/removed_lowppd.Rda")
FF <- removed_lowppd[[2]]
rm(removed_lowppd)
load("data/dataPrep/removed_too_few_days.Rda")
GG <- removed_too_few_days[[2]]
rm(removed_too_few_days)

DF <- data.frame(rows = c(nrow(BB), NA, nrow(DD), nrow(EE), nrow(FF), nrow(GG)),
                 indivs = c(length(unique(BB$Nili_id)), NA, length(unique(DD$Nili_id)), length(unique(EE$Nili_id)), length(unique(FF$Nili_id)), length(unique(GG$Nili_id))),
                 step = c("1_after removing lfr indivs", "2_after interpolating", "3_after removing nighttime points", "4_after removing southern indivs", "5_after removing days with too few points", "6_after removing indivs not tracked for enough days"),
                 group = "no interpolation")

dDfF <- bind_rows(df, DF)

dDfF %>%
  ggplot(aes(x = factor(step), y = indivs, group = group, col = factor(group)))+
  geom_point()+
  geom_line()+
  scale_x_discrete(labels = label_wrap_gen(width = 20))+
  theme_classic() # welp

dDfF %>%
  ggplot(aes(x = factor(step), y = rows, group = group, col = factor(group)))+
  geom_point()+
  geom_line()+
  scale_x_discrete(labels = label_wrap_gen(width = 20))+
  theme_classic() # as expected, interpolation increases the number of points, but 

# Note that this interpolation was performed AFTER the removing-low-fix-rate-individuals step, which seems counter-intuitive at first. But this is actually important! We weren't trying to interpolate points in order to fix those gaps, because those gaps are caused by tags functioning normally that are just set to a low fix rate. The gaps we're trying to fix are the ones that were caused by e.g. missed tag reads in an otherwise normal tag.


length(unique(forComparison$Nili_id)) # 28 individuals retained for this season
