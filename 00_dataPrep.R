# Prepare data for movement analysis
# Setup -------------------------------------------------------------------
## Load packages -----------------------------------------------------------
library(vultureUtils)
library(sf)
library(tidyverse)
library(move)
library(feather)
library(readxl)

## Download data from movebank
# download data from movebank (just a subset of the times for now)
base::load("movebankCredentials/pw.Rda")
MB.LoginObject <- move::movebankLogin(username = "kaijagahm", password = pw)
rm(pw)
minDate <- "2020-09-01 00:00"
maxDate <- "2022-12-15 11:59"
#dat <- vultureUtils::downloadVultures(loginObject = MB.LoginObject, removeDup = T, dfConvert = T, quiet = T, dateTimeStartUTC = minDate, dateTimeEndUTC = maxDate)
#write_feather(dat, "data/dat.feather")
dat <- read_feather("data/dat.feather")
# number of unique individuals
length(unique(dat$trackId)) # 127

## fix trackId
dat2 <- dat %>%
  mutate(trackId = as.character(trackId),
         trackId = case_when(trackId == "E03" ~ "E03w",
                             TRUE ~ trackId))
# number of unique individuals
length(unique(dat2$trackId)) # 126--makes sense that we lost one because one individual had a typo.

  # Add the Nili_id
ww <- read_excel("data/whoswho_vultures_20230315_new.xlsx", sheet = "all gps tags")[,1:35] %>%
  dplyr::select(Nili_id, Movebank_id) %>%
  distinct()

# Note that there is one Nili_id in our dataset that has two associated trackId's--it's yomtov.
ww %>% filter(Movebank_id %in% dat2$trackId) %>% dplyr::select(Nili_id, Movebank_id) %>% distinct() %>% group_by(Nili_id) %>% summarize(n = length(unique(Movebank_id))) %>% arrange(desc(n))
ww %>% filter(Nili_id =="yomtov") # Y26 and Y26b. So we should expect to once again "lose" an individual once we change to Nili ID's

all(dat2$trackId %in% ww$Movebank_id) # true

dat2 <- left_join(dat2, ww, by = c("trackId" = "Movebank_id"))
dim(dat2) # good, same number of rows.
length(unique(dat2$Nili_id))

## annotate the data with periods to remove
toRemove <- read_excel("data/whoswho_vultures_20230315_new.xlsx", sheet = "periods_to_remove")

toRemove <- toRemove %>%
  dplyr::select(Nili_id,
                remove_start,
                remove_end,
                reason) %>%
  mutate(across(c(remove_start, remove_end), .fns = function(x){
    lubridate::ymd(x)
  })) %>%
  dplyr::filter(!is.na(remove_end))

toRemove_long <- toRemove %>%
  group_by(Nili_id, reason) %>%
  # sequence of daily dates for each corresponding start, end elements
  dplyr::mutate(dateOnly = map2(remove_start, remove_end, seq, by = "1 day")) %>%
  # unnest the list column
  unnest(cols = c(dateOnly)) %>%
  # remove any duplicate rows
  distinct() %>%
  dplyr::select(-c(remove_start, remove_end)) %>%
  rename("status" = reason)

# Join to the original data
datAnnot <- dat2 %>%
  left_join(toRemove_long, by = c("Nili_id", "dateOnly")) %>%
  mutate(status = replace_na(status, "valid"))
nrow(datAnnot) == nrow(dat2) #T: good, same number of rows.

# Actually REMOVE the periods to remove...
datAnnot <- datAnnot %>%
  filter(status == "valid")
nrow(datAnnot)
length(unique(datAnnot$Nili_id))
nrow(dat2)
nrow(datAnnot)

# Identify and remove capture dates using Marta's code --------------------
## To identify the capture dates, first we need to classify the roosts (now using the get_roosts() function). Then, if the bird roosted within 500 m of the capture site, it was considered to be captured and that day, and the following day, are excluded from the dataset. This has been validated. Note: this protocol doesn't work for the Carmel because of the position of the roost sites relative to cages. But we're just dealing with southern individuals, so that's okay.
#roosts <- get_roosts_df(df = datAnnot, id = "Nili_id", timestamp = "timestamp", x = "location_long", y = "location_lat", ground_speed = "ground_speed", speed_units = "m/s", quiet = F)
#save(roosts, file = "data/roosts.Rda")
load("data/roosts.Rda")

# Identify the period of time during which the capture sites are open (when we need to do this exclusion)
start.day <- 01
start.month <- 08
end.day <-  30
end.month <- 11
distance <- 500 # distance, in meters, to calculate from the cage

# Load the information about the capture sites
sites <- read.csv("data/capture_sites.csv")

# Subset the roost dataset with the start and end dates for the capture period
sub.roosts <- roosts %>%
  mutate(start_date = as.Date(paste(start.day, start.month, lubridate::year(date), sep="-"),
                              format="%d-%m-%Y"),
         end_date = as.Date(paste(end.day, end.month, lubridate::year(date), sep="-"),
                            format="%d-%m-%Y")) %>%
  filter(date >= start_date & date <= end_date)

unique(lubridate::month(sub.roosts$date)) # now only includes the fall months, which is the capture season.

# then we need to calculate the roost distance to each of the capture cages. if it is less than 500m, keep that line
crds <- matrix(c(sub.roosts$location_long, sub.roosts$location_lat), 
               nrow = nrow(sub.roosts), ncol = 2) # get roost locations as simple lat/long coordinates

DistanceMat <- matrix(ncol = nrow(sites), nrow = nrow(crds)) 
colnames(DistanceMat) <- unique(sites$name)

for(i in 1:nrow(crds)){ # for each roost point...
  DistanceMat[i,] <- round(geosphere::distm(crds[i,], sites[,c(3,2)]), 2) # calculate distance using geosphere::distm
  print(i)
}

ClosestCaptureSite <- colnames(DistanceMat)[apply(DistanceMat,1,which.min)] # ID of closest capture site
ClosestCaptureDist <- apply(DistanceMat,1,min) # Distance from closest capture site

sub.roosts <- cbind(sub.roosts, ClosestCaptureSite, ClosestCaptureDist) 
sub.roosts$Captured <- ifelse(sub.roosts$ClosestCaptureDist <= distance, "Yes", "No")

sub.captured <- subset(sub.roosts, Captured == "Yes") # get list of dates which the bird was inside the cage 

sub.captured.dates <- subset(sub.captured, 
                             select = c("Nili_id",
                                        "date", 
                                        "ClosestCaptureSite",
                                        "ClosestCaptureDist", 
                                        "Captured"))

## This, however, does not work for the Carmel, because the roosts are within 500m of the cage and very often the birds roost on top of the cage without actually being inside it. So for the Carmel captures, we will use another protocol: if the birds were sleeping within 50m of the cage and it was a capture day (or 3 days before the release day), we consider the birds were captured and we remove the data

sub.captured.no.carmel <- subset(sub.captured.dates, ClosestCaptureSite != "Carmel")
sub.captured.carmel <- subset(sub.captured.dates, ClosestCaptureSite == "Carmel")

AllCarmelDates <- read.csv("data/all_captures_carmel_2010-2021.csv")
AllCarmelDates$Date <- as.Date(AllCarmelDates$Date, format = "%d/%m/%Y")

AllCarmelDates.1 <- data.frame(Date = as.Date(paste(AllCarmelDates$Date-1)))
AllCarmelDates.2 <- data.frame(Date = as.Date(paste(AllCarmelDates$Date-2)))
AllCarmelDates.3 <- data.frame(Date = as.Date(paste(AllCarmelDates$Date-3)))

AllCarmelDates.all <- rbind(AllCarmelDates, AllCarmelDates.1, AllCarmelDates.2, AllCarmelDates.3)

sub.captured.carmel <- sub.captured.carmel %>%
  mutate(known_capture = ifelse(date %in% AllCarmelDates.all$Date, 1, 0),
         Captured = ifelse(known_capture == 1 & ClosestCaptureDist <= 50, "yes", "no")) %>%
  filter(Captured == "yes") %>%
  dplyr::select(-c(known_capture))

names(sub.captured.no.carmel)
names(sub.captured.carmel)

sub.captured.dates <- rbind(sub.captured.no.carmel, sub.captured.carmel)

# We also need to exclude the day after the bird was captured
sub.captured.dates.1 <- sub.captured.dates
sub.captured.dates.1$date <- sub.captured.dates.1$date+1

sub.captured.dates <- rbind(sub.captured.dates, sub.captured.dates.1)
sub.captured.dates <- sub.captured.dates %>% 
  dplyr::distinct(Nili_id, date, .keep_all = T)

# It all looks ok, so we can subset the dataset to exclude the capture dates
datAnnot_noCaptures <- datAnnot %>%
  left_join(sub.captured.dates, by = c("Nili_id", "dateOnly" = "date"))
nrow(datAnnot) == nrow(datAnnot_noCaptures) # should be TRUE. NOW we can filter.
datAnnot_noCaptures <- datAnnot_noCaptures %>%
  dplyr::filter(Captured != "Yes"|is.na(Captured)) # remove the individual*days when they were captured

# How many rows and individuals did we remove?
before <- nrow(datAnnot)
after <- nrow(datAnnot_noCaptures)
(propChange <- (before-after)/before) # approx. 1% of data removed.
length(unique(datAnnot$Nili_id))
length(unique(datAnnot_noCaptures$Nili_id)) # no change in number of individuals! As expected.

datAnnot_noCaptures <- datAnnot_noCaptures %>%
  dplyr::select(-c("ClosestCaptureSite", "ClosestCaptureDist", "Captured"))

# Age and sex info --------------------------------------------------------
# Attach age and sex information
as <- read_excel("data/whoswho_vultures_20230315_new.xlsx", sheet = "all gps tags")[,1:35] %>%
  dplyr::select(Nili_id, birth_year, sex) %>%
  distinct()

datAnnot2 <- datAnnot_noCaptures %>%
  dplyr::select(-c("sex")) %>%
  left_join(as, by = "Nili_id")
nrow(datAnnot2) == nrow(datAnnot)
length(unique(datAnnot2$Nili_id)) # 125

datAnnot <- datAnnot2 # a hack so the rest of the code will work

# Clean the data
## Region masking, downsampling, removal of speed outliers, setting altitude outliers to NA, etc.
mask <- sf::st_read("data/CutOffRegion.kml")
datAnnotCleaned <- vultureUtils::cleanData(dataset = datAnnot, mask = mask, inMaskThreshold = 30, removeVars = F, idCol = "Nili_id", downsample = F, reMask = T) # XXX using 30 days overall. If we want 30 days per season, will have to do that differently (e.g. split the seasons earlier)--but I think that that'll be covered later in the data cleaning anyway.
save(datAnnotCleaned, file = "data/datAnnotCleaned.Rda")
load("data/datAnnotCleaned.Rda")
dim(datAnnotCleaned) # about 27 percent of data outside the israel region (roughly)
length(unique(datAnnotCleaned$Nili_id)) # 120 individuals left

# Fix time zone so dates make sense ---------------------------------------
## Overwrite the dateOnly column from the new times
datAnnotCleaned <- datAnnotCleaned %>%
  mutate(timestampIsrael = lubridate::with_tz(timestamp, tzone = "Israel"),
         dateOnly = lubridate::date(timestampIsrael),
         month = lubridate::month(dateOnly),
         year = lubridate::year(dateOnly))

# Split into Marta's 3 seasons --------------------------------------------
s1 <- datAnnotCleaned %>%
  mutate(start_breeding = ifelse(month %in% c(1:5),
                                 paste(as.character(year-1), "-", "12", "-", "15", sep = ""),
                                 paste(as.character(year), "-", "12", "-", "15", sep = "")),
         start_summer = ifelse(month == 12,
                               paste(as.character(year + 1), "-", "05", "-", "15", sep = ""),
                               paste(as.character(year), "-", "05", "-", "15", sep = "")),
         start_fall = as.Date(paste(as.character(year), "-", "09", "-", "15", sep = "")),
         start_breeding = as.Date(start_breeding),
         start_summer = as.Date(start_summer),
         season = case_when(
           dateOnly >= start_breeding & dateOnly < start_summer ~ "breeding",
           dateOnly >= start_summer & dateOnly <= start_fall ~ "summer",
           dateOnly >= start_fall & dateOnly < start_breeding ~ "fall")) %>%
  filter(dateOnly != "2022-12-15") # remove December 15 2022, because it turns out that's a non-inclusive boundary (oops)

s2 <- s1 %>%
  mutate(age = year - birth_year,
         sex = case_when(sex == "f/m" ~ "u",
                         TRUE ~ sex)) %>%
  mutate(age = ifelse(season == "breeding" & month %in% c(1, 12), age + 1, age)) %>%
  group_by(Nili_id, year, month) %>%
  mutate(age = ifelse(month == 2, min(age) + 1, age)) %>%
  mutate(age_group = case_when(age == 0 ~ "juv",
                               age >= 1 & age <= 4 ~ "sub",
                               age >= 5 ~ "adult",
                               TRUE ~ NA)) %>%
  mutate(seasonUnique = case_when(season %in% c("fall", "summer") ~ paste(year, season, sep = "_"),
                                  season == "breeding" & month == 12 ~ paste(year + 1, season, sep = "_"),
                                  season == "breeding" & month != 12 ~ paste(year, season, sep = "_"))) %>%
  # explicitly set the factor levels in temporal order
  mutate(seasonUnique = factor(seasonUnique, levels = c("2020_summer", "2020_fall", "2021_breeding", "2021_summer", "2021_fall", "2022_breeding", "2022_summer", "2022_fall")),
         season = factor(season, levels = c("breeding", "summer", "fall")))
table(s2$seasonUnique, exclude = NULL) # XXX can use this in the report--easy to read

# remove columns that were used in the season calculation
s3 <- s2 %>%
  dplyr::select(-c(month, start_breeding, start_summer, start_fall))

# Separate the seasons -----------------------------
seasons <- s3 %>%
  group_by(seasonUnique) %>%
  group_split(.keep = T)
# extract the season names in case we need a separate vector at some point.
seasonNames <- map_chr(seasons, ~as.character(.x$seasonUnique[1])) # ok good, these are in the right order

# Save copies for social analysis
# The next step will be to remove individuals with too few points per day or too few days tracked. But we don't want those indivs removed for the *social* analysis, since those things will be accounted for with SRI and all individuals make up important parts of the social fabric. So, before I filter for ppd and for days tracked, going to save a copy to use for social analysis. 
seasons_forSoc <- seasons
save(seasons_forSoc, file = "data/seasons_forSoc.Rda")

# Get roosts for each season ----------------------------------------------
roosts_seasons <- purrr::map(seasons, ~vultureUtils::get_roosts_df(df = .x, id = "Nili_id")) 
roosts_seasons <- roosts_seasons %>%
  map(., ~st_as_sf(.x, crs = "WGS84", coords = c("location_long", "location_lat"), remove = F))
save(roosts_seasons, file = "data/roosts_seasons.Rda")

# Remove nighttime points -------------------------------------------------
before <- map_dbl(seasons, nrow)
seasons <- map(seasons, ~{
  times <- suncalc::getSunlightTimes(date = unique(lubridate::date(.x$timestamp)),
                                     lat = 31.434306, lon = 34.991889,
                                     keep = c("sunrise", "sunset")) %>%
    dplyr::select("dateOnly" = date, sunrise, sunset)
  
  .x <- .x %>%
    dplyr::mutate(dateOnly = lubridate::ymd(dateOnly)) %>%
    dplyr::left_join(times, by = "dateOnly") %>%
    dplyr::mutate(daylight = ifelse(timestamp >= sunrise & timestamp <= sunset, "day", "night")) %>%
    dplyr::select(-c(sunrise, sunset)) %>%
    dplyr::filter(daylight == "day")
})
after <- map_dbl(seasons, nrow)
change <- after-before
changeDF <- data.frame(seasonName = seasonNames, nBefore = before, nAfter = after) %>%
  mutate(propRowsRemoved = round((before-after)/before, 2))
changeDF

# Restrict to southern individuals ----------------------------------------
# Based on previous investigations for the 2022 breeding and non-breeding seasons, have found that a good cutoff for southern vs. non-southern is 3550000 (UTM 36N, https://epsg.io/32636)
## Transform to SF object, so we can get centroids
seasonsSF <- map(seasons, ~.x %>%
                   sf::st_as_sf(coords = c("location_long", "location_lat"), remove = F) %>%
                   sf::st_set_crs("WGS84") %>%
                   sf::st_transform(32636))

## Get centroids, so we can see who's "southern" for that season.
centroids <- map(seasonsSF, ~.x %>%
                   group_by(Nili_id) %>%
                   summarize(geometry = st_union(geometry)) %>%
                   st_centroid()) # XXX what is the actual definition of the centroid?

## Examine a histogram of centroid latitudes 
walk(centroids, ~hist(st_coordinates(.x)[,2])) # looks like 3550000 is generally a good cutoff point here.

## Get southern individuals for each season, so we can filter the data
southernIndivs <- map(centroids, ~.x %>%
                        filter(st_coordinates(.)[,2] < 3550000) %>%
                        pull(Nili_id))

## Remove individuals not in the south
before <- seasons
seasons <- map2(.x = seasons, .y = southernIndivs, ~.x %>% filter(Nili_id %in% .y))
after <- seasons

beforeRows <- map_dbl(before, nrow)
afterRows <- map_dbl(after, nrow)
beforeIndivs <- map_dbl(before, ~length(unique(.x$Nili_id)))
afterIndivs <- map_dbl(after, ~length(unique(.x$Nili_id)))
df <- data.frame(season = seasonNames, nBefore = beforeRows, nAfter = afterRows, indivsBefore = beforeIndivs, indivsAfter = afterIndivs)
df$propRowsRemoved <- round((df$nBefore - df$nAfter)/df$nBefore, 2)
df

# Include only individuals with enough points per day ------------------------
# Investigate battery percentage vs points per day ------------------------
# Is there some way we can relate battery charge percentage to number of points per day?
test <- seasons[[5]]
names(test)

ppd <- map_dfr(seasons, ~.x %>%
                 st_drop_geometry() %>%
                 group_by(seasonUnique, season, Nili_id, dateOnly) %>%
                 summarize(n = n(),
                           minBatt = min(battery_charge_percent, na.rm = T),
                           meanBatt = mean(battery_charge_percent, na.rm = T),
                           medBatt = mean(battery_charge_percent, na.rm = T)))

excl_batt <- 50
excl_pts <- 10
## min battery charge
ppd %>%
  filter(n < 100) %>% # only interested in the lower numbers
  mutate(remove = case_when(minBatt <= excl_batt & n <= excl_pts ~ T,
                            TRUE ~ F)) %>%
  ggplot(aes(x = minBatt, y = n))+
  geom_point(aes(col = remove))+
  facet_wrap(~seasonUnique) +
  scale_color_manual(values = c("black", "red")) +
  theme_classic()

# cumulative histogram to look at the results of removing days based only on ppd
ppd %>%
  filter(n < 100) %>%
  ggplot(aes(x = n)) +
  stat_ecdf(aes(y = ..y..*100))+
  ylab("Percentage of vulture-days included")+
  xlab("Points per day")+
  theme_classic()+
  facet_wrap(~seasonUnique)

# cumulative histogram to look at results of removing days based on ppd after battery is taken into account (min pct < 50)
ppd %>%
  filter(n < 100, minBatt > 50) %>%
  ggplot(aes(x = n)) +
  stat_ecdf(aes(y = ..y..*100))+
  ylab("Percentage of vulture-days included")+
  xlab("Points per day")+
  theme_classic()+
  facet_wrap(~seasonUnique)

# same thing, but with batery set at 75
# cumulative histogram to look at results of removing days based on ppd after battery is taken into account (min pct < 50)
ppd %>%
  filter(n < 100, minBatt > 75) %>%
  ggplot(aes(x = n)) +
  stat_ecdf(aes(y = ..y..*100))+
  ylab("Percentage of vulture-days included")+
  xlab("Points per day")+
  theme_classic()+
  facet_wrap(~seasonUnique)

# What if instead we decide we want a certain number of ppd--battery cuttoff may be sharper. Say, 30 ppd: is there a discernible battery cutoff that distinguishes "real" from "battery artifact" days?
ppd %>%
  filter(n < 30) %>%
  ggplot(aes(x = minBatt))+
  stat_ecdf(aes(y = ..y..*100))+
  ylim(c(0, 100))+
  facet_wrap(~seasonUnique)+
  ylab("Percentage of vulture-days included")+
  xlab("Minimum battery percentage") # we do seem to see crooks here around 75%!

# Same thing but with 10 points per day? Predict much sharper cutoffs.
ppd %>%
  filter(n < 10) %>%
  ggplot(aes(x = minBatt))+
  stat_ecdf(aes(y = ..y..*100))+
  ylim(c(0, 100))+
  facet_wrap(~seasonUnique)+
  ylab("Percentage of vulture-days included")+
  xlab("Minimum battery percentage") # crooks around 50% battery minimum for 10ppd. This seems like a pretty good way to go!

# Let's throw out anything with < 10ppd unless the battery has a min charge > 50%.

# Orr suggests to just restrict to vulture*days with >30 points per day. Let's see what that would look like.
thresh <- 10
ppd %>%
  mutate(Status = ifelse(n < thresh, "removed", "kept")) %>%
  ggplot(aes(x = n))+
  geom_histogram(aes(fill = Status))+
  facet_wrap(~seasonUnique, scales ="free")+
  geom_vline(aes(xintercept = thresh), col = "red")+
  scale_fill_manual(values = c("darkgray", "red"))+
  theme_classic()+
  ggtitle(paste("n removed =", thresh, sep = " "))+
  ylab("# vulture*days")+
  xlab("# points per vulture*day") # short days in breeding season, and vultures sitting on nests

ppd <- ppd %>%
  ungroup() %>%
  mutate(removed = ifelse(n < thresh, T, F))

# what proportion of vulture*days are removed overall?
(propRemoved <- ppd %>%
    group_by(seasonUnique, season) %>%
    summarize(nTot = n(),
              nRemoved = sum(removed),
              propRemoved = nRemoved/nTot))
propRemoved %>%
  ggplot(aes(x = seasonUnique, y = propRemoved, col = season))+
  geom_point(size = 10)+
  theme_classic()+
  scale_color_manual(values = c("blue", "red", "darkorange"))

# I'm a bit concerned about this because it removes more data for the breeding season than for the non-breeding seasons (likely because less sunlight during those seasons)

# what about by individual over each season?
ppd %>%
  group_by(seasonUnique, season, Nili_id) %>%
  summarize(propRemoved = sum(removed)/n()) %>%
  ggplot(aes(x = seasonUnique, y = propRemoved))+
  geom_boxplot(aes(group = seasonUnique, fill = season))+
  geom_jitter(width = 0.2, alpha = 0.3, size = 2)+
  scale_fill_manual(name = "Season", values = c("blue", "red", "darkorange"))+
  theme_classic()+ # tells the same story
  ylab("Proportion of days removed")+
  xlab("")

# Based on the battery charge analysis above, going to remove individuals that have 10 or fewer points per day, but only if the minimum battery charge is less than 50%.
# XXXX start here!


## Keep days with more than 10 points, or with fewer than 10 if the minimum battery charge that day is >50%
seasons <- map(seasons, ~.x %>%
                 group_by(Nili_id, dateOnly) %>%
                 filter(n() >= 10 | (n() < 10 & min(battery_charge_percent, na.rm = T) > 50)) %>%
                 ungroup())

# Include only individuals with enough days tracked -----------------------
## Must be tracked for at least 30 days per season
durs <- map_dbl(seasons, ~length(unique(.x$dateOnly)))

walk2(seasons, durs, ~.x %>%
  st_drop_geometry() %>%
  group_by(Nili_id) %>%
  summarize(nDaysTracked = length(unique(dateOnly)),
            propDaysTracked = nDaysTracked/.y) %>%
  pull(propDaysTracked) %>%
  hist())

indivsToKeep <- map2(.x = seasons, .y = durs, ~.x %>%
                       st_drop_geometry() %>%
                       group_by(Nili_id) %>%
                       summarize(nDaysTracked = length(unique(dateOnly)),
                                 propDaysTracked = nDaysTracked/.y) %>%
                       filter(nDaysTracked >= 30) %>%
                       pull(Nili_id))

## remove those individuals not tracked for enough days
before <- seasons
after <- map2(.x = seasons, .y = indivsToKeep, ~.x %>% filter(Nili_id %in% .y))

beforeIndivs <- map_dbl(before, ~length(unique(.x$Nili_id)))
afterIndivs <- map_dbl(after, ~length(unique(.x$Nili_id)))
seasons <- after

## Now I notice that season 1, Summer 2020, doesn't have enough individuals. Let's remove it--we're going to have to anyway.
seasons <- seasons[-1]

# Get elevation rasters ---------------------------------------------------
seasons <- map(seasons, ~st_as_sf(.x, coords = c("location_lat", "location_long"), remove = F, crs = "WGS84"))
# elevs_z10_seasons <- map(seasons, ~elevatr::get_elev_raster(.x , z = 10))
# save(elevs_z10_seasons, file = "data/elevs_z10_seasons.Rda")
load("data/elevs_z10_seasons.Rda")

groundElev_z10 <- map2(elevs_z10_seasons, seasons, ~raster::extract(x = .x, y = .y)) # have to convert the crs because the raster from elevatr is in WGS84.

## use elevation rasters to calculate height above ground level
before <- seasons
seasons <- map2(.x = seasons, .y = groundElev_z10, 
                ~.x %>% mutate(height_above_ground = .y,
                               height_above_ground = case_when(height_above_ground < 0 ~ 0,
                                                               TRUE ~ height_above_ground)))
after <- seasons

# There should be no change in the number of rows
all(map2_lgl(before, after, ~nrow(.x) == nrow(.y)))

# Export the data
save(seasons, file = "data/seasons.Rda")

# Downsample the data and save the downsampled data
subsample <- function(df, idCol = "Nili_id", timestampCol = "timestamp", mins = 10){
  sub <- df %>%
    arrange(idCol, timestampCol) %>%
    group_by(.data[[idCol]]) %>%
    mutate(tc = cut(.data[[timestampCol]], 
                    breaks = paste(as.character(mins), "min", sep = " "))) %>%
    group_by(.data[[idCol]], 
             "d" = lubridate::date(.data[[timestampCol]]), tc) %>%
    slice(1) %>%
    ungroup() %>%
    dplyr::select(-c("d", "tc"))
  return(sub)
}

# create and save downsampled datasets for later use
seasons_10min <- map(seasons, ~subsample(df = .x, idCol = "Nili_id", timestampCol = "timestamp", mins = 10), .progress = T)
seasons_20min <- map(seasons, ~subsample(df = .x, idCol = "Nili_id", timestampCol = "timestamp", mins = 20), .progress = T)
seasons_30min <- map(seasons, ~subsample(df = .x, idCol = "Nili_id", timestampCol = "timestamp", mins = 30), .progress = T)
seasons_60min <- map(seasons, ~subsample(df = .x, idCol = "Nili_id", timestampCol = "timestamp", mins = 60), .progress = T)
seasons_120min <- map(seasons, ~subsample(df = .x, idCol = "Nili_id", timestampCol = "timestamp", mins = 120), .progress = T)

map_dbl(seasons, nrow)
map_dbl(seasons_10min, nrow)

save(seasons_10min, file = "data/seasons_10min.Rda")
save(seasons_20min, file = "data/seasons_20min.Rda")
save(seasons_30min, file = "data/seasons_30min.Rda")
save(seasons_60min, file = "data/seasons_60min.Rda")
save(seasons_120min, file = "data/seasons_120min.Rda")

# no need to calculate roosts for the downsampled data because in theory there's only one point per night.

seasons_10min %>% purrr::list_rbind() %>% group_by(seasonUnique) %>% summarize(vultures = length(unique(Nili_id)))


df <- data.frame(season = seasonNames, 
                 nBefore = map_dbl(seasons, nrow),
                 nAfter = map_dbl(seasons_10min, nrow)) %>%
  mutate(propChange = round((nBefore-nAfter)/nBefore, 2))
