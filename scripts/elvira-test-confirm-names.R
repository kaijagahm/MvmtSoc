# The problem: different numbers of vultures in Elvira's data and my data
# The example: Fall 2020, there are several vultures
kaija_but_not_elvira <- c("chegovar", "juno", "levy", "sabath", "tammy", "tyra", "uri")
# There are also some that elvira had that Kaija didn't, but let's save that as a separate problem.

# Function definitions--Elvira. Download data just for Fall 2020
# Download data for 2020
get_inpa <- function(loginObject){
  inpa <- move::getMovebankData(study = 6071688, 
                                login = loginObject, 
                                removeDuplicatedTimestamps = TRUE,
                                timestamp_start = "2020091500000",
                                timestamp_end = "2020121400000")
  inpa <- methods::as(inpa, "data.frame")
  inpa <- inpa %>%
    mutate(dateOnly = lubridate::ymd(substr(timestamp, 1, 10)),
           year = as.numeric(lubridate::year(timestamp)))
  return(inpa)
}

get_ornitela <- function(loginObject){
  minDate <- "2020-09-15 00:00"
  maxDate <- "2020-12-14 11:59"
  ornitela <- downloadVultures(loginObject = loginObject, 
                               removeDup = T, dfConvert = T, 
                               quiet = T, 
                               dateTimeStartUTC = minDate, 
                               dateTimeEndUTC = maxDate)
  return(ornitela)
}

# Kaija's data (previously)
targets::tar_load(fixed_names)
fixed_names_kaija <- fixed_names %>%
  filter(dateOnly >= "2020-09-15" & dateOnly <= "2020-12-14")
trouble %in% fixed_names_kaija$Nili_id # all the names are present here.

# Elvira's data (I don't have access to this directly, but see above for trouble list). Can ask her for the file if I need to confirm.

# Re-downloading with Elvira's functions above)
pw <- "credentials/pw.Rda"
loginObject <- get_loginObject(pw)
inpa <- get_inpa(loginObject)
ornitela <- get_ornitela(loginObject)
joined0 <- join_inpa_ornitela(inpa, ornitela)
fixed_names_new <- fix_names(joined0, ww_file = "data/raw/whoswho_vultures_20230920_new.xlsx")
trouble %in% fixed_names_new$Nili_id # the names are missing from this dataset when we download it using Elvira's functions above.

# Checking on INPA website
# using fixed_names, not fixed_names_new, to extract this info about the missing vultures, because they are not included in fixed_names_new.
fixed_names %>%
  filter(Nili_id %in% trouble) %>%
  select(Nili_id, dataset) %>%
  distinct() # we see that these are all INPA vultures that are missing, so let's go check on Movebank to make sure they are all there.

fixed_names %>%
  filter(Nili_id %in% trouble) %>%
  select(Nili_id, dataset, local_identifier, tag_id, tag_local_identifier) %>%
  distinct() # okay, time to check on movebank to make sure those are there, and then we can start trying to figure out why they are not appearing.

## There does not seem to be a way to search individuals by date on the map, so I'm going to download the data between the exact dates that Elvira specified above and then we can search in that download.

## Downloading data from 2020-09-15 through 2020-12-14.

## Now searching in the CSV.
# J29 White (chegovar): YES but "J29w" instead of "J29 White"
# J52 White (juno): YES but "J52w" instead of "J52 White"
# S94>A99W (sabath): NO--at the time, the tag 190123 was on a bird B55w. B55w does not appear on the who's who.
## This makes me wonder: does the data include J62?
fixed_names_new %>% filter(grepl("J62", trackId)) %>% pull(Nili_id) %>% unique() # aha, these are in the dataset but their Nili_ids are NA! Could that also be what's going on with the other missing one?
# T08 B (A73>Y31): uri: seems to be in the data as B41w (A73>Y31>T08 B>J62 W). In the who's who, this is encoded as J62 (uri).
fixed_names_new %>% filter(grepl("Y31", trackId)) %>% pull(Nili_id) %>% unique() # also encoded as NA! This seems to maybe be a Nili id problem rather than a tag_local_identifier problem. The data are not actually missing, they're just incorrectly encoded.
# T24 B (J63>Y 65>R47>P05>S44) (levy): YES but it's "T24b" instead of "T24 B" so maybe that's the problem?
# T35 White (tyra): Yes but "B43w (T35 White)"
# Y01>T60 W (tammy): in the data as "B01w (Y01>T60 W)", but in the who's who as "Y01>T60" or "Y01>T60 white".

# this raises the question: how many NAs are in the Nili_id column of my data and of Elvira's data?
sort(unique(fixed_names_kaija$Nili_id)) # I don't see any NAs here
sum(is.na(fixed_names_kaija$Nili_id)) # there are a lot of NAs here, yikes!
sum(is.na(fixed_names_new$Nili_id)) # there are more NAs here than in mine, but we have NAs in both. That's scary.

# Let's figure out where those NAs got excluded in my data process.
#targets::tar_load(cleaned)
table(cleaned$Nili_id, exclude = NULL) # there are a lot of NAs at this step.
#targets::tar_load(removed_captures)
table(removed_captures$Nili_id, exclude = NULL) # still a lot here
#targets::tar_load(with_age_sex)
table(with_age_sex$Nili_id, exclude = NULL) # still a lot here
#targets::tar_load(seasons_list)
table(seasons_list[[1]]$Nili_id, exclude = NULL) # still some here
map_dbl(seasons_list, ~sum(is.na(.x$Nili_id)))# a bunch of NAs here
### XXX START HERE--are these actually northern birds? How many are southern?
map_dbl(seasons_list, ~.x %>% filter(is.na(Nili_id)) %>% pull(tag_id) %>% unique() %>% length()) # this is how many individuals are affected in each season.

seasons_sf <- map(seasons_list, ~.x %>%
                    sf::st_as_sf(coords = c("location_long", "location_lat"), 
                                 remove = F) %>%
                    sf::st_set_crs("WGS84") %>%
                    sf::st_transform(32636))

centroids <- map(seasons_sf, ~.x %>%
                   group_by(factor(tag_id), Nili_id) %>%
                   summarize(geometry = st_union(geometry)) %>%
                   st_centroid()) # calculating centroids by tag_id and also Nili_id

centroids2 <- map(centroids, ~.x %>% bind_cols(st_coordinates(.x)) %>% mutate(southern = ifelse(Y < 3550000, T, F)))
View(centroids2[[1]])

# How many of the NA individuals are southern?
map_dbl(centroids2, ~.x %>% filter(is.na(Nili_id), southern == T) %>% nrow()) # THANK GOD

# Ok so there were a bunch of NAs in my data, as well as in Elvira's current data, that resulted from bad joins with Nili_ids in the who's who. I don't quite understand why that's the case, but either we need to fix them or we need to use a different column other than Nili_id for this. The reason we didn't use tag_id was because the same tag could be put on different individuals, I think

# Now what about the ones that Elvira had that I didn't have? I strongly suspect that it's going to be the same story--bad joins.

#targets::tar_load(removed_northern)
table(removed_northern[[1]]$Nili_id, exclude = NULL)
map_dbl(removed_northern, ~sum(is.na(.x$Nili_id))) # no more NAs at this step.
#targets::tar_load(removed_lfr)
map_dbl(removed_lfr, ~sum(is.na(.x$Nili_id)))# no more NAs at this step
#targets::tar_load(downsampled_10min)
table(downsampled_10min[[1]]$Nili_id, exclude = NULL)

# Now checking the other way--individuals missing from kaija that  --------
trouble2 <- c("kfir", "yagur")
fixed_names_new %>%
  filter(Nili_id %in% trouble2) %>%
  select(Nili_id, tag_id, tag_local_identifier, local_identifier, dataset) %>% 
  distinct() # I cannot find kfir and yagur, which means that for those two individuals I cannot reproduce finding them in the new download.

# Elvira checked again and it turns out that kfir and yagur are not in her fall 2020 dataset either. This probably means that we were looking at the wrong season when we initially did this comparison.



