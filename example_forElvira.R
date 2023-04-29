# Restrict to southern individuals ----------------------------------------
# Based on previous investigations for the 2022 breeding and non-breeding seasons, have found that a good cutoff for southern vs. non-southern is 3550000 (in ITM)
## Transform to SF object, so we can get centroids
sf <- dataset %>%
  sf::st_as_sf(coords = c("location_long", "location_lat"), remove = F) %>%
  sf::st_set_crs("WGS84") %>%
  sf::st_transform(32636))

## Get centroids, so we can see who's "southern" for that season.
centroids <- sf %>% group_by(Nili_id) %>%
  summarize(geometry = st_union(geometry)) %>%
  st_centroid()

## Examine a histogram of centroid latitudes 
hist(st_coordinates(centroids[,2])) # looks like 3550000 is generally a good cutoff point here

## Get southern individuals for each season, so we can filter the data
southernIndividuals <- centroids %>%
  filter(st_coordinates(.)[,2] < 3550000) %>% # you will probably have to add a second filter here to remove the ones that are too far to the south, as we talked about.
  pull(Nili_id)

## Remove individuals from the dataset that are not in the south. Note that even if a "southern individual" has some points that are not in the south, the individual will still be kept and the points will also be kept.
sf <- sf %>% filter(Nili_id %in% southernIndividuals)