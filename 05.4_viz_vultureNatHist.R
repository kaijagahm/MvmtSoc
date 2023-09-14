# Vulture natural history visualizations, general
# 2023-09-13

library(vultureUtils)
library(tidyverse)
library(mapview)
library(sf)
library(ggspatial)
library(grid)
library(ggmap)
library(Polychrome)

# Roosts map --------------------------------------------------------------
r <- sf::st_read("data/roosts50_kde95_cutOffRegion.kml")
# add an identifier column
r$id <- factor(1:nrow(r))
mapview(r, zcol = "id") # quick and dirty mapview
ggplot(r) + 
  geom_sf(aes(fill = id, col = id))+ 
  theme_minimal()+
  theme(legend.position = "none") # quick and dirty ggplot

# now adding a background map
ggplot(r) + 
  ggspatial::annotation_map_tile("cartolight", zoom = 9)+
  geom_sf(aes(fill = id, col = id))+ 
  theme_minimal()+
  theme(legend.position = "none")

# now restricting to a smaller area
# rosm::osm.types() # to see a list of the different types
# opencycle is good and shows terrain
# cartolight and cartodark are fine but unremarkable
# stamenwatercolor is pretty but not that useful
# thunderforestlandscape is good and also shows terrain. Needs API key
r_cropped <- st_crop(r, xmin = 34.7, xmax = 35.4,
                        ymin = 30.58, ymax = 31.18)

# get a palette of distinct colors
colors <- Polychrome::glasbey.colors(nrow(r_cropped)+1) # don't want white, so took 19 colors (1 more than we have roost polygons in the cropped set below.)
colors <- colors[-1]
swatch(colors)
colors <- as.character(colors)

roostmap_1 <- ggmap(get_stamenmap(bbox = c(left = 34.6, bottom = 30.53, right = 35.5, top = 31.18), maptype = "terrain", zoom = 9)) + 
  theme_void()+
  geom_sf(data = r_cropped, inherit.aes = F, aes(fill = id))+
  theme(legend.position = "none")+
  scale_fill_manual(values = colors)+
  annotation_scale(location = "br")
roostmap_1
ggsave(roostmap_1, filename = "fig/roostmap_1.png")

# Now let's do another one that's even more zoomed in
mapview(r)

r_cropped_2 <- st_crop(r, xmin = 34.7, xmax = 35.2,
                     ymin = 30.57, ymax = 30.88)

# get a palette of distinct colors
colors <- Polychrome::glasbey.colors(nrow(r_cropped_2)+1) # don't want white, so took 19 colors (1 more than we have roost polygons in the cropped set below.)
colors <- colors[-1]
swatch(colors)
colors <- as.character(colors)

roostmap_2 <- ggmap(get_stamenmap(bbox = c(left = 34.7, bottom = 30.57, right = 35.2, top = 30.88), maptype = "terrain", zoom = 9)) + 
  theme_void()+
  geom_sf(data = r_cropped_2, inherit.aes = F, aes(fill = id))+
  theme(legend.position = "none")+
  scale_fill_manual(values = colors)+
  annotation_scale(location = "br")
roostmap_2
ggsave(roostmap_2, filename = "fig/roostmap_2.png")

# Feeding stations map ----------------------------------------------------
fs <- read_csv("data/feeding_station_south_coordinates.csv")
fs$lat <- as.numeric(str_trim(fs$lat))
fs$long <- as.numeric(str_trim(fs$long))
fs_ll <- sf::st_as_sf(fs, coords = c("long", "lat"), crs = "WGS84")
cs <- read_csv("data/capture_sites.csv")
cs_ll <- sf::st_as_sf(cs, coords = c("long", "lat"), crs = "WGS84")

mapview(fs_utm) # quick and dirty map

r_cropped_3 <- st_crop(r, xmin = 34.29, xmax = 35.57,
                       ymin = 30.3, ymax = 31.40)
feedingstationmap_1 <- ggmap(get_stamenmap(bbox = c(left = 34.29, bottom = 30.3, right = 35.57, top = 31.40), maptype = "terrain", zoom = 9)) + 
  theme_void()+
  theme(legend.position = "none")+
  geom_sf(data = r_cropped_3, inherit.aes = F, fill = "purple")+
  geom_sf(data = fs_ll, inherit.aes = F, aes(geometry = geometry), color = "red", size = 2, alpha = 0.8)+
  geom_sf(data = cs_ll, inherit.aes = F, aes(geometry = geometry), color = "yellow", size = 3, pch = 8)+
  annotation_scale(location = "br")
feedingstationmap_1
ggsave(feedingstationmap_1, file = "fig/feedingstationmap_1.png", width = 6)

# Interactions map --------------------------------------------------------







