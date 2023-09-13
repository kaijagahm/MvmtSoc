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

roostmap_1 <- ggmap(get_stamenmap(bbox = c(left = 34.6, bottom = 30.53, right = 35.5, top = 31.18), maptype = "terrain-background", zoom = 10)) + 
  theme_void()+
  geom_sf(data = r_cropped, inherit.aes = F, aes(fill = id))+
  theme(legend.position = "none")+
  scale_fill_manual(values = colors)
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

roostmap_2 <- ggmap(get_stamenmap(bbox = c(left = 34.7, bottom = 30.57, right = 35.2, top = 30.88), maptype = "terrain-background", zoom = 10)) + 
  theme_void()+
  geom_sf(data = r_cropped_2, inherit.aes = F, aes(fill = id))+
  theme(legend.position = "none")+
  scale_fill_manual(values = colors)
roostmap_2
ggsave(roostmap_2, filename = "fig/roostmap_2.png")




# Feeding stations map ----------------------------------------------------
# Capture sites map -------------------------------------------------------
# Interactions map --------------------------------------------------------







