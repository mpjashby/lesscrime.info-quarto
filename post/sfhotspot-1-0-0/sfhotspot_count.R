library(ggspatial)
library(sfhotspot)
library(ggplot2) 

memphis_grid <- hotspot_grid(memphis_precincts, quiet = TRUE)

robbery_count <- hotspot_count(memphis_robberies, grid = memphis_grid) 

ggplot() +
  annotation_map_tile(type = "cartolight", zoomin = 1, progress = "none") +
  autolayer(robbery_count, alpha = 0.75) +
  geom_sf(data = memphis_precincts, colour = "grey20", fill = NA) +
  scale_fill_distiller(direction = 1) +
  labs(fill = "count of\npoints in\neach cell") +
  theme_void()