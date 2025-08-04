library(ggspatial)
library(sfhotspot)
library(ggplot2) 

memphis_grid <- hotspot_grid(memphis_precincts, quiet = TRUE)

robbery_kde <- hotspot_kde(
  memphis_robberies,
  grid = memphis_grid,
  bandwidth_adjust = 0.4
)

ggplot() +
  annotation_map_tile(type = "cartolight", zoomin = 1, progress = "none") +
  autolayer(robbery_kde, alpha = 0.75) +
  geom_sf(data = memphis_precincts, colour = "grey20", fill = NA) +
  scale_fill_distiller(direction = 1) +
  labs(fill = "density of\nrobberies") +
  theme_void() 