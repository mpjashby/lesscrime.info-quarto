library(ggspatial)
library(sfhotspot)
library(ggplot2) 

memphis_grid <- hotspot_grid(memphis_precincts, quiet = TRUE)

robbery_change <- hotspot_change(memphis_robberies, grid = memphis_grid)

ggplot() +
  annotation_map_tile(type = "cartolight", zoomin = 1, progress = "none") +
  geom_sf(aes(fill = change), data = robbery_change, alpha = 0.75, colour = NA) +
  geom_sf(data = memphis_precincts, colour = "grey20", fill = NA) +
  scale_fill_gradient2() +
  labs(fill = "change in\nrobberies") +
  theme_void()
