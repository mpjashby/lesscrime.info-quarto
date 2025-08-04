library(ggspatial)
library(sfhotspot)
library(ggplot2) 

memphis_grid <- hotspot_grid(memphis_precincts, quiet = TRUE)

robbery_gistar <- hotspot_gistar(
  st_transform_auto(memphis_robberies),
  grid = st_transform_auto(memphis_grid),
  bandwidth_adjust = 0.4
) |> 
  dplyr::filter(gistar > 0, pvalue < 0.05)

ggplot() +
  annotation_map_tile(type = "cartolight", zoomin = 1, progress = "none") +
  geom_sf(aes(fill = kde), data = robbery_gistar, alpha = 0.75, colour = NA) +
  geom_sf(data = memphis_precincts, colour = "grey20", fill = NA) +
  scale_fill_distiller(direction = 1) +
  labs(fill = "density of\nrobberies") +
  theme_void()