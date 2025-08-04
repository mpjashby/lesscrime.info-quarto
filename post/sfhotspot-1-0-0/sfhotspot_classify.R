library(ggspatial)
library(sfhotspot)
library(ggplot2) 

memphis_grid <- hotspot_grid(memphis_precincts, quiet = TRUE)

robbery_classify <- hotspot_classify(memphis_robberies, grid = memphis_grid)

autoplot(robbery_classify) +
  geom_sf(data = memphis_precincts, colour = "grey20", fill = NA) +
  theme_void()
