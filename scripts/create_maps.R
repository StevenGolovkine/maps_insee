################################################################################
# Create maps of France
################################################################################

library(maps)
library(tidyverse)
library(sf)

# Load France shapefile
fr <- rnaturalearth::ne_states("france")


# Plot France
ggplot() +
    geom_sf(data = fr)

