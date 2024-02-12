################################################################################
# Create maps of France
################################################################################

library(ggpubr)
library(maps)
library(tidyverse)
library(sf)

# Load France shapefile
fr <- rnaturalearth::ne_states("france")


# Plot France

# Metropolitan
fr_metro <- fr |> filter(type_en == 'Metropolitan department')
fr_metro_gg <- ggplot() +
    geom_sf(data = fr_metro) +
    theme_void()

# Mayotte
mayotte <- fr |> filter(name == 'Mayotte')
mayotte_gg <- ggplot() +
    geom_sf(data = mayotte) +
    theme_void()

# La Réunion
reunion <- fr |> filter(name == 'La Réunion')
reunion_gg <- ggplot() +
    geom_sf(data = reunion) +
    theme_void()

# Martinique
martinique <- fr |> filter(name == 'Martinique')
martinique_gg <- ggplot() +
    geom_sf(data = martinique) +
    theme_void()

# Guadeloupe
guadeloupe <- fr |> filter(name == 'Guadeloupe')
guadeloupe_gg <- ggplot() +
    geom_sf(data = guadeloupe) +
    theme_void()

# Guyane
guyane <- fr |> filter(name == 'Guyane française')
guyane_gg <- ggplot() +
    geom_sf(data = guyane) +
    theme_void()


# Arrange plots
seas <- ggarrange(
    plotlist = list(
        mayotte_gg, reunion_gg, martinique_gg, guadeloupe_gg, guyane_gg
    ),
    ncol = 1, nrow = 5
)

ggarrange(
    plotlist = list(fr_metro_gg, seas),
    ncol = 2, nrow = 1,
    widths = c(5, 1)
)
