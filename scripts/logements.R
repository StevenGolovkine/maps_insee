################################################################################
# Logements
################################################################################

library(ggpubr)
library(maps)
library(tidyverse)
library(sf)

source('./scripts/theme.R')

# Load France shapefile
fr <- read_rds('./data/france.rds')

# Load logement file
logement <- read_csv('./data/logements.csv')

# Join
df <- fr |> inner_join(logement, by = join_by(depart_code == REGION))

# Plot France
myPalette <- colorRampPalette(RColorBrewer::brewer.pal(11, "Purples"))

# Metropolitan
fr_metro <- df |> filter(type == 'Metropolitan département')
fr_metro_gg <- ggplot() +
    geom_sf(data = fr_metro, aes(fill = N_LOGEMENT)) +
    scale_fill_gradientn(
        colours = myPalette(100), limits = c(0, 1e6)
    ) +
    labs(
        title = "Nombre de logements ordinaires en France en 2020",
        subtitle = "Source: INSEE.fr — Recensement 2020"
    ) +
    theme_own() +
    theme(
        legend.title = element_blank()
    ) +
    guides(fill = guide_colourbar(barwidth = 15, barheight = 1))

# La Réunion
reunion <- df |> filter(name == 'La Réunion')
reunion_gg <- ggplot() +
    geom_sf(data = reunion, aes(fill = N_LOGEMENT)) +
    scale_fill_gradientn(colours = myPalette(100), limits = c(0, 1e6)) +
    theme_void() +
    theme(legend.position = "none")

# Martinique
martinique <- df |> filter(name == 'Martinique')
martinique_gg <- ggplot() +
    geom_sf(data = martinique, aes(fill = N_LOGEMENT)) +
    scale_fill_gradientn(colours = myPalette(100), limits = c(0, 1e6)) +
    theme_void() +
    theme(legend.position = "none")

# Guadeloupe
guadeloupe <- df |> filter(name == 'Guadeloupe')
guadeloupe_gg <- ggplot() +
    geom_sf(data = guadeloupe, aes(fill = N_LOGEMENT)) +
    scale_fill_gradientn(colours = myPalette(100), limits = c(0, 1e6)) +
    theme_void() +
    theme(legend.position = "none")

# Guyane
guyane <- df |> filter(name == 'Guyane française')
guyane_gg <- ggplot() +
    geom_sf(data = guyane, aes(fill = N_LOGEMENT)) +
    scale_fill_gradientn(colours = myPalette(100), limits = c(0, 1e6)) +
    theme_void() +
    theme(legend.position = "none")


# Arrange plots
seas <- ggarrange(
    plotlist = list(
        reunion_gg, martinique_gg, guadeloupe_gg, guyane_gg
    ),
    ncol = 1, nrow = 4
)

ggarrange(
    plotlist = list(fr_metro_gg, seas),
    common.legend = TRUE, legend = "bottom",
    ncol = 2, nrow = 1,
    widths = c(6, 1)
)

