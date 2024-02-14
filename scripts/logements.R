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
df <- fr |> 
    inner_join(logement, by = join_by(depart_code == REGION)) |> 
    mutate(n_logement_bin = cut(df$N_LOGEMENT, breaks = seq(0, 8e5, by = 1e5)))


# Plot France
myPalette <- RColorBrewer::brewer.pal(8, "PuRd")
names(myPalette) <- levels(df$n_logement_bin)
LABELS <- paste0("< ", seq(100, 800, 100), "K")
col_scale <- scale_fill_manual(
    name = "n_logement_bin", values = myPalette,
    labels = LABELS, drop = FALSE
)


# Metropolitan
fr_metro <- df |> filter(type == 'Metropolitan département')
fr_metro_gg <- ggplot() +
    geom_sf(data = fr_metro, aes(fill = n_logement_bin)) +
    col_scale +
    theme_own() +
    theme(
        legend.title = element_blank(),
        legend.text = element_text(size = 6, family = "Courier"),
        legend.position = c(0.1, 0.25),
        legend.key.size = unit(0.5, "line")
    )

# La Réunion
reunion <- df |> filter(name == 'La Réunion')
reunion_gg <- ggplot() +
    geom_sf(data = reunion, aes(fill = n_logement_bin)) +
    col_scale +
    theme_void() +
    theme(
        legend.position = "none"
    )

# Martinique
martinique <- df |> filter(name == 'Martinique')
martinique_gg <- ggplot() +
    geom_sf(data = martinique, aes(fill = n_logement_bin)) +
    col_scale +
    theme_void() +
    theme(
        legend.position = "none"
    )

# Guadeloupe
guadeloupe <- df |> filter(name == 'Guadeloupe')
guadeloupe_gg <- ggplot() +
    geom_sf(data = guadeloupe, aes(fill = n_logement_bin)) +
    col_scale +
    theme_void() +
    theme(
        legend.position = "none"
    )

# Guyane
guyane <- df |> filter(name == 'Guyane française')
guyane_gg <- ggplot() +
    geom_sf(data = guyane, aes(fill = n_logement_bin)) +
    col_scale +
    theme_void() +
    theme(
        legend.position = "none"
    )


# Arrange plots
seas <- ggarrange(
    plotlist = list(
        reunion_gg, martinique_gg, guadeloupe_gg, guyane_gg
    ),
    ncol = 1, nrow = 4
)

pp <- ggarrange(
    plotlist = list(fr_metro_gg, seas),
    ncol = 2, nrow = 1,
    widths = c(6, 1)
)
annotate_figure(
    pp,
    top = text_grob(
        "Nombre de logements ordinaires en France en 2020",
        family = "Courier",
        size = 8
    ),
    bottom = text_grob(
        "Source: INSEE.fr — Recensement 2020",
        family = "Courier", hjust = 1, x = 1, size = 6
    )
) + theme(plot.margin = margin(0.1, 0.1, 0.1, 0.1, "cm")) 

ggsave(
    "./figures/logement.png", width = 1080, height = 1080, units = "px"
)
