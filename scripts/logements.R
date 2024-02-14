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
    inner_join(logement, by = join_by(depart_code == REGION))
df <- df |>
    mutate(n_logement_bin = cut(N_LOGEMENT, breaks = seq(0, 8e5, by = 1e5))) |> 
    mutate(n_logement_print = scales::label_number(
        scale_cut = scales::cut_short_scale(), accuracy = 0.1)(N_LOGEMENT)
    )


# Plot France
myPalette <- RColorBrewer::brewer.pal(8, "PuRd")
names(myPalette) <- levels(df$n_logement_bin)
LABELS <- paste0("< ", seq(100, 800, 100), "K")
col_scale <- scale_fill_manual(
    name = "n_logement_bin", values = myPalette,
    labels = LABELS, drop = FALSE
)

labels <- ggrepel::geom_label_repel(
    stat = "sf_coordinates",
    fill = alpha(c("white"), 0.5),
    min.segment.length = 0,
    max.overlaps = 4,
    color = "black",
    segment.color = "black",
    family = "Courier",
    fontface = "bold",
    size = 1,
    box.padding = 0.05,
    label.size = 0.1,
    segment.size = 0.1
)

# Metropolitan
fr_metro <- df |> filter(type == 'Metropolitan département')
fr_metro_gg <- ggplot(
        data = fr_metro,
        aes(fill = n_logement_bin, label = n_logement_print, geometry = geometry)
    ) +
    geom_sf() +
    labels +
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
reunion_gg <- ggplot(
        data = reunion,
        aes(fill = n_logement_bin, label = n_logement_print, geometry = geometry)
    ) +
    geom_sf() +
    labels +
    col_scale +
    theme_void() +
    theme(legend.position = "none")

# Martinique
martinique <- df |> filter(name == 'Martinique')
martinique_gg <- ggplot(
        data = martinique,
        aes(fill = n_logement_bin, label = n_logement_print, geometry = geometry)
    ) +
    geom_sf() +
    labels + 
    col_scale +
    theme_void() +
    theme(legend.position = "none")

# Guadeloupe
guadeloupe <- df |> filter(name == 'Guadeloupe')
guadeloupe_gg <- ggplot(
        data = guadeloupe,
        aes(fill = n_logement_bin, label = n_logement_print, geometry = geometry)
    ) +
    geom_sf() +
    labels +
    col_scale +
    theme_void() +
    theme(legend.position = "none")

# Guyane
guyane <- df |> filter(name == 'Guyane française')
guyane_gg <- ggplot(
        data = guyane,
        aes(fill = n_logement_bin, label = n_logement_print, geometry = geometry)
    ) +
    geom_sf() +
    labels + 
    col_scale +
    theme_void() +
    theme(legend.position = "none")


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
    "./figures/logement.png",
    width = 1080, height = 1080, units = "px",
    bg = "white"
)
