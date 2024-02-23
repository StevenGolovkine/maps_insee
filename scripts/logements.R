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
            scale_cut = append(scales::cut_short_scale(), 1, 1), # workaround with scales 1.3.0
            accuracy = 0.1
        )(N_LOGEMENT)
    )

# Variables
LABELS <- paste0("<", seq(100, 800, 100), "K")
PARIS <- c('Paris', 'Hauts-de-Seine', 'Seine-Saint-Denis', 'Val-de-Marne')

PALETTE <- RColorBrewer::brewer.pal(8, "PuRd")
names(PALETTE) <- levels(df$n_logement_bin)

ADD_LABELS <- TRUE

# Functions
plot_logement <- function(df, add_labels = TRUE, box.padding = 0.05) {
    col_scale <- scale_fill_manual(
        name = "n_logement_bin", values = PALETTE,
        labels = LABELS, drop = FALSE
    )
    
    labels <- ggrepel::geom_label_repel(
        stat = "sf_coordinates",
        fill = alpha(c("white"), 0.5),
        min.segment.length = 0,
        family = "Courier",
        fontface = "bold",
        size = 1,
        box.padding = box.padding,
        label.size = 0.1,
        label.padding = 0.1,
        segment.size = 0.1
    )
    
    gg <- ggplot(
        data = df, 
        aes(
            fill = n_logement_bin,
            label = n_logement_print,
            geometry = geometry
        )
    ) +
        geom_sf() +
        col_scale

    if (add_labels) {
        return(gg + labels)
    } else {
        return(gg)
    }
}



# Metropolitan
fr_metro <- df |> filter(type == 'Metropolitan département')
fr_metro_gg <- plot_logement(
        fr_metro, add_labels = ADD_LABELS, box.padding = 0.05
    ) +
    theme_own() +
    labs(
        title = "Nombre de logements ordinaires en France en 2020",
        subtitle = "Number of regular dwellings in France in 2020",
        caption = "Source: INSEE.fr — Recensement 2020"
    ) +
    theme(legend.title = element_blank())

# Région parisienne
paris <- df |> filter(name %in% PARIS)
paris_gg <- plot_logement(
        paris, add_labels = ADD_LABELS, box.padding = 0.15
    ) +
    theme_void() +
    theme(legend.position = "none")


# La Réunion
reunion <- df |> filter(name == 'La Réunion')
reunion_gg <- plot_logement(reunion, add_labels = ADD_LABELS) +
    theme_void() +
    theme(legend.position = "none")

# Martinique
martinique <- df |> filter(name == 'Martinique')
martinique_gg <- plot_logement(martinique, add_labels = ADD_LABELS) +
    theme_void() +
    theme(legend.position = "none")

# Guadeloupe
guadeloupe <- df |> filter(name == 'Guadeloupe')
guadeloupe_gg <- plot_logement(guadeloupe, add_labels = ADD_LABELS) +
    theme_void() +
    theme(legend.position = "none")

# Guyane
guyane <- df |> filter(name == 'Guyane française')
guyane_gg <- plot_logement(guyane, add_labels = ADD_LABELS) +
    theme_void() +
    theme(legend.position = "none")


# Arrange plots
gg <- fr_metro_gg + 
    ggmagnify::geom_magnify(
        aes(from = name %in% PARIS),
        to = c(7, 9, 49.75, 51.25),
        plot = paris_gg,
        linewidth = 0.1
    ) +
    annotation_custom(
        ggplotGrob(guyane_gg), xmin = -5, xmax = -3, ymin = 45, ymax = 46
    ) +
    annotation_custom(
        ggplotGrob(reunion_gg), xmin = -5, xmax = -3, ymin = 43.75, ymax = 44.75
    ) +
    annotation_custom(
        ggplotGrob(martinique_gg), xmin = -5, xmax = -3, ymin = 42.5, ymax = 43.5
    ) +
    annotation_custom(
        ggplotGrob(guadeloupe_gg), xmin = -5, xmax = -3, ymin = 41.25, ymax = 42.25
    )

ggsave(
    "./figures/logement_labels.png",
    plot = gg,
    width = 1080, height = 1080, units = "px",
    bg = "#E4F4F3"
)

