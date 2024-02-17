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

# Variables
LABELS <- paste0("< ", seq(100, 800, 100), "K")
PARIS <- c('Paris', 'Hauts-de-Seine', 'Seine-Saint-Denis', 'Val-de-Marne')

PALETTE <- RColorBrewer::brewer.pal(8, "PuRd")
names(PALETTE) <- levels(df$n_logement_bin)

# Functions
plot_logement <- function(df, box.padding = 0.05) {
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
    
    ggplot(
        data = df, 
        aes(
            fill = n_logement_bin,
            label = n_logement_print,
            geometry = geometry
        )
    ) +
        geom_sf() +
        labels +
        col_scale
}



# Metropolitan
fr_metro <- df |> filter(type == 'Metropolitan département')
fr_metro_gg <- plot_logement(fr_metro, box.padding = 0.05) +
    theme_own() +
    theme(
        legend.title = element_blank(),
        legend.text = element_text(size = 6, family = "Courier"),
        legend.position = c(0.925, 0.5),
        legend.key.size = unit(0.5, "line")
    )

# Région parisienne
paris <- df |> filter(name %in% PARIS)
paris_gg <- plot_logement(paris, box.padding = 0.1) +
    theme_void() +
    theme(legend.position = "none")


# La Réunion
reunion <- df |> filter(name == 'La Réunion')
reunion_gg <- plot_logement(reunion) +
    theme_void() +
    theme(legend.position = "none")

# Martinique
martinique <- df |> filter(name == 'Martinique')
martinique_gg <- plot_logement(martinique) +
    theme_void() +
    theme(legend.position = "none")

# Guadeloupe
guadeloupe <- df |> filter(name == 'Guadeloupe')
guadeloupe_gg <- plot_logement(guadeloupe) +
    theme_void() +
    theme(legend.position = "none")

# Guyane
guyane <- df |> filter(name == 'Guyane française')
guyane_gg <- plot_logement(guyane) +
    theme_void() +
    theme(legend.position = "none")


# Arrange plots
fr_metro_gg + 
    annotation_custom(
        ggplotGrob(paris_gg), xmin = -4.75, xmax = -2.75, ymin = 49.75, ymax = 51.75
    ) +
    annotation_custom(
        ggplotGrob(guyane_gg), xmin = -4.75, xmax = -2.75, ymin = 45, ymax = 47
    ) +
    annotation_custom(
        ggplotGrob(reunion_gg), xmin = -4.75, xmax = -2.75, ymin = 43.5, ymax = 45.5
    ) +
    annotation_custom(
        ggplotGrob(martinique_gg), xmin = -4.75, xmax = -2.75, ymin = 42, ymax = 44
    ) +
    annotation_custom(
        ggplotGrob(guadeloupe_gg), xmin = -2.75, xmax = -0.75, ymin = 40.75, ymax = 42.75
    ) +
    theme(plot.margin = margin(0.1, 0.1, 0.1, 0.1, "cm"))

ggsave(
    "./figures/logement.png",
    width = 1080, height = 1080, units = "px",
    bg = "white"
)


seas <- ggarrange(
    plotlist = list(
        reunion_gg, martinique_gg, guadeloupe_gg, guyane_gg
    ),
    ncol = 1, nrow = 4
)

pp <- ggarrange(
    plotlist = list(
        fr_metro_gg + annotation_custom(
            ggplotGrob(paris_gg), xmin = -4.75, xmax = -2.75, ymin = 49.75, ymax = 51.75
        ),
        seas
    ),
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
