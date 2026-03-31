#Load libraries
library(dplyr)
library(ggplot2)
library(patchwork)
library(viridisLite)

#Read botanical garden data
jena_phen = readRDS("Data/Phenology_data/clean_plant_phenobs_jena.rds")
halle_phen = readRDS("Data/Phenology_data/clean_plant_phenobs_halle.rds")
leipzig_phen = readRDS("Data/Phenology_data/clean_plant_phenobs_leipzig.rds")

halle_phen %>% 
  filter(Species == "Origanum vulgare") %>% 
  arrange(Doy)

halle_phen %>% 
  filter(Species == "Origanum vulgare", Flowers_opening == "y")

make_phen_plot <- function(dat, title_text) {
  
  d1 <- dat %>%
    mutate(Flowering_intensity = if_else(Flowers_opening == "no", 0, Flowering_intensity)) %>%
    group_by(Species, Doy) %>%
    summarise(Flowering_intensity = mean(Flowering_intensity, na.rm = TRUE), .groups = "drop") %>%
    group_by(Species) %>%
    filter(any(Flowering_intensity > 0, na.rm = TRUE)) %>%
    ungroup()
  
  lev_species <- d1 %>%
    filter(Flowering_intensity > 0, !is.na(Doy)) %>%
    group_by(Species) %>%
    summarise(first_doy = min(Doy), .groups = "drop") %>%
    arrange(first_doy) %>%
    pull(Species)
  
  d1 <- d1 %>%
    mutate(Species = factor(Species, levels = lev_species))
  
  cols <- plasma(nlevels(d1$Species), begin = 0.05, end = 0.95)
  
  ggplot(d1, aes(x = Doy, y = Flowering_intensity, fill = Species, group = Species)) +
    geom_area(alpha = 0.75) +
    theme_minimal() +
    theme(
      legend.position = "none",
      strip.background = element_blank(),
      strip.text.y.left = element_text(
        angle = 0,
        face = "italic",
        size = 7.5,
        hjust = 1,
        color = "black",
        margin = margin(r = 6)
      ),
      strip.placement = "outside",
      panel.spacing = unit(0.15, "lines"),
      panel.grid.major.x = element_line(color = "grey85", linewidth = 0.3),
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank()
    ) +
    coord_cartesian(expand = FALSE) +
    scale_y_continuous(expand = c(0, 0), breaks = NULL, labels = NULL) +
    facet_wrap(~Species, ncol = 1, strip.position = "left") +
    scale_fill_manual(values = cols) +
    labs(
      x = "Day of the year",
      y = "Flowering intensity",
      title = title_text
    )
}

jena_plot    = make_phen_plot(jena_phen, "Jena Botanical Garden")
halle_plot   = make_phen_plot(halle_phen, "Halle Botanical Garden")
leipzig_plot = make_phen_plot(leipzig_phen, "Leipzig Botanical Garden")

jena_plot + halle_plot + leipzig_plot