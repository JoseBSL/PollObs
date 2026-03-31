#Load libraries
library(dplyr)
library(ggplot2)
library(patchwork)
library(viridisLite)

#Read botanical garden data
jena_phen = readRDS("Data/Phenology_data/clean_plant_phenobs_jena.rds")
halle_phen = readRDS("Data/Phenology_data/clean_plant_phenobs_halle.rds")
leipzig_phen = readRDS("Data/Phenology_data/clean_plant_phenobs_leipzig.rds")

make_phen_plot <- function(dat, title_text) {
  
  v = unique(dat$Species)
  
  d1 = dat %>% 
    filter(Species %in% v) %>% 
    mutate(Flowering_intensity = if_else(Flowers_opening == "no", 0, Flowering_intensity))
  
  # Find first flowering day to order species
  lev_species = d1 %>% 
    group_by(Species) %>% 
    filter(Flowers_opening == "y") %>% 
    slice_min(Doy) %>% 
    arrange(Doy) %>% 
    pull(Species)
  
  d1$Species = factor(d1$Species, levels = lev_species)
  
  # Plasma palette
  cols = plasma(nlevels(d1$Species), begin = 0.05, end = 0.95)
  
  label_data = d1 %>% 
    group_by(Species) %>% 
    filter(Flowers_opening == "y") %>% 
    slice_min(Doy) %>% 
    arrange(Doy) %>% 
    mutate(Flowering_intensity = 0) %>% 
    select(Species, Doy, Flowering_intensity) %>% 
    mutate(Doy = Doy - 30)
  
  ggplot(d1, aes(x = Doy, y = Flowering_intensity, fill = Species)) + 
    geom_text(
      data = label_data,
      aes(label = Species),
      fontface = "italic",
      vjust = -0.3,
      hjust = -0.1,
      size = 2,
      color = "black"
    ) +
    stat_smooth(
      method = "gam",
      method.args = list(family = poisson),
      geom = "area",
      alpha = 0.75,
      span = 0.1
    ) +
    theme_minimal() +
    theme(
      legend.position = "none",
      strip.background = element_blank(),
      strip.text.x = element_blank(),
      panel.spacing = unit(-1.5, "lines")
    ) +
    coord_cartesian(expand = FALSE) +
    scale_y_continuous(expand = c(0, 0), breaks = NULL, labels = NULL) +
    facet_wrap(~Species, ncol = 1) +
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