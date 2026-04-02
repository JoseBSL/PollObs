library(dplyr)
library(purrr)
library(ggplot2)
library(patchwork)

# Read data
jena_phen    <- readRDS("Data/Phenology_data/clean_plant_phenobs_jena.rds")
halle_phen   <- readRDS("Data/Phenology_data/clean_plant_phenobs_halle.rds")
leipzig_phen <- readRDS("Data/Phenology_data/clean_plant_phenobs_leipzig.rds")

# Combinar jardines
plant_data <- bind_rows(
  jena_phen %>% mutate(Garden = "Jena"),
  halle_phen %>% mutate(Garden = "Halle"),
  leipzig_phen %>% mutate(Garden = "Leipzig")
)

# Calcular centro fenológico por especie
# (media ponderada de DOY usando la intensidad de floración)
plant_centers <- plant_data %>%
  mutate(
    Flowering_intensity = if_else(
      Flowers_opening == "no",
      0,
      as.numeric(Flowering_intensity)
    )
  ) %>%
  filter(!is.na(Doy), Flowering_intensity > 0) %>%
  group_by(Garden, Species) %>%
  summarise(
    center = weighted.mean(Doy, Flowering_intensity, na.rm = TRUE),
    n_obs = n(),
    .groups = "drop"
  ) %>%
  filter(n_obs >= 3)

plant_centers

plant_sync_simple <- plant_centers %>%
  group_by(Garden) %>%
  summarise(
    q05 = quantile(center, 0.05, na.rm = TRUE),
    q50 = quantile(center, 0.50, na.rm = TRUE),
    q95 = quantile(center, 0.95, na.rm = TRUE),
    duration = q95 - q05,
    sd_center = sd(center, na.rm = TRUE),
    sync = 1 - sd_center / duration,
    n_species = n(),
    .groups = "drop"
  )

plant_sync_simple


saveRDS(plant_centers, "Data/Working_files/plant_pheno_centers.rds")
