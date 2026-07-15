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


common_plants <- plant_data %>%
  distinct(Garden, Species) %>%
  count(Species) %>%
  filter(n == 3) %>%
  pull(Species)

plant_data_filtered <- plant_data %>%
  filter(Species %in% common_plants)

centroids <- plant_data %>%
  filter(!is.na(Flowering_intensity),
         Flowering_intensity > 0) %>%
  group_by(Species, Garden) %>%
  summarise(
    centroid =
      weighted.mean(
        Doy,
        Flowering_intensity
      ),
    .groups = "drop"
  )



library(lme4)

model <- lmer(
  centroid ~ Garden + (1 | Species),
  data = centroids
)

summary(model)
anova(model)
library(performance)
icc(model)
