library(dplyr)
library(lme4)
library(performance)

# ============================================================
# COMPARE FLOWERING PHENOLOGY OF SHARED SPECIES ACROSS GARDENS
# ============================================================


# ------------------------------------------------------------
# 1. Load phenology data
# ------------------------------------------------------------

jena_phen <- readRDS(
  "Data/Phenology_data/clean_plant_phenobs_jena.rds"
)

halle_phen <- readRDS(
  "Data/Phenology_data/clean_plant_phenobs_halle.rds"
)

leipzig_phen <- readRDS(
  "Data/Phenology_data/clean_plant_phenobs_leipzig.rds"
)


# ------------------------------------------------------------
# 2. Combine garden datasets
# ------------------------------------------------------------

plant_data <- bind_rows(
  jena_phen %>%
    mutate(Garden = "Jena"),
  
  halle_phen %>%
    mutate(Garden = "Halle"),
  
  leipzig_phen %>%
    mutate(Garden = "Leipzig")
) %>%
  mutate(
    Garden = factor(
      Garden,
      levels = c("Halle", "Jena", "Leipzig")
    )
  )


# ------------------------------------------------------------
# 3. Identify species occurring in all three gardens
# ------------------------------------------------------------

common_plants <- plant_data %>%
  distinct(Garden, Species) %>%
  count(Species, name = "n_gardens") %>%
  filter(n_gardens == nlevels(plant_data$Garden)) %>%
  pull(Species)


# ------------------------------------------------------------
# 4. Retain only species shared among gardens
# ------------------------------------------------------------

plant_data_filtered <- plant_data %>%
  filter(Species %in% common_plants)


# Optional checks
cat(
  "Number of shared plant species:",
  length(common_plants),
  "\n"
)

plant_data_filtered %>%
  distinct(Species, Garden) %>%
  count(Garden) %>%
  print()


# ------------------------------------------------------------
# 5. Calculate flowering centroid for each species and garden
#
# Flowering centroid = intensity-weighted mean day of year
# ------------------------------------------------------------

centroids <- plant_data_filtered %>%
  filter(
    !is.na(Flowering_intensity),
    Flowering_intensity > 0,
    !is.na(Doy)
  ) %>%
  group_by(
    Species,
    Garden
  ) %>%
  summarise(
    centroid = weighted.mean(
      x = Doy,
      w = Flowering_intensity,
      na.rm = TRUE
    ),
    n_observations = n(),
    .groups = "drop"
  )


# ------------------------------------------------------------
# 6. Check completeness of the repeated-measures dataset
#
# Keep only species with a centroid in all three gardens.
# This is useful because a species may occur in all gardens but
# lack positive flowering-intensity observations in one garden.
# ------------------------------------------------------------

species_with_complete_centroids <- centroids %>%
  distinct(Species, Garden) %>%
  count(Species, name = "n_gardens") %>%
  filter(n_gardens == nlevels(plant_data$Garden)) %>%
  pull(Species)

centroids_complete <- centroids %>%
  filter(Species %in% species_with_complete_centroids)


cat(
  "Number of species with flowering centroids in all gardens:",
  n_distinct(centroids_complete$Species),
  "\n"
)

cat(
  "Number of centroid observations:",
  nrow(centroids_complete),
  "\n"
)


# ------------------------------------------------------------
# 7. Fit linear mixed-effects model
#
# Fixed effect: garden
# Random intercept: species identity
# ------------------------------------------------------------

model <- lmer(
  centroid ~ Garden + (1 | Species),
  data = centroids_complete,
  REML = TRUE
)


# ------------------------------------------------------------
# 8. Inspect results
# ------------------------------------------------------------

summary(model)

anova(model)

performance::icc(model)