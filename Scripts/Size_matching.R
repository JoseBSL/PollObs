# =========================================================
# Trait-matching probability matrix (Flower width ↔ IT_mm)
# Gaussian scaled function
# =========================================================

library(readr)
library(dplyr)
library(tidyr)
library(tibble)

# -----------------------------
# 1) Load and summarize traits
# -----------------------------

# Pollinator traits (IT_mm = intertegular distance)
poll_traits <- readRDS("Data/Trait_data/Processed/PollTraits.rds")

poll_size <- poll_traits %>%
  select(Pollinator, IT_mm) %>%
  group_by(Pollinator) %>%
  summarise(
    t_poll = mean(IT_mm, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(!is.na(t_poll))

# Plant traits (flower width)
plant_traits <- read_csv("Data/Trait_data/Raw/ReproductiveTraits_Morphometrics.csv")

plant_size <- plant_traits %>%
  select(Species, Flower_width) %>%
  group_by(Species) %>%
  summarise(
    t_plant = mean(Flower_width, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(!is.na(t_plant))

# -----------------------------
# 2) All pollinator × plant pairs
# -----------------------------

trait_pairs <- tidyr::crossing(
  Pollinator = poll_size$Pollinator,
  Plant      = plant_size$Species
) %>%
  left_join(poll_size,  by = "Pollinator") %>%
  left_join(plant_size, by = c("Plant" = "Species"))

# -----------------------------
# 3) Gaussian scaled matching
#    f = exp(-(Δ²)/(2σ²))
# -----------------------------

gaussian_scaled <- function(t_poll, t_plant, sigma) {
  exp(- (t_poll - t_plant)^2 / (2 * sigma^2))
}

# Trait tolerance (in mm)
# Try 1–3 mm; adjust via sensitivity analysis
sigma <- 2

trait_pairs <- trait_pairs %>%
  mutate(
    T_gauss = gaussian_scaled(t_poll, t_plant, sigma)
  )

# -----------------------------
# 4) Build probability matrix
#    rows = pollinators
#    cols = plants
# -----------------------------

T_gauss <- trait_pairs %>%
  select(Pollinator, Plant, T_gauss) %>%
  pivot_wider(
    names_from  = Plant,
    values_from = T_gauss
  ) %>%
  column_to_rownames("Pollinator") %>%
  as.matrix()

# -----------------------------
# 5) Optional: row-normalize
#    (each pollinator sums to 1)
# -----------------------------

row_normalize <- function(M) {
  rs <- rowSums(M, na.rm = TRUE)
  M2 <- sweep(M, 1, rs, "/")
  M2[!is.finite(M2)] <- 0
  M2
}

T_gauss_rowNorm <- row_normalize(T_gauss)

# -----------------------------
# Outputs
# -----------------------------
# T_gauss        : raw trait-matching weights (0–1, max at perfect match)
# T_gauss_rowNorm: probability matrix (rows sum to 1)

T_gauss
