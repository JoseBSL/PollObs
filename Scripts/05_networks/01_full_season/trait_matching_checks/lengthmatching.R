# =========================================================
# Trait-matching probability matrix (Tube length ↔ Tongue length)
# Gaussian scaled function
# =========================================================

library(readr)
library(dplyr)
library(tidyr)
library(tibble)

# -----------------------------
# 0) Choose the columns to use
# -----------------------------
# EDIT THESE to match your data columns exactly
poll_length_col  <- "Tongue_mm"      # pollinator length trait
plant_length_col <- "Floral_tube_length"    # plant length trait (tube depth/length)

# Trait tolerance (in mm)
# try 1–5 mm depending on your measurement scale
sigma <- 3

# -----------------------------
# 1) Load and summarize traits
# -----------------------------

# Pollinator traits
poll_traits <- readRDS("Data/Trait_data/Processed/PollTraits.rds")

poll_len <- poll_traits %>%
  select(Pollinator, !!sym(poll_length_col)) %>%
  rename(trait = !!sym(poll_length_col)) %>%
  group_by(Pollinator) %>%
  summarise(
    t_poll = mean(trait, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(!is.na(t_poll))

# Plant traits
plant_traits <- read_csv("Data/Trait_data/Raw/ReproductiveTraits_Morphometrics.csv")

plant_len <- plant_traits %>%
  select(Species, !!sym(plant_length_col)) %>%
  rename(trait = !!sym(plant_length_col)) %>%
  group_by(Species) %>%
  summarise(
    t_plant = mean(trait, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(!is.na(t_plant))

# -----------------------------
# 2) All pollinator × plant pairs
# -----------------------------

trait_pairs <- tidyr::crossing(
  Pollinator = poll_len$Pollinator,
  Plant      = plant_len$Species
) %>%
  left_join(poll_len,  by = "Pollinator") %>%
  left_join(plant_len, by = c("Plant" = "Species"))

# -----------------------------
# 3) Gaussian scaled matching
#    f = exp(-(Δ²)/(2σ²))
# -----------------------------

gaussian_scaled <- function(t_poll, t_plant, sigma) {
  exp(- (t_poll - t_plant)^2 / (2 * sigma^2))
}

trait_pairs <- trait_pairs %>%
  mutate(T_gauss = gaussian_scaled(t_poll, t_plant, sigma))

# -----------------------------
# 4) Build probability matrix
#    rows = pollinators
#    cols = plants
# -----------------------------

T_gauss <- trait_pairs %>%
  select(Pollinator, Plant, T_gauss) %>%
  pivot_wider(names_from = Plant, values_from = T_gauss) %>%
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
T_gauss
