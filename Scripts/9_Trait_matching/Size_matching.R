# ======================================================
# Trait-based probability matrices for each garden
# (Flower width ↔ IT_mm) using Gaussian scaled matching
# Built to match your MAIN abundance script structure
# ======================================================

library(dplyr)
library(purrr)
library(tidyr)
library(readr)
library(stringr)

# ======================================================
# Load data (same as your main script)
# ======================================================

raw_data <- readRDS("Data/Working_files/interaction_data.rds")

morphometrics <- read_csv("Data/Trait_data/Raw/ReproductiveTraits_Morphometrics.csv")

phenobs_spp <- morphometrics %>%
  select(Species) %>%
  mutate(Species = str_replace(Species, "Persicaria bistorta", "Polygonum bistorta")) %>%
  mutate(Species = str_replace(Species, "Aquilegia chrysantha", "Aquilegia vulgaris")) %>%
  distinct() %>%
  pull(Species)

net_by_garden <- readRDS("Data/Working_files/networks_by_garden_only_phenobs.rds")

# ======================================================
# Prepare interaction data (same filtering you use)
# ======================================================

poll_order <- c("Hymenoptera", "Diptera", "Coleoptera", "Lepidoptera")

interaction_data <- raw_data %>%
  filter(!is.na(Interactions),
         !is.na(Floral_abundance),
         Pollinator != "None") %>%
  rename(Plants = Plant_accepted_name,
         Pollinators = Pollinator_accepted_name) %>%
  filter(!is.na(Pollinators)) %>%
  filter(Pollinator_rank == "SPECIES") %>%
  filter(Sampling == "Focal") %>%
  filter(Pollinator_order %in% poll_order) %>%
  filter(!Plants == "Iberis sempervirens") %>%
  filter(Plants %in% phenobs_spp)

# ======================================================
# Trait summaries (global tables, reused for each garden)
# ======================================================

poll_traits <- readRDS("Data/Trait_data/Processed/PollTraits.rds")

poll_size <- poll_traits %>%
  select(Pollinator, IT_mm) %>%
  group_by(Pollinator) %>%
  summarise(
    t_poll = mean(IT_mm, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(!is.na(t_poll)) %>%
  rename(Pollinators = Pollinator)   # <-- match your network column names

plant_size <- morphometrics %>%
  select(Species, Flower_width) %>%
  group_by(Species) %>%
  summarise(
    t_plant = mean(Flower_width, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(!is.na(t_plant)) %>%
  rename(Plants = Species)           # <-- match your network row names

# ======================================================
# Gaussian matching function (scaled 0–1)
# ======================================================

gaussian_scaled <- function(t_poll, t_plant, sigma) {
  exp(- (t_poll - t_plant)^2 / (2 * sigma^2))
}

sigma <- 2  # trait tolerance (mm) — tune in sensitivity analysis

# ======================================================
# Build TRAIT probability matrix for a given garden
# - Uses the same row/col order as Interaction_network
# - Fills missing traits with 0 (conservative)
# ======================================================

build_trait_prob_matrix <- function(garden_name, sigma = 2) {
  
  network <- net_by_garden %>%
    filter(Botanical_garden == garden_name) %>%
    pull(Interaction_network) %>%
    .[[1]]
  
  poll_order_tbl  <- tibble(Pollinators = colnames(network))
  plant_order_tbl <- tibble(Plants     = rownames(network))
  
  # bring in trait values in the SAME order as the network
  poll_traits_ord <- poll_order_tbl %>%
    left_join(poll_size, by = "Pollinators")
  
  plant_traits_ord <- plant_order_tbl %>%
    left_join(plant_size, by = "Plants")
  
  # compute pairwise Gaussian matching for all ordered pairs
  trait_pairs <- tidyr::crossing(
    Plants = plant_traits_ord$Plants,
    Pollinators = poll_traits_ord$Pollinators
  ) %>%
    left_join(plant_traits_ord, by = "Plants") %>%
    left_join(poll_traits_ord,  by = "Pollinators") %>%
    mutate(
      T_gauss = ifelse(
        is.na(t_plant) | is.na(t_poll),
        0,
        gaussian_scaled(t_poll, t_plant, sigma)
      )
    )
  
  # to matrix (rows = plants, cols = pollinators)
  trait_mat <- trait_pairs %>%
    select(Plants, Pollinators, T_gauss) %>%
    pivot_wider(names_from = Pollinators, values_from = T_gauss) %>%
    tibble::column_to_rownames("Plants") %>%
    as.matrix()
  
  # guarantee same dimnames/order as the observed network
  trait_mat <- trait_mat[rownames(network), colnames(network), drop = FALSE]
  
  return(trait_mat)
}

# ======================================================
# Run for each garden and store like your abundance object
# ======================================================

trait_prob_matrices_by_garden <- net_by_garden %>%
  mutate(Prob_matrix_traits = map(Botanical_garden, ~ build_trait_prob_matrix(.x, sigma = sigma)))

# Save
saveRDS(trait_prob_matrices_by_garden,
        "Data/Working_files/trait_networks_only_phenobs.rds")

# Quick check
trait_prob_matrices_by_garden$Prob_matrix_traits[[1]][1:5, 1:5]
