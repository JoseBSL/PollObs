# ======================================================
# Trait matching networks for each garden × week
# (built using the SAME TEMPLATE as abundance networks)
# ======================================================

# Load libraries
library(dplyr)
library(purrr)
library(tidyr)
library(ggplot2)
library(vegan)
library(readr)
library(stringr)
library(tibble)

# ======================================================
# SETTINGS (edit if needed)
# ======================================================
poll_length_col  <- "IT_mm"        # pollinator trait column in PollTraits.rds
plant_length_col <- "Flower_width" # plant trait column in morphometrics csv
sigma <- 3                         # trait tolerance (mm)

gaussian_scaled <- function(t_poll, t_plant, sigma) {
  exp(- (t_poll - t_plant)^2 / (2 * sigma^2))
}

row_normalize <- function(M) {
  rs <- rowSums(M, na.rm = TRUE)
  M2 <- sweep(M, 1, rs, "/")
  M2[!is.finite(M2)] <- 0
  M2
}

# ======================================================
# Load data
# Interaction data
raw_data <- readRDS("Data/Working_files/interaction_data.rds")

# Morphometrics to get phenobs species vector
morphometrics <- read_csv("Data/Trait_data/Raw/ReproductiveTraits_Morphometrics.csv")

phenobs_spp <- morphometrics %>%
  select(Species) %>%
  mutate(Species = str_replace(Species, "Persicaria bistorta", "Polygonum bistorta")) %>%
  mutate(Species = str_replace(Species, "Aquilegia chrysantha", "Aquilegia vulgaris")) %>%
  distinct() %>%
  pull(Species)

# Load plant-poll networks by garden × week
net_by_garden <- readRDS("Data/Working_files/networks_by_garden_week_only_phenobs.rds")

# ======================================================
# Create vector with main pollinator orders (kept consistent)
poll_order <- c("Hymenoptera", "Diptera", "Coleoptera", "Lepidoptera")

# (Optional / kept from template) Create tibble with unique dates + week number
dates <- raw_data %>%
  select(Botanical_garden, Date) %>%
  group_by(Botanical_garden) %>%
  distinct()

weeks_2023 <- tibble(Doy = 1:365) %>%
  mutate(Date = as.Date(Doy - 0, origin = "2023-01-01"),
         Sampling_week = isoweek(Date))

dates <- left_join(dates, weeks_2023)

# Prepare interaction data (kept for consistency with template)
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
  filter(!Plants == "Iberis sempervirens")

interaction_data <- interaction_data %>%
  filter(Plant %in% phenobs_spp)

# Add week number (kept for consistency; not required for trait-only matrices)
interaction_data <- left_join(interaction_data, dates)

# ======================================================
# LOAD & SUMMARIZE TRAITS (global)
# ======================================================

# Pollinator traits
poll_traits <- readRDS("Data/Trait_data/Processed/PollTraits.rds")

poll_len <- poll_traits %>%
  select(Pollinator, !!sym(poll_length_col)) %>%
  rename(trait = !!sym(poll_length_col)) %>%
  group_by(Pollinator) %>%
  summarise(t_poll = mean(trait, na.rm = TRUE), .groups = "drop") %>%
  filter(!is.na(t_poll))

# Plant traits
plant_len <- morphometrics %>%
  select(Species, !!sym(plant_length_col)) %>%
  rename(trait = !!sym(plant_length_col)) %>%
  group_by(Species) %>%
  summarise(t_plant = mean(trait, na.rm = TRUE), .groups = "drop") %>%
  filter(!is.na(t_plant))

# ======================================================
# GLOBAL TRAIT MATCHING MATRIX (rows = Plants, cols = Pollinators)
# ======================================================
trait_pairs <- tidyr::crossing(
  Plants      = plant_len$Species,
  Pollinators = poll_len$Pollinator
) %>%
  left_join(plant_len, by = c("Plants" = "Species")) %>%
  left_join(poll_len,  by = c("Pollinators" = "Pollinator")) %>%
  mutate(T_gauss = gaussian_scaled(t_poll, t_plant, sigma))

T_gauss_mat <- trait_pairs %>%
  select(Plants, Pollinators, T_gauss) %>%
  pivot_wider(names_from = Pollinators, values_from = T_gauss) %>%
  column_to_rownames("Plants") %>%
  as.matrix()

# ======================================================
# BUILD TRAIT MATRIX ALIGNED TO EACH garden × week NETWORK
# ======================================================
build_trait_matrix <- function(garden_name, week) {
  
  network <- net_by_garden %>%
    filter(Botanical_garden == garden_name, Sampling_week == week) %>%
    pull(Interaction_network) %>%
    .[[1]]
  
  plants_g <- rownames(network)
  polls_g  <- colnames(network)
  
  # aligned output filled with 0s
  out <- matrix(
    0,
    nrow = length(plants_g),
    ncol = length(polls_g),
    dimnames = list(plants_g, polls_g)
  )
  
  # overlap indices (only spp with traits)
  r_ok <- intersect(plants_g, rownames(T_gauss_mat))
  c_ok <- intersect(polls_g,  colnames(T_gauss_mat))
  
  if (length(r_ok) > 0 && length(c_ok) > 0) {
    out[r_ok, c_ok] <- T_gauss_mat[r_ok, c_ok, drop = FALSE]
  }
  
  out[is.na(out)] <- 0
  out
}

build_trait_matrix_rowNorm <- function(garden_name, week) {
  M <- build_trait_matrix(garden_name, week)
  row_normalize(M)
}

# ======================================================
# Run it for each garden-week combination (template-style pmap)
trait_prob_matrices_by_garden_week <- net_by_garden %>%
  mutate(
    Trait_prob = pmap(
      list(Botanical_garden, Sampling_week),
      build_trait_matrix
    ),
    Trait_prob_rowNorm = pmap(
      list(Botanical_garden, Sampling_week),
      build_trait_matrix_rowNorm
    )
  )

# ======================================================
# Save
saveRDS(
  trait_prob_matrices_by_garden_week,
  "Data/Working_files/trait_probability_networks_only_phenobs_by_week.rds"
)

# Object contains (plus whatever else exists in net_by_garden):
# - Botanical_garden
# - Sampling_week
# - Interaction_network (existing weekly)
# - Trait_prob (aligned weekly matrix)
# - Trait_prob_rowNorm (aligned + row-normalized weekly matrix)
