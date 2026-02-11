# =========================================================
# Trait probability matrices per garden x season
# - Same row/col alignment as Interaction_network
# - Filtered to garden-season species sets
# - Uses map2() (cleaner than pmap/list(..1,..2))
# =========================================================

library(readr)
library(dplyr)
library(tidyr)
library(tibble)
library(stringr)
library(purrr)

# -----------------------------
# 0) SETTINGS (edit if needed)
# -----------------------------
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

# -----------------------------
# 1) LOAD DATA
# -----------------------------
raw_data <- readRDS("Data/Working_files/interaction_data.rds")
morphometrics <- read_csv("Data/Trait_data/Raw/ReproductiveTraits_Morphometrics.csv")

# NOTE: seasonal networks object here (must contain Botanical_garden + Season + Interaction_network)
net_by_garden <- readRDS("Data/Working_files/networks_by_garden_season_only_phenobs.rds")

# phenobs species vector (same as your workflow)
phenobs_spp <- morphometrics %>%
  select(Species) %>%
  mutate(Species = str_replace(Species, "Persicaria bistorta", "Polygonum bistorta")) %>%
  mutate(Species = str_replace(Species, "Aquilegia chrysantha", "Aquilegia vulgaris")) %>%
  distinct() %>%
  pull(Species)

# -----------------------------
# 2) OPTIONAL: FILTER raw_data (kept for consistency with your pipeline)
#     (Not strictly required for matrices if net_by_garden already filtered)
# -----------------------------
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

# -----------------------------
# 3) LOAD & SUMMARIZE TRAITS
# -----------------------------
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

# -----------------------------
# 4) GLOBAL TRAIT MATCHING MATRIX
#     rows = Plants, cols = Pollinators
# -----------------------------
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

# -----------------------------
# 5) BUILD GARDEN-SPECIFIC MATRICES
#     aligned to each garden x season interaction network
# -----------------------------
build_trait_matrix <- function(garden_name, season_name, trait_mat, normalize = FALSE) {
  
  network <- net_by_garden %>%
    filter(Botanical_garden == garden_name, Season == season_name) %>%
    pull(Interaction_network) %>%
    .[[1]]
  
  plants_g <- rownames(network)
  polls_g  <- colnames(network)
  
  # create aligned matrix filled with 0s
  out <- matrix(
    0,
    nrow = length(plants_g),
    ncol = length(polls_g),
    dimnames = list(plants_g, polls_g)
  )
  
  # overlap indices (only species with traits)
  r_ok <- intersect(plants_g, rownames(trait_mat))
  c_ok <- intersect(polls_g,  colnames(trait_mat))
  
  if (length(r_ok) > 0 && length(c_ok) > 0) {
    out[r_ok, c_ok] <- trait_mat[r_ok, c_ok, drop = FALSE]
  }
  
  # replace any remaining NA just in case
  out[is.na(out)] <- 0
  
  if (normalize) out <- row_normalize(out)
  out
}

# Run it for each garden-season combination (map2 is cleaner than pmap for 2 inputs)
trait_prob_matrices_by_garden_season <- net_by_garden %>%
  mutate(
    Trait_prob = map2(
      Botanical_garden,
      Season,
      build_trait_matrix,
      trait_mat = T_gauss_mat,
      normalize = FALSE
    ),
    Trait_prob_rowNorm = map2(
      Botanical_garden,
      Season,
      build_trait_matrix,
      trait_mat = T_gauss_mat,
      normalize = TRUE
    )
  )

# -----------------------------
# 6) SAVE
# -----------------------------
saveRDS(
  trait_prob_matrices_by_garden_season,
  "Data/Working_files/trait_probability_networks_only_phenobs_by_season.rds"
)

# Object contains:
# - Botanical_garden
# - Season (Early/Mid/Late)
# - Interaction_network (existing seasonal)
# - Trait_prob (aligned seasonal matrix)
# - Trait_prob_rowNorm (aligned + row-normalized seasonal matrix)
