# ======================================================
# Prepare plant-family × pollinator-guild networks by garden × week
# Bee families kept separate
# Phenology-aware nulls: randomize within garden-week blocks
# Outputs: M_obs_guild, M_exp_guild, M_sd_guild, SES_guild, pval_df_guild
# ======================================================

library(dplyr)
library(tidyr)
library(purrr)
library(tibble)
library(bipartite)
library(lubridate)

# -----------------------------
# Load data
# -----------------------------

raw_data <- readRDS("Data/Working_files/interaction_data.rds")

# -----------------------------
# Add ISO week
# -----------------------------

dates <- raw_data %>%
  distinct(Botanical_garden, Date) %>%
  mutate(Sampling_week = isoweek(Date))

# -----------------------------
# Filter interactions and define pollinator guilds
# -----------------------------

poll_order <- c("Hymenoptera", "Diptera", "Coleoptera", "Lepidoptera")

bee_families <- c(
  "Apidae",
  "Andrenidae",
  "Halictidae",
  "Megachilidae",
  "Colletidae",
  "Melittidae"
)

interaction_data <- raw_data %>%
  filter(
    !is.na(Interactions),
    !is.na(Floral_abundance),
    Pollinator != "None"
  ) %>%
  rename(
    Plants = Plant_accepted_name,
    Pollinators = Pollinator_accepted_name
  ) %>%
  filter(
    !is.na(Pollinators),
    Pollinator_rank == "SPECIES",
    Pollinator_order %in% poll_order,
    Plants != "Iberis sempervirens"
  ) %>%
  left_join(dates, by = c("Botanical_garden", "Date")) %>%
  mutate(
    Pollinator_guild = case_when(
      Pollinator_order == "Hymenoptera" &
        Pollinator_family %in% bee_families ~
        Pollinator_family,
      
      Pollinator_order == "Hymenoptera" ~
        "Non-bee Hymenoptera",
      
      Pollinator_family == "Bombyliidae" ~
        "Bombylids",
      
      Pollinator_family == "Syrphidae" ~
        "Syrphids",
      
      Pollinator_order == "Diptera" ~
        "Other Diptera",
      
      Pollinator_order == "Lepidoptera" ~
        "Lepidoptera",
      
      Pollinator_order == "Coleoptera" ~
        "Coleoptera",
      
      TRUE ~
        "Other taxa"
    )
  )

# ======================================================
# Keep plant families and pollinator guilds with enough interactions
# ======================================================

min_interactions <- 75

families_keep_plants <- interaction_data %>%
  group_by(Plant_family) %>%
  summarise(total_int = sum(Interactions), .groups = "drop") %>%
  filter(total_int >= min_interactions) %>%
  pull(Plant_family)

guilds_keep_polls <- interaction_data %>%
  group_by(Pollinator_guild) %>%
  summarise(total_int = sum(Interactions), .groups = "drop") %>%
  filter(total_int >= min_interactions) %>%
  pull(Pollinator_guild)

interaction_data <- interaction_data %>%
  filter(
    Plant_family %in% families_keep_plants,
    Pollinator_guild %in% guilds_keep_polls
  )

# -----------------------------
# Helper: garden-week -> plant family × pollinator guild matrix
# -----------------------------

to_network_guild <- function(df) {
  df %>%
    count(
      Plant_family,
      Pollinator_guild,
      wt = Interactions,
      name = "Total_interactions"
    ) %>%
    pivot_wider(
      names_from = Pollinator_guild,
      values_from = Total_interactions,
      values_fill = 0
    ) %>%
    column_to_rownames("Plant_family") %>%
    as.matrix()
}

# -----------------------------
# Build networks per garden × week
# -----------------------------

networks_by_garden_week <- interaction_data %>%
  group_by(Botanical_garden, Sampling_week) %>%
  nest() %>%
  mutate(Interaction_network_guild = map(data, to_network_guild)) %>%
  select(-data) %>%
  ungroup()

# -----------------------------
# Filter out small / unstable networks
# -----------------------------

networks_by_garden_week <- networks_by_garden_week %>%
  filter(map_int(Interaction_network_guild, sum) >= 20) %>%
  filter(
    map_int(Interaction_network_guild, nrow) >= 2,
    map_int(Interaction_network_guild, ncol) >= 2
  )

# -----------------------------
# Extract matrices and pad to common dimension
# -----------------------------

block_mats_guild <- networks_by_garden_week$Interaction_network_guild

all_rows <- sort(unique(unlist(map(block_mats_guild, rownames))))
all_cols <- sort(unique(unlist(map(block_mats_guild, colnames))))

pad_matrix <- function(M, all_rows, all_cols) {
  out <- matrix(
    0,
    nrow = length(all_rows),
    ncol = length(all_cols),
    dimnames = list(all_rows, all_cols)
  )
  
  out[rownames(M), colnames(M)] <- M
  out
}

block_mats_guild <- map(
  block_mats_guild,
  pad_matrix,
  all_rows = all_rows,
  all_cols = all_cols
)

# -----------------------------
# Observed aggregated matrix
# -----------------------------

M_obs_guild <- Reduce(`+`, block_mats_guild)

# -----------------------------
# Null simulations
# -----------------------------

set.seed(1)
n.sim <- 1000

simulate_one <- function() {
  rand_blocks <- lapply(block_mats_guild, function(M) {
    R <- nullmodel(M, N = 1, method = "r2dtable")[[1]]
    dimnames(R) <- dimnames(M)
    R
  })
  
  Reduce(`+`, rand_blocks)
}

null_list_guild <- replicate(n.sim, simulate_one(), simplify = FALSE)

# -----------------------------
# Expected matrix
# -----------------------------

M_exp_guild <- Reduce(`+`, null_list_guild) / n.sim

# -----------------------------
# SD matrix
# -----------------------------

ssq <- matrix(
  0,
  nrow = nrow(M_exp_guild),
  ncol = ncol(M_exp_guild),
  dimnames = dimnames(M_exp_guild)
)

for (k in seq_along(null_list_guild)) {
  ssq <- ssq + (null_list_guild[[k]] - M_exp_guild)^2
}

M_sd_guild <- sqrt(ssq / n.sim)
M_sd_guild[M_sd_guild == 0] <- NA

# -----------------------------
# Standardized effect size
# -----------------------------

SES_guild <- (M_obs_guild - M_exp_guild) / M_sd_guild
SES_guild[!is.finite(SES_guild)] <- NA

# -----------------------------
# Two-tailed Monte Carlo p-values per cell
# -----------------------------

obs_long <- as.data.frame(M_obs_guild) %>%
  rownames_to_column("Plant_family") %>%
  pivot_longer(
    -Plant_family,
    names_to = "Pollinator_guild",
    values_to = "Observed"
  )

null_long <- map2_dfr(
  null_list_guild,
  seq_along(null_list_guild),
  ~ as.data.frame(.x) %>%
    rownames_to_column("Plant_family") %>%
    pivot_longer(
      -Plant_family,
      names_to = "Pollinator_guild",
      values_to = "Simulated"
    ) %>%
    mutate(SimID = .y)
)

pval_df_guild <- null_long %>%
  inner_join(obs_long, by = c("Plant_family", "Pollinator_guild")) %>%
  group_by(Plant_family, Pollinator_guild) %>%
  summarise(
    p_hi  = (sum(Simulated >= Observed) + 1) / (n.sim + 1),
    p_lo  = (sum(Simulated <= Observed) + 1) / (n.sim + 1),
    p_two = pmin(1, 2 * pmin(p_hi, p_lo)),
    .groups = "drop"
  )

# -----------------------------
# Save outputs
# -----------------------------

saveRDS(block_mats_guild, "Data/Working_files/block_mats_guild_split_bees75.rds")
saveRDS(M_obs_guild, "Data/Working_files/M_obs_guild_split_bees75.rds")
saveRDS(M_exp_guild, "Data/Working_files/M_exp_guild_split_bees75.rds")
saveRDS(M_sd_guild, "Data/Working_files/M_sd_guild_split_bees75.rds")
saveRDS(SES_guild, "Data/Working_files/SES_guild_split_bees75.rds")
saveRDS(pval_df_guild, "Data/Working_files/pval_df_guild_split_bees75.rds")