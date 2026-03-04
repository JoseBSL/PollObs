# ======================================================
# Prepare family-level plant–pollinator networks by garden × week
# Phenology-aware nulls (randomize within garden-week blocks)
# Outputs: M_obs, M_exp, M_sd, SES, pval_df
# ======================================================

library(dplyr)
library(tidyr)
library(purrr)
library(tibble)
library(bipartite)

# -----------------------------
# Load data
# -----------------------------
raw_data <- readRDS("Data/Working_files/interaction_data.rds")

# -----------------------------
# Add ISO week (2023)
# -----------------------------
dates <- raw_data %>%
  distinct(Botanical_garden, Date) %>%
  mutate(Sampling_week = isoweek(Date))

# -----------------------------
# Filter interactions
# -----------------------------
poll_order <- c("Hymenoptera", "Diptera", "Coleoptera", "Lepidoptera")

interaction_data <- raw_data %>%
  filter(!is.na(Interactions),
         !is.na(Floral_abundance),
         Pollinator != "None") %>%
  rename(
    Plants = Plant_accepted_name,
    Pollinators = Pollinator_accepted_name
  ) %>%
  filter(!is.na(Pollinators),
         Pollinator_rank == "SPECIES",
         Pollinator_order %in% poll_order,
         Plants != "Iberis sempervirens") %>%
  left_join(dates, by = c("Botanical_garden", "Date"))

# -----------------------------
# Helper: garden-week -> family matrix (counts)
# -----------------------------
to_network_family <- function(df) {
  df %>%
    count(Plant_family, Pollinator_family, wt = Interactions, name = "Total_interactions") %>%
    pivot_wider(names_from = Pollinator_family,
                values_from = Total_interactions,
                values_fill = 0) %>%
    column_to_rownames("Plant_family") %>%
    as.matrix()
}

# -----------------------------
# Build networks per garden × week
# -----------------------------
networks_by_garden_week <- interaction_data %>%
  group_by(Botanical_garden, Sampling_week) %>%
  nest() %>%
  mutate(Interaction_network_family = map(data, to_network_family)) %>%
  select(-data) %>%
  ungroup()

# -----------------------------
# Filter out small / unstable networks
# -----------------------------
networks_by_garden_week <- networks_by_garden_week %>%
  filter(map_int(Interaction_network_family, sum) >= 20) %>%
  filter(map_int(Interaction_network_family, nrow) >= 2,
         map_int(Interaction_network_family, ncol) >= 2)

# -----------------------------
# Extract matrices and pad to common dimension
# -----------------------------
block_mats <- networks_by_garden_week$Interaction_network_family

all_rows <- sort(unique(unlist(map(block_mats, rownames))))
all_cols <- sort(unique(unlist(map(block_mats, colnames))))

pad_matrix <- function(M, all_rows, all_cols) {
  out <- matrix(0,
                nrow = length(all_rows),
                ncol = length(all_cols),
                dimnames = list(all_rows, all_cols))
  out[rownames(M), colnames(M)] <- M
  out
}

block_mats <- map(block_mats, pad_matrix, all_rows = all_rows, all_cols = all_cols)

# Observed aggregated matrix
M_obs <- Reduce(`+`, block_mats)

# -----------------------------
# Null simulations: randomize within each block, then sum
# -----------------------------
set.seed(1)
n.sim <- 1000

simulate_one <- function() {
  rand_blocks <- lapply(block_mats, function(M) {
    R <- nullmodel(M, N = 1, method = "r2dtable")[[1]]
    dimnames(R) <- dimnames(M)  # keep family names
    R
  })
  Reduce(`+`, rand_blocks)
}

null_list <- replicate(n.sim, simulate_one(), simplify = FALSE)

# Expected matrix
M_exp <- Reduce(`+`, null_list) / n.sim

# SD matrix (robust)
ssq <- matrix(0, nrow = nrow(M_exp), ncol = ncol(M_exp), dimnames = dimnames(M_exp))
for (k in seq_along(null_list)) ssq <- ssq + (null_list[[k]] - M_exp)^2
M_sd <- sqrt(ssq / n.sim)
M_sd[M_sd == 0] <- NA

# SES
SES <- (M_obs - M_exp) / M_sd
SES[!is.finite(SES)] <- NA

# -----------------------------
# Two-tailed Monte Carlo p-values per cell
# -----------------------------
obs_long <- as.data.frame(M_obs) %>%
  rownames_to_column("Plant_family") %>%
  pivot_longer(-Plant_family, names_to = "Pollinator_family", values_to = "Observed")

null_long <- map2_dfr(
  null_list, seq_along(null_list),
  ~ as.data.frame(.x) %>%
    rownames_to_column("Plant_family") %>%
    pivot_longer(-Plant_family, names_to = "Pollinator_family", values_to = "Simulated") %>%
    mutate(SimID = .y)
)

pval_df <- null_long %>%
  inner_join(obs_long, by = c("Plant_family", "Pollinator_family")) %>%
  group_by(Plant_family, Pollinator_family) %>%
  summarise(
    p_hi  = (sum(Simulated >= Observed) + 1) / (n.sim + 1),
    p_lo  = (sum(Simulated <= Observed) + 1) / (n.sim + 1),
    p_two = pmin(1, 2 * pmin(p_hi, p_lo)),
    .groups = "drop"
  )