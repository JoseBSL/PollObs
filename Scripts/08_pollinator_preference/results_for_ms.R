# ======================================================
# Recalculate preference / avoidance frequencies
# excluding spatiotemporally impossible interactions
# ======================================================

library(dplyr)
library(tidyr)
library(tibble)
library(purrr)

# -----------------------------
# Load data
# -----------------------------
pval_df   <- readRDS("Data/Working_files/pval_df.rds")
SES       <- readRDS("Data/Working_files/SES.rds")
block_mats <- readRDS("Data/Working_files/block_mats.rds")

# -----------------------------
# Set significance threshold
# -----------------------------
alpha <- 0.01

# -----------------------------
# Identify spatiotemporally possible interactions
# A cell is possible in a block if both row and column are present
# -----------------------------
possible_in_block <- function(M) {
  r_present <- rowSums(M) > 0
  c_present <- colSums(M) > 0
  outer(r_present, c_present, FUN = "&")
}

possible_list <- lapply(block_mats, possible_in_block)

# possible in at least one block across the whole season
possible_any <- Reduce(`|`, possible_list)

# impossible interactions
phenology_impossible <- !possible_any
phenology_impossible <- phenology_impossible[rownames(SES), colnames(SES), drop = FALSE]

# testable interactions
testable <- !phenology_impossible

# -----------------------------
# Build p-value matrix aligned to SES
# -----------------------------
p_mat <- pval_df %>%
  group_by(Plant_family, Pollinator_family) %>%
  summarise(
    p_two = mean(p_two, na.rm = TRUE),  # or first()
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = Pollinator_family,
    values_from = p_two
  ) %>%
  column_to_rownames("Plant_family") %>%
  as.matrix()

p_hi_mat <- pval_df %>%
  select(Plant_family, Pollinator_family, p_hi) %>%
  pivot_wider(
    names_from = Pollinator_family,
    values_from = p_hi
  ) %>%
  column_to_rownames("Plant_family") %>%
  as.matrix()

p_lo_mat <- pval_df %>%
  select(Plant_family, Pollinator_family, p_lo) %>%
  pivot_wider(
    names_from = Pollinator_family,
    values_from = p_lo
  ) %>%
  column_to_rownames("Plant_family") %>%
  as.matrix()

# Align matrices to SES
SES_mat  <- SES
p_mat    <- p_mat   [rownames(SES_mat), colnames(SES_mat), drop = FALSE]
p_hi_mat <- p_hi_mat[rownames(SES_mat), colnames(SES_mat), drop = FALSE]
p_lo_mat <- p_lo_mat[rownames(SES_mat), colnames(SES_mat), drop = FALSE]

# -----------------------------
# Define valid cells
# valid = testable + non-missing SES + non-missing p-value
# -----------------------------
valid <- testable & !is.na(SES_mat) & !is.na(p_mat)

# -----------------------------
# Classify interactions
# preference  = observed > expected and significant
# avoidance   = observed < expected and significant
# neutral     = valid but not significant
# impossible  = not testable
# not_tested  = missing SES or p-values
# -----------------------------
direction <- matrix(
  "not_tested",
  nrow = nrow(SES_mat),
  ncol = ncol(SES_mat),
  dimnames = dimnames(SES_mat)
)

direction[testable] <- "neutral"
direction[!testable] <- "impossible"

direction[ valid & (p_mat < alpha) & (SES_mat > 0) ] <- "preference"
direction[ valid & (p_mat < alpha) & (SES_mat < 0) ] <- "avoidance"

# Optional: stricter classification using p_hi / p_lo explicitly
# direction[ valid & (p_hi_mat < alpha) ] <- "preference"
# direction[ valid & (p_lo_mat < alpha) ] <- "avoidance"

# -----------------------------
# Convert to long format
# -----------------------------
direction_df <- as.data.frame(direction) %>%
  rownames_to_column("Plant_family") %>%
  pivot_longer(
    -Plant_family,
    names_to = "Pollinator_family",
    values_to = "direction"
  )

valid_df <- as.data.frame(valid) %>%
  rownames_to_column("Plant_family") %>%
  pivot_longer(
    -Plant_family,
    names_to = "Pollinator_family",
    values_to = "valid"
  )

testable_df <- as.data.frame(testable) %>%
  rownames_to_column("Plant_family") %>%
  pivot_longer(
    -Plant_family,
    names_to = "Pollinator_family",
    values_to = "testable"
  )

SES_df <- as.data.frame(SES_mat) %>%
  rownames_to_column("Plant_family") %>%
  pivot_longer(
    -Plant_family,
    names_to = "Pollinator_family",
    values_to = "SES"
  )

p_df <- as.data.frame(p_mat) %>%
  rownames_to_column("Plant_family") %>%
  pivot_longer(
    -Plant_family,
    names_to = "Pollinator_family",
    values_to = "p_two"
  )

summary_cells <- direction_df %>%
  left_join(valid_df, by = c("Plant_family", "Pollinator_family")) %>%
  left_join(testable_df, by = c("Plant_family", "Pollinator_family")) %>%
  left_join(SES_df, by = c("Plant_family", "Pollinator_family")) %>%
  left_join(p_df, by = c("Plant_family", "Pollinator_family"))

# -----------------------------
# Summarise ONLY testable interactions
# -----------------------------
summary_tab <- summary_cells %>%
  filter(testable) %>%
  count(direction) %>%
  mutate(prop = 100 * n / sum(n))

summary_tab

# -----------------------------
# Optional: summary with all categories
# -----------------------------
summary_tab_all <- summary_cells %>%
  count(direction) %>%
  mutate(prop = 100 * n / sum(n))

summary_tab_all

# -----------------------------
# Optional: extract percentages for writing
# -----------------------------
pct_neutral <- summary_tab %>%
  filter(direction == "neutral") %>%
  pull(prop)

pct_preference <- summary_tab %>%
  filter(direction == "preference") %>%
  pull(prop)

pct_avoidance <- summary_tab %>%
  filter(direction == "avoidance") %>%
  pull(prop)

pct_neutral
pct_preference
pct_avoidance

# -----------------------------
# Optional: save outputs
# -----------------------------
saveRDS(summary_cells, "Data/Working_files/preference_cells_summary.rds")
saveRDS(summary_tab,   "Data/Working_files/preference_summary_tab.rds")