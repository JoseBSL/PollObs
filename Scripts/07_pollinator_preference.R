# ======================================================
# Pollinator preference exploration at family level
# QUANTITATIVE HEATMAP + significance overlay + long-format output
#   - Builds observed preference matrix (Pollinator_family × Plant_family)
#   - Null model (r2dtable) -> expected mean/SD
#   - SES = (Obs - Exp)/SD
#   - Two-tailed Monte Carlo p-values
#   - Heatmap: SES with diverging gradient + "*" on significant cells
#   - Outputs a clean long-format dataframe (results_long)
# ======================================================

library(bipartite)
library(dplyr)
library(tidyr)
library(purrr)
library(tibble)

library(ComplexHeatmap)
library(circlize)
library(grid)

# -----------------------------
# Load data
# -----------------------------
raw_data    <- readRDS("Data/Working_files/interaction_data.rds")
phenobs_spp <- readRDS("Data/Working_files/phenobs_spp.rds")

# -----------------------------
# Filter + keep only relevant families
# -----------------------------
poll_order <- c("Hymenoptera", "Diptera")

int_data <- raw_data %>%
  filter(Pollinator_order %in% poll_order) %>%
  transmute(
    Plant_family,
    Plant,
    Pollinator_family
  ) %>%
  drop_na()

# keep only plant families present in phenobs subset (your original logic)
phenobs_fam <- int_data %>%
  filter(Plant %in% phenobs_spp) %>%
  distinct(Plant_family) %>%
  pull()

int_data <- int_data %>%
  filter(Plant_family %in% phenobs_fam)

# drop rare plant/pollinator families (< 15 interactions)
int_data <- int_data %>%
  group_by(Plant_family) %>%
  filter(n() >=20) %>%
  ungroup() %>%
  group_by(Pollinator_family) %>%
  filter(n() >= 20) %>%
  ungroup()

# -----------------------------
# Observed preference table
# -----------------------------
pref.table <- int_data %>%
  count(Pollinator_family, Plant_family) %>%
  pivot_wider(names_from = Plant_family, values_from = n, values_fill = 0) %>%
  column_to_rownames("Pollinator_family")

# -----------------------------
# Null models
# -----------------------------
set.seed(1)
n.sim <- 1000
n.mod <- nullmodel(pref.table, N = n.sim, method = "r2dtable")

for (i in seq_len(n.sim)) {
  rownames(n.mod[[i]]) <- rownames(pref.table)
  colnames(n.mod[[i]]) <- colnames(pref.table)
}

# Expected mean & SD under null
expected.means <- reduce(n.mod, `+`) / n.sim

expected.sds <- n.mod %>%
  map(~ (.x - expected.means)^2) %>%
  reduce(`+`) %>%
  (`/`)(n.sim) %>%
  sqrt()

expected.sds[expected.sds == 0] <- NA

# SES matrix
SES <- (as.matrix(pref.table) - expected.means) / expected.sds
SES[!is.finite(SES)] <- NA

# -----------------------------
# Long-format observed + simulated for p-values
# -----------------------------
obs_df <- pref.table %>%
  as.data.frame() %>%
  rownames_to_column("Pollinator_family") %>%
  pivot_longer(-Pollinator_family, names_to = "Plant_family", values_to = "Observed")

sim_df <- map2_dfr(
  n.mod, seq_along(n.mod),
  ~ as.data.frame(.x) %>%
    rownames_to_column("Pollinator_family") %>%
    pivot_longer(-Pollinator_family, names_to = "Plant_family", values_to = "Simulated") %>%
    mutate(SimID = .y)
)

pval_df <- sim_df %>%
  inner_join(obs_df, by = c("Pollinator_family", "Plant_family")) %>%
  group_by(Pollinator_family, Plant_family) %>%
  summarise(
    p_hi  = (sum(Simulated >= Observed) + 1) / (n.sim + 1),
    p_lo  = (sum(Simulated <= Observed) + 1) / (n.sim + 1),
    p_two = pmin(1, 2 * pmin(p_hi, p_lo)),
    .groups = "drop"
  )

# Optional: multiple testing correction (uncomment if desired)
# pval_df <- pval_df %>% mutate(p_fdr = p.adjust(p_two, method = "fdr"))

alpha <- 0.05
p_col <- "p_two"  # or "p_fdr" if you compute it

# -----------------------------
# Build significance matrix (aligned to SES)
# -----------------------------
sig_mat <- pval_df %>%
  transmute(
    Pollinator_family,
    Plant_family,
    Sig = !is.na(.data[[p_col]]) & (.data[[p_col]] < alpha)
  ) %>%
  pivot_wider(names_from = Plant_family, values_from = Sig, values_fill = FALSE) %>%
  column_to_rownames("Pollinator_family") %>%
  as.matrix()

sig_mat <- sig_mat[rownames(SES), colnames(SES)]

# -----------------------------
# Heatmap settings (quantitative, capped for contrast)
# -----------------------------
cap <- 6
SES_plot <- SES
SES_plot[SES_plot >  cap] <-  cap
SES_plot[SES_plot < -cap] <- -cap

col_fun <- colorRamp2(
  c(-cap, 0, cap),
  c("#C65DAE", "white", "#FDB863")
)

ht <- Heatmap(
  SES_plot,
  col = col_fun,
  cluster_rows = FALSE,
  cluster_columns = FALSE,
  column_names_rot = 45,
  column_names_gp = gpar(fontsize = 10),
  row_names_gp = gpar(fontsize = 10),
  rect_gp = gpar(col = "black"),
  na_col = "grey90",
  name = "SES",
  column_title = "Pollinator preference",
  column_title_gp = gpar(fontsize = 16, fontface = "bold"),
  heatmap_legend_param = list(
    title = "SES",
    at = c(-cap, 0, cap),
    labels = c(paste0("-", cap, " (Avoidance)"), "0", paste0("+", cap, " (Association)"))
  ),
  cell_fun = function(j, i, x, y, width, height, fill) {
    if (isTRUE(sig_mat[i, j])) {
      grid.text("*", x = x, y = y, gp = gpar(col = "black", fontsize = 10))
    }
  }
)

draw(ht, padding = unit(c(10, 15, 10, 15), "mm"))

# -----------------------------
# Long-format output: everything in one tidy table
# -----------------------------
ses_long <- as.data.frame(SES) %>%
  rownames_to_column("Pollinator_family") %>%
  pivot_longer(-Pollinator_family, names_to = "Plant_family", values_to = "SES")

results_long <- obs_df %>%
  left_join(ses_long, by = c("Pollinator_family", "Plant_family")) %>%
  left_join(pval_df, by = c("Pollinator_family", "Plant_family")) %>%
  mutate(
    Significant = !is.na(.data[[p_col]]) & (.data[[p_col]] < alpha),
    Direction = case_when(
      Significant & SES > 0 ~ "Association",
      Significant & SES < 0 ~ "Avoidance",
      TRUE ~ "NS"
    )
  )

# results_long now contains: Observed, SES, p_hi, p_lo, p_two (+ Significant, Direction)
# You can save it if you want:
# saveRDS(results_long, "Data/Working_files/pollinator_preference_long.rds")