# ======================================================
# Procrustes + PROTEST by SEASON (VISITATION RATE ONLY): VisitRate–Abundance
# (Bray-Curtis + keeps all rows; fixes empty-row Bray NA with tiny eps)
# ======================================================

library(dplyr)
library(purrr)
library(ggplot2)
library(vegan)

# Read seasonal network data
prob_matrices_by_garden <- readRDS("Data/Working_files/abundance_networks_only_phenobs_by_season.rds")

# Helpers
pcoa_scores <- function(d, k) cmdscale(d, k = k, eig = TRUE, add = TRUE)$points

make_bray_safe <- function(M, eps = 1e-10) {
  M <- as.matrix(M)
  M[!is.finite(M)] <- 0
  empty <- rowSums(M) == 0
  if (any(empty)) M[empty, ] <- M[empty, ] + eps
  M
}

# Function: Procrustes + PROTEST for one garden × season
protest_visitRate_abundance_season <- function(garden_name, season_name, k, permutations = 999) {
  
  visit_rate_network <- prob_matrices_by_garden %>%
    filter(Botanical_garden == garden_name, Season == season_name) %>%
    pull(Int_frequency_network) %>%
    .[[1]]
  
  abundance_network <- prob_matrices_by_garden %>%
    filter(Botanical_garden == garden_name, Season == season_name) %>%
    pull(Prob_matrix) %>%
    .[[1]]
  
  # Align by rownames if present
  if (!is.null(rownames(visit_rate_network)) && !is.null(rownames(abundance_network))) {
    common <- intersect(rownames(visit_rate_network), rownames(abundance_network))
    visit_rate_network <- visit_rate_network[common, , drop = FALSE]
    abundance_network  <- abundance_network[common, , drop = FALSE]
  }
  
  # Clean for Bray + keep all rows
  visit_rate_network <- make_bray_safe(visit_rate_network)
  abundance_network  <- make_bray_safe(abundance_network)
  
  n <- nrow(visit_rate_network)
  if (n < 3) {
    return(tibble(
      Botanical_garden = garden_name,
      Season = season_name,
      k = NA_integer_,
      Procrustes_m2 = NA_real_,
      Procrustes_r  = NA_real_,
      PROTEST_Pval  = NA_real_
    ))
  }
  
  d1 <- dist(visit_rate_network)
  d2 <- dist(abundance_network)
  
  k_eff <- min(k, n - 1)
  
  X <- pcoa_scores(d1, k_eff)
  Y <- pcoa_scores(d2, k_eff)
  
  proc <- procrustes(X, Y, symmetric = TRUE)
  prot <- protest(X, Y, permutations = permutations, symmetric = TRUE)
  
  m2 <- proc$ss
  r  <- sqrt(1 - m2)
  
  tibble(
    Botanical_garden = garden_name,
    Season = season_name,
    k = k_eff,
    Procrustes_m2 = as.numeric(m2),
    Procrustes_r  = as.numeric(r),
    PROTEST_Pval  = as.numeric(prot$signif)
  )
}

# Garden × season combinations
garden_season_combos <- prob_matrices_by_garden %>%
  select(Botanical_garden, Season) %>%
  distinct()

# Choose k (use the plateau value you decided)
k_final <- 10

# Run PROTEST for all combos
results_protest_season <- pmap_dfr(
  list(
    garden_name = garden_season_combos$Botanical_garden,
    season_name = garden_season_combos$Season
  ),
  ~ protest_visitRate_abundance_season(..1, ..2, k = k_final, permutations = 999)
) %>%
  mutate(Season = factor(Season, levels = c("Early", "Mid", "Late")))

# Save
saveRDS(results_protest_season, "Data/Working_files/PROTEST_abund_season_result.rds")

# Plot
ggplot(results_protest_season, aes(x = Season, y = Procrustes_r, fill = Season)) +
  geom_col(width = 0.75) +
  facet_wrap(~Botanical_garden, nrow = 1) +
  theme_minimal() +
  coord_cartesian(ylim = c(0, 1)) +
  labs(
    x = "Season",
    y = "Procrustes r",
    title = "VisitRate–Abundance Procrustes (PROTEST) by garden and season"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")

# -------------------------
# k-curve / plateau check
# -------------------------
k_grid <- 2:40
scan_perms <- 0  # not used (we only compute Procrustes r for the curve)

curve_df <- pmap_dfr(
  list(garden_season_combos$Botanical_garden, garden_season_combos$Season),
  ~ map_dfr(k_grid, function(kk) {
    protest_visitRate_abundance_season(..1, ..2, k = kk, permutations = 0)
  })
) %>%
  mutate(Season = factor(Season, levels = c("Early", "Mid", "Late")))

# Plot curves: per garden, colored by season
ggplot(curve_df, aes(x = k, y = Procrustes_r, color = Season, group = Season)) +
  geom_line() +
  facet_wrap(~ Botanical_garden) +
  theme_minimal() +
  labs(
    x = "k (PCoA axes)",
    y = "Procrustes r",
    title = "k-curve check: Procrustes r vs k (by season)"
  ) +
  coord_cartesian(ylim = c(0, 1))
