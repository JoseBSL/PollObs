# ======================================================
# Procrustes + PROTEST: VISITATION RATE (Int_frequency_network) ~ phenology (Prob_matrix)
# by garden & week + k-curve check
# Distance: Bray-Curtis (vegdist) with eps fix for empty rows
# ======================================================

library(dplyr)
library(purrr)
library(ggplot2)
library(vegan)

# -----------------------------
# Load data (contains Int_frequency_network + Prob_matrix)
# -----------------------------
prob_matrices_by_garden <- readRDS("Data/Working_files/phenology_networks_only_phenobs_pheno_by_week.rds")

# -----------------------------
# Robust "is a usable matrix" check (prevents length-0 errors)
# -----------------------------
ok_mat_n3 <- function(x) {
  !is.null(x) && length(dim(x)) == 2 && nrow(x) >= 3 && ncol(x) >= 2
}

# Keep only usable rows (real 2D matrices with >= 3 rows)
prob_matrices_by_garden <- prob_matrices_by_garden %>%
  filter(
    map_lgl(Int_frequency_network, ok_mat_n3),
    map_lgl(Prob_matrix, ok_mat_n3)
  )

# -----------------------------
# Helpers
# -----------------------------
pcoa_scores <- function(d, k) cmdscale(d, k = k, eig = TRUE, add = TRUE)$points

make_bray_safe <- function(M, eps = 1e-10) {
  M <- as.matrix(M)
  M[!is.finite(M)] <- 0
  empty <- rowSums(M) == 0
  if (any(empty)) M[empty, ] <- M[empty, ] + eps
  M
}

# -----------------------------
# One garden × week: full Procrustes + PROTEST at chosen k
# -----------------------------
protest_visitRate_phenology_week <- function(garden_name, week, k, permutations = 999) {
  
  df_filtered <- prob_matrices_by_garden %>%
    filter(Botanical_garden == garden_name, Sampling_week == week)
  
  if (nrow(df_filtered) == 0) {
    return(tibble(
      Botanical_garden = garden_name,
      Sampling_week = week,
      k = NA_integer_,
      Procrustes_m2 = NA_real_,
      Procrustes_r  = NA_real_,
      PROTEST_Pval  = NA_real_
    ))
  }
  
  interaction_network <- df_filtered$Int_frequency_network[[1]]
  phenology_matrix    <- df_filtered$Prob_matrix[[1]]
  
  # Align by rownames if present
  if (!is.null(rownames(interaction_network)) && !is.null(rownames(phenology_matrix))) {
    common <- intersect(rownames(interaction_network), rownames(phenology_matrix))
    interaction_network <- interaction_network[common, , drop = FALSE]
    phenology_matrix    <- phenology_matrix[common, , drop = FALSE]
  }
  
  interaction_network <- make_bray_safe(interaction_network)
  phenology_matrix    <- make_bray_safe(phenology_matrix)
  
  n <- min(nrow(interaction_network), nrow(phenology_matrix))
  if (n < 3) {
    return(tibble(
      Botanical_garden = garden_name,
      Sampling_week = week,
      k = NA_integer_,
      Procrustes_m2 = NA_real_,
      Procrustes_r  = NA_real_,
      PROTEST_Pval  = NA_real_
    ))
  }
  
  d1 <- dist(interaction_network)
  d2 <- dist(phenology_matrix)
  
  k_eff <- min(k, n - 1)
  
  X <- pcoa_scores(d1, k_eff)
  Y <- pcoa_scores(d2, k_eff)
  
  proc <- procrustes(X, Y, symmetric = TRUE)
  prot <- protest(X, Y, permutations = permutations, symmetric = TRUE)
  
  tibble(
    Botanical_garden = garden_name,
    Sampling_week = week,
    k = k_eff,
    Procrustes_m2 = as.numeric(proc$ss),
    Procrustes_r  = as.numeric(sqrt(1 - proc$ss)),
    PROTEST_Pval  = as.numeric(prot$signif)
  )
}

# -----------------------------
# One garden × week: Procrustes r only (fast) for k-curve
# -----------------------------
procrustes_r_visitRate_phenology_week <- function(garden_name, week, k) {
  
  df_filtered <- prob_matrices_by_garden %>%
    filter(Botanical_garden == garden_name, Sampling_week == week)
  
  if (nrow(df_filtered) == 0) {
    return(tibble(
      Botanical_garden = garden_name,
      Sampling_week = week,
      k = NA_integer_,
      Procrustes_r = NA_real_
    ))
  }
  
  interaction_network <- df_filtered$Int_frequency_network[[1]]
  phenology_matrix    <- df_filtered$Prob_matrix[[1]]
  
  if (!is.null(rownames(interaction_network)) && !is.null(rownames(phenology_matrix))) {
    common <- intersect(rownames(interaction_network), rownames(phenology_matrix))
    interaction_network <- interaction_network[common, , drop = FALSE]
    phenology_matrix    <- phenology_matrix[common, , drop = FALSE]
  }
  
  interaction_network <- make_bray_safe(interaction_network)
  phenology_matrix    <- make_bray_safe(phenology_matrix)
  
  n <- min(nrow(interaction_network), nrow(phenology_matrix))
  if (n < 3) {
    return(tibble(
      Botanical_garden = garden_name,
      Sampling_week = week,
      k = NA_integer_,
      Procrustes_r = NA_real_
    ))
  }
  
  d1 <- vegdist(interaction_network, method = "bray")
  d2 <- vegdist(phenology_matrix, method = "bray")
  
  k_eff <- min(k, n - 1)
  
  X <- pcoa_scores(d1, k_eff)
  Y <- pcoa_scores(d2, k_eff)
  
  r <- sqrt(1 - procrustes(X, Y, symmetric = TRUE)$ss)
  
  tibble(
    Botanical_garden = garden_name,
    Sampling_week = week,
    k = k_eff,
    Procrustes_r = as.numeric(r)
  )
}

# -----------------------------
# Garden × week combos
# -----------------------------
garden_week_combos <- prob_matrices_by_garden %>%
  select(Botanical_garden, Sampling_week) %>%
  distinct()

# -----------------------------
# Final run at chosen k
# -----------------------------
k_final <- 10

results_protest_pheno_week <- pmap_dfr(
  list(garden_week_combos$Botanical_garden, garden_week_combos$Sampling_week),
  ~ protest_visitRate_phenology_week(..1, ..2, k = k_final, permutations = 999)
)

saveRDS(results_protest_pheno_week, "Data/Working_files/PROTEST_pheno_week_result.rds")

# Plot: Procrustes r by garden and week
ggplot(results_protest_pheno_week, aes(x = Sampling_week, y = Procrustes_r)) +
  geom_col(width = 0.75) +
  facet_wrap(~Botanical_garden, nrow = 1) +
  theme_minimal() +
  labs(
    x = "Sampling Week",
    y = "Procrustes r",
    title = "VisitRate–Phenology Procrustes (PROTEST) by Garden and Week"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  coord_cartesian(ylim = c(0, 1))

# -----------------------------
# k-curve / plateau check (fast, no permutations)
# -----------------------------
k_grid <- 2:40

curve_df_pheno_week <- pmap_dfr(
  list(garden_week_combos$Botanical_garden, garden_week_combos$Sampling_week),
  ~ map_dfr(k_grid, function(kk) {
    procrustes_r_visitRate_phenology_week(..1, ..2, k = kk)
  })
)

ggplot(curve_df_pheno_week, aes(x = k, y = Procrustes_r, group = Sampling_week)) +
  geom_line(alpha = 0.35) +
  facet_wrap(~Botanical_garden) +
  theme_minimal() +
  labs(
    x = "k (PCoA axes)",
    y = "Procrustes r",
    title = "k-curve check: Procrustes r vs k (VisitRate–Phenology, by week)"
  ) +
  coord_cartesian(ylim = c(0, 1))
