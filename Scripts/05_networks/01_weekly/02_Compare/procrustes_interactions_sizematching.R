# ======================================================
# Procrustes + PROTEST by WEEK: interaction networks ~ size/trait matching (Trait_prob)
# Runs for two network types: Interaction_network and Int_frequency_network
# Distance: Bray-Curtis (vegdist) with eps fix for empty rows
# + k-curve check (optional, fast)
# ======================================================

library(dplyr)
library(purrr)
library(ggplot2)
library(vegan)

# -----------------------------
# Load data
# -----------------------------
prob_matrices_by_garden <- readRDS("Data/Working_files/trait_probability_networks_only_phenobs_by_week.rds")

# -----------------------------
# Robust matrix check (prevents nrow() length-0 errors)
# -----------------------------
ok_mat_n3 <- function(x) {
  !is.null(x) && length(dim(x)) == 2 && nrow(x) >= 3 && ncol(x) >= 2
}

# Keep only rows with usable matrices for BOTH interaction nets and Trait_prob
# (We don't filter Interaction_network and Int_frequency_network here because we run both later;
#  we just ensure Trait_prob is usable, and combos exist.)
prob_matrices_by_garden <- prob_matrices_by_garden %>%
  filter(map_lgl(Trait_prob, ok_mat_n3))

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
# One garden × week × network_type: full Procrustes + PROTEST
# -----------------------------
protest_network_trait_week <- function(garden_name, week, network_type, k, permutations = 999) {
  
  df_filtered <- prob_matrices_by_garden %>%
    filter(Botanical_garden == garden_name, Sampling_week == week)
  
  if (nrow(df_filtered) == 0) {
    return(tibble(
      Botanical_garden = garden_name,
      Sampling_week = week,
      Test = network_type,
      k = NA_integer_,
      Procrustes_m2 = NA_real_,
      Procrustes_r  = NA_real_,
      PROTEST_Pval  = NA_real_
    ))
  }
  
  interaction_network <- df_filtered %>% pull(!!sym(network_type)) %>% .[[1]]
  trait_network       <- df_filtered$Trait_prob[[1]]
  
  if (!ok_mat_n3(interaction_network) || !ok_mat_n3(trait_network)) {
    return(tibble(
      Botanical_garden = garden_name,
      Sampling_week = week,
      Test = network_type,
      k = NA_integer_,
      Procrustes_m2 = NA_real_,
      Procrustes_r  = NA_real_,
      PROTEST_Pval  = NA_real_
    ))
  }
  
  # Align by rownames if present
  if (!is.null(rownames(interaction_network)) && !is.null(rownames(trait_network))) {
    common <- intersect(rownames(interaction_network), rownames(trait_network))
    interaction_network <- interaction_network[common, , drop = FALSE]
    trait_network       <- trait_network[common, , drop = FALSE]
  }
  
  interaction_network <- make_bray_safe(interaction_network)
  trait_network       <- make_bray_safe(trait_network)
  
  n <- min(nrow(interaction_network), nrow(trait_network))
  if (n < 3) {
    return(tibble(
      Botanical_garden = garden_name,
      Sampling_week = week,
      Test = network_type,
      k = NA_integer_,
      Procrustes_m2 = NA_real_,
      Procrustes_r  = NA_real_,
      PROTEST_Pval  = NA_real_
    ))
  }
  
  d1 <- dist(interaction_network)
  d2 <- dist(trait_network)
  
  k_eff <- min(k, n - 1)
  
  X <- pcoa_scores(d1, k_eff)
  Y <- pcoa_scores(d2, k_eff)
  
  proc <- procrustes(X, Y, symmetric = TRUE)
  prot <- protest(X, Y, permutations = permutations, symmetric = TRUE)
  
  tibble(
    Botanical_garden = garden_name,
    Sampling_week = week,
    Test = network_type,
    k = k_eff,
    Procrustes_m2 = as.numeric(proc$ss),
    Procrustes_r  = as.numeric(sqrt(1 - proc$ss)),
    PROTEST_Pval  = as.numeric(prot$signif)
  )
}

# -----------------------------
# Optional: Procrustes r only for k-curve (fast)
# -----------------------------
procrustes_r_network_trait_week <- function(garden_name, week, network_type, k) {
  
  df_filtered <- prob_matrices_by_garden %>%
    filter(Botanical_garden == garden_name, Sampling_week == week)
  
  if (nrow(df_filtered) == 0) {
    return(tibble(
      Botanical_garden = garden_name,
      Sampling_week = week,
      Test = network_type,
      k = NA_integer_,
      Procrustes_r = NA_real_
    ))
  }
  
  interaction_network <- df_filtered %>% pull(!!sym(network_type)) %>% .[[1]]
  trait_network       <- df_filtered$Trait_prob[[1]]
  
  if (!ok_mat_n3(interaction_network) || !ok_mat_n3(trait_network)) {
    return(tibble(
      Botanical_garden = garden_name,
      Sampling_week = week,
      Test = network_type,
      k = NA_integer_,
      Procrustes_r = NA_real_
    ))
  }
  
  if (!is.null(rownames(interaction_network)) && !is.null(rownames(trait_network))) {
    common <- intersect(rownames(interaction_network), rownames(trait_network))
    interaction_network <- interaction_network[common, , drop = FALSE]
    trait_network       <- trait_network[common, , drop = FALSE]
  }
  
  interaction_network <- make_bray_safe(interaction_network)
  trait_network       <- make_bray_safe(trait_network)
  
  n <- min(nrow(interaction_network), nrow(trait_network))
  if (n < 3) {
    return(tibble(
      Botanical_garden = garden_name,
      Sampling_week = week,
      Test = network_type,
      k = NA_integer_,
      Procrustes_r = NA_real_
    ))
  }
  
  d1 <- vegdist(interaction_network, method = "bray")
  d2 <- vegdist(trait_network, method = "bray")
  
  k_eff <- min(k, n - 1)
  
  X <- pcoa_scores(d1, k_eff)
  Y <- pcoa_scores(d2, k_eff)
  
  r <- sqrt(1 - procrustes(X, Y, symmetric = TRUE)$ss)
  
  tibble(
    Botanical_garden = garden_name,
    Sampling_week = week,
    Test = network_type,
    k = k_eff,
    Procrustes_r = as.numeric(r)
  )
}

# -----------------------------
# Combos to test
# -----------------------------
garden_week_combos <- prob_matrices_by_garden %>%
  select(Botanical_garden, Sampling_week) %>%
  distinct()

k_final <- 10  # set from your plateau

# Run PROTEST for both network types
results_int_trait <- pmap_dfr(
  list(garden_week_combos$Botanical_garden, garden_week_combos$Sampling_week),
  ~ protest_network_trait_week(..1, ..2, "Interaction_network", k = k_final, permutations = 999)
)

results_intFreq_trait <- pmap_dfr(
  list(garden_week_combos$Botanical_garden, garden_week_combos$Sampling_week),
  ~ protest_network_trait_week(..1, ..2, "Int_frequency_network", k = k_final, permutations = 999)
)

combined_results <- bind_rows(results_int_trait, results_intFreq_trait)

saveRDS(combined_results, "Data/Working_files/PROTEST_trait_week_result.rds")

# -----------------------------
# Plot (Procrustes r)
# -----------------------------
ggplot(combined_results, aes(x = Sampling_week, y = Procrustes_r, fill = Test)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.75) +
  facet_wrap(~Botanical_garden, nrow = 1) +
  theme_minimal() +
  labs(
    x = "Sampling Week",
    y = "Procrustes r",
    title = "Procrustes (PROTEST) by Garden, Week and Test Type (size/trait matching)",
    fill = "Test Type"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  coord_cartesian(ylim = c(0, 1))

