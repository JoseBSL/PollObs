# ======================================================
# Procrustes + PROTEST by WEEK: interaction networks ~ abundance Prob_matrix
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
prob_matrices_by_garden <- readRDS("Data/Working_files/abundance_networks_only_phenobs_by_week.rds")

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
protest_network_abundance_week <- function(garden_name, week, network_type, k, permutations = 999) {
  
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
  
  interaction_network <- df_filtered %>%
    pull(!!sym(network_type)) %>%
    .[[1]]
  
  abundance_network <- df_filtered %>%
    pull(Prob_matrix) %>%
    .[[1]]
  
  if (is.null(interaction_network) || is.null(abundance_network) ||
      nrow(interaction_network) < 3 || nrow(abundance_network) < 3) {
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
  if (!is.null(rownames(interaction_network)) && !is.null(rownames(abundance_network))) {
    common <- intersect(rownames(interaction_network), rownames(abundance_network))
    interaction_network <- interaction_network[common, , drop = FALSE]
    abundance_network   <- abundance_network[common, , drop = FALSE]
  }
  
  interaction_network <- make_bray_safe(interaction_network)
  abundance_network   <- make_bray_safe(abundance_network)
  
  n <- min(nrow(interaction_network), nrow(abundance_network))
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
  d2 <- dist(abundance_network)
  
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
# Combos to test
# -----------------------------
garden_week_combos <- prob_matrices_by_garden %>%
  select(Botanical_garden, Sampling_week) %>%
  distinct()

k_final <- 10  # set from your plateau (adjust if needed)

# Run PROTEST for both network types
results_int_abund <- pmap_dfr(
  list(garden_week_combos$Botanical_garden, garden_week_combos$Sampling_week),
  ~ protest_network_abundance_week(..1, ..2, "Interaction_network", k = k_final, permutations = 999)
)

results_intFreq_abund <- pmap_dfr(
  list(garden_week_combos$Botanical_garden, garden_week_combos$Sampling_week),
  ~ protest_network_abundance_week(..1, ..2, "Int_frequency_network", k = k_final, permutations = 999)
)

combined_results <- bind_rows(results_int_abund, results_intFreq_abund)

saveRDS(combined_results, "Data/Working_files/PROTEST_abund_week_result.rds")

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
    title = "VisitRate–Abundance Procrustes (PROTEST) by Garden, Week and Network Type",
    fill = "Network Type"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  coord_cartesian(ylim = c(0, 1))

