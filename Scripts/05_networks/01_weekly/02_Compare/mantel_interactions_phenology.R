# ======================================================
# Mantel test: VISITATION RATE (Int_frequency_network) ~ phenology (Prob_matrix) by garden & week
# ======================================================

# Libraries
library(dplyr)
library(purrr)
library(tidyr)
library(tibble)
library(ggplot2)
library(vegan)

# ======================================================
# Read data (contains Int_frequency_network + Prob_matrix)
# ======================================================
prob_matrices_by_garden <- readRDS("Data/Working_files/phenology_networks_only_phenobs_pheno_by_week.rds")

# ======================================================
# Filter out rows with NULL or too-small matrices (less than 2x2)
# ======================================================
prob_matrices_by_garden <- prob_matrices_by_garden %>%
  filter(!map_lgl(Int_frequency_network, is.null),
         !map_lgl(Prob_matrix, is.null)) %>%
  filter(
    map_lgl(Int_frequency_network, ~ all(dim(.) >= 2)),
    map_lgl(Prob_matrix, ~ all(dim(.) >= 2))
  )

# ======================================================
# Unique garden-week combinations
# ======================================================
garden_week_combos <- prob_matrices_by_garden %>%
  select(Botanical_garden, Sampling_week) %>%
  distinct()

# ======================================================
# Mantel test function (visitation rate only)
# ======================================================
mantel_visitRate_prob <- function(garden_name, week) {
  
  df_filtered <- prob_matrices_by_garden %>%
    filter(Botanical_garden == garden_name, Sampling_week == week)
  
  if (nrow(df_filtered) == 0) {
    message("Skipping ", garden_name, " ", week, " — no data.")
    return(tibble(Botanical_garden = garden_name, Sampling_week = week,
                  Test = "VisitRate-Phenology", Mantel_corr = NA, Mantel_Pval = NA))
  }
  
  interaction_network <- df_filtered %>%
    pull(Int_frequency_network) %>%
    .[[1]]
  
  phenology_network <- df_filtered %>%
    pull(Prob_matrix) %>%
    .[[1]]
  
  # Safety checks
  if (is.null(interaction_network) | is.null(phenology_network) |
      anyNA(interaction_network) | anyNA(phenology_network)) {
    message("Skipping ", garden_name, " ", week, " — invalid matrices (NULL or NAs).")
    return(tibble(Botanical_garden = garden_name, Sampling_week = week,
                  Test = "VisitRate-Phenology", Mantel_corr = NA, Mantel_Pval = NA))
  }
  
  if (!all(dim(interaction_network) == dim(phenology_network))) {
    message("Skipping ", garden_name, " ", week, " — matrices not same size.")
    return(tibble(Botanical_garden = garden_name, Sampling_week = week,
                  Test = "VisitRate-Phenology", Mantel_corr = NA, Mantel_Pval = NA))
  }
  
  # Distances (Bray-Curtis)
  dist1 <- vegdist(interaction_network, method = "bray")
  dist2 <- vegdist(phenology_network, method = "bray")
  
  # Mantel test
  mantel_result <- mantel(dist1, dist2, method = "pearson", permutations = 999)
  
  tibble(
    Botanical_garden = garden_name,
    Sampling_week = week,
    Test = "VisitRate-Phenology",
    Mantel_corr = mantel_result$statistic,
    Mantel_Pval = mantel_result$signif
  )
}

# ======================================================
# Run Mantel tests (visitation rate only)
# ======================================================
results_visitRate <- pmap_dfr(
  list(
    garden_name = garden_week_combos$Botanical_garden,
    week = garden_week_combos$Sampling_week
  ),
  mantel_visitRate_prob
) %>%
  mutate(Mantel_corr = abs(Mantel_corr))

# Inspect results
results_visitRate
saveRDS(results_visitRate, "Data/Working_files/Mantel_pheno_week_result.rds")

# ======================================================
# Plot Mantel correlations by garden and week
# ======================================================
ggplot(results_visitRate, aes(x = Sampling_week, y = Mantel_corr)) +
  geom_bar(stat = "identity") +
  facet_wrap(~Botanical_garden, nrow = 1) +
  theme_minimal() +
  labs(
    x = "Sampling Week",
    y = "Mantel Correlation (abs)",
    title = "Mantel correlation by Garden and Week (Visitation Rate vs Phenology)"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylim(0, 1)
