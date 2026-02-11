# ======================================================
# Compute Mantel test between int and int freq and prob abundance matrix
# ======================================================

library(dplyr)
library(purrr)
library(tidyr)
library(ggplot2)
library(vegan)

# ======================================================
# Read network data
prob_matrices_by_garden <- readRDS("Data/Working_files/abundance_networks_only_phenobs_by_week.rds")

# ======================================================
# Generalized Mantel Test Function
mantel_network_abundance <- function(garden_name, week, network_type) {
  
  df_filtered <- prob_matrices_by_garden %>%
    filter(Botanical_garden == garden_name, Sampling_week == week)
  
  # Safety checks
  if (nrow(df_filtered) == 0) {
    message("Skipping ", garden_name, " ", week, " — no data.")
    return(tibble(Botanical_garden = garden_name, Sampling_week = week, Test = network_type, Mantel_corr = NA, Mantel_Pval = NA))
  }
  
  interaction_network <- df_filtered %>%
    pull(!!sym(network_type)) %>%
    .[[1]]
  
  abundance_network <- df_filtered %>%
    pull(Prob_matrix) %>%
    .[[1]]
  
  # Check for valid matrices
  if (is.null(interaction_network) | is.null(abundance_network) |
      nrow(interaction_network) < 2 | nrow(abundance_network) < 2 |
      anyNA(interaction_network) | anyNA(abundance_network)) {
    message("Skipping ", garden_name, " ", week, " — invalid matrices.")
    return(tibble(Botanical_garden = garden_name, Sampling_week = week, Test = network_type, Mantel_corr = NA, Mantel_Pval = NA))
  }
  
  # Compute distance matrices and Mantel test
  dist1 <- dist(interaction_network)
  dist2 <- dist(abundance_network)
  mantel_result <- mantel(dist1, dist2, method = "pearson", permutations = 999)
  
  return(tibble(Botanical_garden = garden_name,
                Sampling_week = week,
                Test = network_type,
                Mantel_corr = mantel_result$statistic,
                Mantel_Pval = mantel_result$signif))
}

# ======================================================
# Prepare combinations to test
garden_season_combos <- prob_matrices_by_garden %>%
  select(Botanical_garden, Sampling_week) %>%
  distinct()

# Run Mantel tests for both network types
results_int_abund <- pmap_dfr(
  list(
    garden_name = garden_season_combos$Botanical_garden,
    week = garden_season_combos$Sampling_week
  ),
  ~ mantel_network_abundance(..1, ..2, "Interaction_network")
)

results_intFreq_abund <- pmap_dfr(
  list(
    garden_name = garden_season_combos$Botanical_garden,
    week = garden_season_combos$Sampling_week
  ),
  ~ mantel_network_abundance(..1, ..2, "Int_frequency_network")
)

# Combine results
combined_results <- bind_rows(results_int_abund, results_intFreq_abund) %>%
  mutate(Mantel_corr = abs(Mantel_corr))

results_intFreq_abund$Mantel_corr = abs(results_intFreq_abund$Mantel_corr)

saveRDS(results_intFreq_abund, "Data/Working_files/Mantel_abund_week_result.rds")
# ======================================================
# Plot
ggplot(combined_results, aes(x = Sampling_week, 
                             y = Mantel_corr, 
                             fill = Test)) +
  geom_bar(aes(group = interaction(Sampling_week, Test)), 
           stat = "identity", 
           position = position_dodge(width = 0.8)) +
  facet_wrap(~Botanical_garden, nrow = 1) +
  theme_minimal() +
  labs(x = "Sampling Week", 
       y = "Mantel Correlation", 
       title = "Mantel correlation by Garden, Week and Test Type",
       fill = "Test Type") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylim(0, 1)
