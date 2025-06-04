# ======================================================
# Libraries
# ======================================================
library(dplyr)
library(purrr)
library(tidyr)
library(ggplot2)
library(vegan)

# ======================================================
# Read network data
# ======================================================
prob_matrices_by_garden <- readRDS("Data/Working_files/phenology_networks_only_phenobs_pheno_by_week.rds")

# ======================================================
# Filter out rows with NULL or too-small matrices (less than 2x2)
# ======================================================
prob_matrices_by_garden <- prob_matrices_by_garden %>%
  filter(!map_lgl(Interaction_network, is.null),
         !map_lgl(Prob_matrix, is.null)) %>%
  filter(
    map_lgl(Interaction_network, ~ all(dim(.) >= 2)),
    map_lgl(Prob_matrix, ~ all(dim(.) >= 2))
  )

# ======================================================
# Prepare unique garden-week combinations
# ======================================================
garden_season_combos <- prob_matrices_by_garden %>%
  select(Botanical_garden, Sampling_week) %>%
  distinct()

# ======================================================
# Mantel Test Function with debugging and safety
# ======================================================
mantel_network_abundance <- function(garden_name, week, network_type) {
  
  df_filtered <- prob_matrices_by_garden %>%
    filter(Botanical_garden == garden_name, Sampling_week == week)
  
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
  
  # Debug info
  print(paste("Garden:", garden_name, "Week:", week))
  print(paste("Interaction matrix class:", class(interaction_network), "dim:", paste(dim(interaction_network), collapse="x")))
  print(paste("Abundance matrix class:", class(abundance_network), "dim:", paste(dim(abundance_network), collapse="x")))
  
  # Check for NULL or NA matrices
  if (is.null(interaction_network) | is.null(abundance_network) |
      anyNA(interaction_network) | anyNA(abundance_network)) {
    message("Skipping ", garden_name, " ", week, " — invalid matrices (NULL or NAs).")
    return(tibble(Botanical_garden = garden_name, Sampling_week = week, Test = network_type, Mantel_corr = NA, Mantel_Pval = NA))
  }
  
  # Check dimensions match
  if (!all(dim(interaction_network) == dim(abundance_network))) {
    message("Skipping ", garden_name, " ", week, " — matrices not same size.")
    return(tibble(Botanical_garden = garden_name, Sampling_week = week, Test = network_type, Mantel_corr = NA, Mantel_Pval = NA))
  }
  
  # Compute distance matrices using Bray-Curtis (vegdist)
  dist1 <- vegdist(interaction_network, method = "bray")
  dist2 <- vegdist(abundance_network, method = "bray")
  
  # Mantel test
  mantel_result <- mantel(dist1, dist2, method = "pearson", permutations = 999)
  
  return(tibble(Botanical_garden = garden_name,
                Sampling_week = week,
                Test = network_type,
                Mantel_corr = mantel_result$statistic,
                Mantel_Pval = mantel_result$signif))
}

# ======================================================
# Run Mantel tests for both network types
# ======================================================
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

# ======================================================
# Combine and clean results
# ======================================================
combined_results <- bind_rows(results_int_abund, results_intFreq_abund) %>%
  mutate(Mantel_corr = abs(Mantel_corr))

# ======================================================
# Plot Mantel correlations by garden and week
# ======================================================
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
