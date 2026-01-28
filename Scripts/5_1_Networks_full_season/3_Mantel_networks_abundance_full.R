# ======================================================
# Mantel test (VISITATION RATE ONLY): VisitRate-Abundance
# ======================================================

library(dplyr)
library(purrr)
library(tidyr)
library(ggplot2)
library(vegan)

# Load data
prob_matrices_by_garden <- readRDS("Data/Working_files/abundance_networks_only_phenobs.rds")

# Function: Mantel (VisitRate-Abundance)
mantel_visitRate_abundance <- function(garden_name) {
  
  visit_rate_network <- prob_matrices_by_garden %>%
    filter(Botanical_garden == garden_name) %>%
    pull(Int_frequency_network) %>%
    .[[1]]
  
  abundance_network <- prob_matrices_by_garden %>%
    filter(Botanical_garden == garden_name) %>%
    pull(Prob_matrix) %>%
    .[[1]]
  
  dist1 <- dist(visit_rate_network)
  dist2 <- dist(abundance_network)
  
  mantel_result <- mantel(dist1, dist2, method = "pearson", permutations = 999)
  
  tibble(
    Botanical_garden = garden_name,
    Test = "VisitRate-Abundance",
    Mantel_corr = as.numeric(mantel_result$statistic),
    Mantel_Pval = as.numeric(mantel_result$signif)
  )
}

# Run for all gardens
gardens <- unique(prob_matrices_by_garden$Botanical_garden)
results_visitRate_abund <- map_dfr(gardens, mantel_visitRate_abundance)

# Save results
saveRDS(results_visitRate_abund, "Data/Working_files/Mantel_abund_full_result.rds")

# Plot
ggplot(results_visitRate_abund, aes(x = Botanical_garden, y = Mantel_corr)) +
  geom_col(fill = "steelblue") +
  theme_minimal() +
  labs(
    x = "Botanical Garden",
    y = "Mantel correlation",
    title = "VisitRate–Abundance Mantel test"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  coord_cartesian(ylim = c(0, 1))
