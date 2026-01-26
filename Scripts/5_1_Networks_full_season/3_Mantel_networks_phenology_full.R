# ======================================================
# Compute Mantel test: VISITATION RATE network ~ phenology matrix
# ======================================================

# Load libraries
library(dplyr)
library(purrr)
library(tibble)
library(ggplot2)
library(vegan) # mantel

# ======================================================
# Load data
# Phenology matrices
prob_matrices_by_garden <- readRDS("Data/Working_files/phenology_networks_only_phenobs_pheno.rds")
# Visitation rate networks (Int_frequency_network)
net_by_garden <- readRDS("Data/Working_files/networks_by_garden_only_phenobs_pheno.rds")

# ======================================================
# Function: Mantel visitation RATE network ~ phenology
mantel_visitRate_abundance <- function(garden_name) {
  
  interaction_network <- net_by_garden %>%
    filter(Botanical_garden == garden_name) %>%
    pull(Int_frequency_network) %>%
    .[[1]]
  
  phenology_network <- prob_matrices_by_garden %>%
    filter(Botanical_garden == garden_name) %>%
    pull(Prob_matrix) %>%
    .[[1]]
  
  dist1 <- dist(interaction_network)
  dist2 <- dist(phenology_network)
  
  mantel_result <- mantel(dist1, dist2, method = "pearson", permutations = 999)
  
  tibble(
    Botanical_garden = garden_name,
    Test = "VisitRate-Phenology",
    Mantel_corr = mantel_result$statistic,
    Mantel_Pval = mantel_result$signif
  )
}

# ======================================================
# Run
gardens <- unique(prob_matrices_by_garden$Botanical_garden)
results_visitRate <- map_dfr(gardens, mantel_visitRate_abundance)

# Optional: absolute correlation
results_visitRate <- results_visitRate %>%
  mutate(Mantel_corr = abs(Mantel_corr))

# Inspect results
results_visitRate

#Save data
saveRDS(results_visitRate, "Data/Working_files/Mantel_pheno_full_result.rds")

# ======================================================
# Plot
ggplot(results_visitRate, aes(x = Botanical_garden, y = Mantel_corr)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(
    x = "Botanical Garden",
    y = "Mantel Correlation",
    title = "VisitationRate-Phenology Mantel correlation"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylim(0, 1)


