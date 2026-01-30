# ======================================================
# Mantel test: VisitRate – Trait probability
# ======================================================

library(dplyr)
library(purrr)
library(ggplot2)
library(vegan)

# -----------------------------
# Load data
# -----------------------------
trait_matrices_by_garden <- readRDS(
  "Data/Working_files/trait_probability_networks_only_phenobs.rds"
)

# -----------------------------
# Function: Mantel VisitRate–Trait
# -----------------------------
mantel_visitRate_trait <- function(garden_name) {
  
  visit_rate_network <- trait_matrices_by_garden %>%
    filter(Botanical_garden == garden_name) %>%
    pull(Int_frequency_network) %>%
    .[[1]]
  
  trait_network <- trait_matrices_by_garden %>%
    filter(Botanical_garden == garden_name) %>%
    pull(Trait_prob) %>%
    .[[1]]
  
  dist1 <- dist(visit_rate_network)
  dist2 <- dist(trait_network)
  
  mantel_result <- mantel(dist1, dist2, method = "pearson", permutations = 999)
  
  tibble(
    Botanical_garden = garden_name,
    Test = "VisitRate–Trait",
    Mantel_corr = as.numeric(mantel_result$statistic),
    Mantel_Pval = as.numeric(mantel_result$signif)
  )
}

# -----------------------------
# Run for all gardens
# -----------------------------
gardens <- unique(trait_matrices_by_garden$Botanical_garden)

results_visitRate_trait <- map_dfr(gardens, mantel_visitRate_trait)

# -----------------------------
# Save results
# -----------------------------
saveRDS(
  results_visitRate_trait,
  "Data/Working_files/Mantel_trait_full_result.rds"
)

# -----------------------------
# Plot
# -----------------------------
ggplot(results_visitRate_trait,
       aes(x = Botanical_garden, y = Mantel_corr)) +
  geom_col(fill = "darkorange") +
  theme_minimal() +
  labs(
    x = "Botanical Garden",
    y = "Mantel correlation",
    title = "VisitRate–Trait Mantel test"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  coord_cartesian(ylim = c(0, 1))
