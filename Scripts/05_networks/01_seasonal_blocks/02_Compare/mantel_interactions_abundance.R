# ======================================================
# Mantel test by SEASON (VISITATION RATE ONLY): VisitRate-Abundance
# ======================================================

library(dplyr)
library(purrr)
library(tidyr)
library(ggplot2)
library(vegan)

# Read seasonal network data
prob_matrices_by_garden <- readRDS("Data/Working_files/abundance_networks_only_phenobs_by_season.rds")

# Function: Mantel (VisitRate-Abundance) for one garden × season
mantel_visitRate_abundance <- function(garden_name, season_name) {
  
  visit_rate_network <- prob_matrices_by_garden %>%
    filter(Botanical_garden == garden_name, Season == season_name) %>%
    pull(Int_frequency_network) %>%
    .[[1]]
  
  abundance_network <- prob_matrices_by_garden %>%
    filter(Botanical_garden == garden_name, Season == season_name) %>%
    pull(Prob_matrix) %>%
    .[[1]]
  
  dist1 <- dist(visit_rate_network)
  dist2 <- dist(abundance_network)
  
  mantel_result <- mantel(dist1, dist2, method = "pearson", permutations = 999)
  
  tibble(
    Botanical_garden = garden_name,
    Season = season_name,
    Test = "VisitRate-Abundance",
    Mantel_corr = as.numeric(mantel_result$statistic),
    Mantel_Pval = as.numeric(mantel_result$signif)
  )
}

# Garden × season combinations
garden_season_combos <- prob_matrices_by_garden %>%
  select(Botanical_garden, Season) %>%
  distinct()

# Run Mantel for all combos
results_visitRate_abund <- pmap_dfr(
  list(
    garden_name = garden_season_combos$Botanical_garden,
    season_name = garden_season_combos$Season
  ),
  mantel_visitRate_abundance
)

# Optional: absolute correlation + ordered seasons
results_visitRate_abund <- results_visitRate_abund %>%
  mutate(
    Mantel_corr = abs(Mantel_corr),
    Season = factor(Season, levels = c("Early", "Mid", "Late"))
  )

# Save
saveRDS(results_visitRate_abund, "Data/Working_files/Mantel_abund_season_result.rds")

# Plot
ggplot(results_visitRate_abund, aes(x = Season, y = Mantel_corr, fill = Season)) +
  geom_col(width = 0.75) +
  facet_wrap(~Botanical_garden, nrow = 1) +
  theme_minimal() +
  coord_cartesian(ylim = c(0, 1)) +
  labs(
    x = "Season",
    y = "Mantel correlation",
    title = "VisitRate–Abundance Mantel correlation by garden and season"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")
