# ======================================================
# Mantel test by SEASON (VISITATION RATE ONLY): VisitRate–Trait
# ======================================================

library(dplyr)
library(purrr)
library(tidyr)
library(ggplot2)
library(vegan)

# Read seasonal TRAIT network data (created earlier)
trait_matrices_by_garden <- readRDS(
  "Data/Working_files/trait_probability_networks_only_phenobs_by_season.rds"
)

# Function: Mantel (VisitRate–Trait) for one garden × season
mantel_visitRate_trait <- function(garden_name, season_name) {
  
  visit_rate_network <- trait_matrices_by_garden %>%
    filter(Botanical_garden == garden_name, Season == season_name) %>%
    pull(Int_frequency_network) %>%
    .[[1]]
  
  trait_network <- trait_matrices_by_garden %>%
    filter(Botanical_garden == garden_name, Season == season_name) %>%
    pull(Trait_prob) %>%
    .[[1]]
  
  dist1 <- dist(visit_rate_network)
  dist2 <- dist(trait_network)
  
  mantel_result <- mantel(dist1, dist2, method = "pearson", permutations = 999)
  
  tibble(
    Botanical_garden = garden_name,
    Season = season_name,
    Test = "VisitRate-Trait",
    Mantel_corr = as.numeric(mantel_result$statistic),
    Mantel_Pval = as.numeric(mantel_result$signif)
  )
}

# Garden × season combinations
garden_season_combos <- trait_matrices_by_garden %>%
  select(Botanical_garden, Season) %>%
  distinct()

# Run Mantel for all combos
results_visitRate_trait <- pmap_dfr(
  list(
    garden_name = garden_season_combos$Botanical_garden,
    season_name = garden_season_combos$Season
  ),
  mantel_visitRate_trait
)

# Optional: absolute correlation + ordered seasons
results_visitRate_trait <- results_visitRate_trait %>%
  mutate(
    Mantel_corr = abs(Mantel_corr),
    Season = factor(Season, levels = c("Early", "Mid", "Late"))
  )

# Save
saveRDS(results_visitRate_trait, "Data/Working_files/Mantel_trait_season_result.rds")

# Plot
ggplot(results_visitRate_trait, aes(x = Season, y = Mantel_corr, fill = Season)) +
  geom_col(width = 0.75) +
  facet_wrap(~Botanical_garden, nrow = 1) +
  theme_minimal() +
  coord_cartesian(ylim = c(0, 1)) +
  labs(
    x = "Season",
    y = "Mantel correlation",
    title = "VisitRate–Trait Mantel correlation by garden and season"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")
