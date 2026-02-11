# ======================================================
# Compute Mantel test: VISITATION RATE network ~ phenology matrix (by garden & season)
# ======================================================

# Load libraries
library(dplyr)
library(purrr)
library(tibble)
library(ggplot2)
library(vegan) # mantel

# ======================================================
# Load data
prob_matrices_by_garden <- readRDS("Data/Working_files/phenology_networks_only_phenobs_pheno_by_season.rds")
net_by_garden <- readRDS("Data/Working_files/networks_by_garden_and_season_only_phenobs_pheno.rds")

# ======================================================
# Function: Mantel visitation RATE network ~ phenology
mantel_visitRate_abundance <- function(garden_name, season_name) {
  
  interaction_network <- net_by_garden %>%
    filter(Botanical_garden == garden_name, Season == season_name) %>%
    pull(Int_frequency_network) %>%
    .[[1]]
  
  phenology_network <- prob_matrices_by_garden %>%
    filter(Botanical_garden == garden_name, Season == season_name) %>%
    pull(Prob_matrix) %>%
    .[[1]]
  
  dist1 <- dist(interaction_network)
  dist2 <- dist(phenology_network)
  
  mantel_result <- mantel(dist1, dist2, method = "pearson", permutations = 999)
  
  tibble(
    Botanical_garden = garden_name,
    Season = season_name,
    Test = "VisitRate-Phenology",
    Mantel_corr = mantel_result$statistic,
    Mantel_Pval = mantel_result$signif
  )
}

# ======================================================
# Run across garden-season combos
garden_season_combos <- prob_matrices_by_garden %>%
  select(Botanical_garden, Season) %>%
  distinct()

results_visitRate <- pmap_dfr(
  list(
    garden_name = garden_season_combos$Botanical_garden,
    season_name = garden_season_combos$Season
  ),
  mantel_visitRate_abundance
)

# Absolute correlation + season order
results_visitRate <- results_visitRate %>%
  mutate(
    Mantel_corr = abs(Mantel_corr),
    Season = factor(Season, levels = c("Early", "Mid", "Late"))
  )

# Inspect results
results_visitRate
saveRDS(results_visitRate, "Data/Working_files/Mantel_pheno_season_result.rds")

# ======================================================
# Plot
ggplot(results_visitRate, aes(x = Season, y = Mantel_corr)) +
  geom_bar(stat = "identity") +
  facet_wrap(~Botanical_garden, nrow = 1) +
  theme_minimal() +
  labs(
    x = "Season",
    y = "Mantel Correlation",
    title = "Mantel correlation by Garden and Season (Visitation Rate vs Phenology)"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylim(0, 1)
