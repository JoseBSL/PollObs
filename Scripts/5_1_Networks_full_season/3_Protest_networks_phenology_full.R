# ======================================================
# Procrustes + PROTEST: VISITATION RATE network ~ phenology matrix
# (with k-curve / plateau check)
# ======================================================

library(dplyr)
library(purrr)
library(tibble)
library(ggplot2)
library(vegan)

# ======================================================
# Load data
# Phenology matrices
prob_matrices_by_garden <- readRDS("Data/Working_files/phenology_networks_only_phenobs_pheno.rds")
# Visitation rate networks (Int_frequency_network)
net_by_garden <- readRDS("Data/Working_files/networks_by_garden_only_phenobs_pheno.rds")

# ======================================================
# PCoA scores (for Procrustes/PROTEST)
pcoa_scores <- function(d, k) cmdscale(d, k = k, eig = TRUE, add = TRUE)$points

# ======================================================
# Function: Procrustes + PROTEST visitation RATE network ~ phenology
procrustes_visitRate_phenology <- function(garden_name, k, permutations = 999) {
  
  interaction_network <- net_by_garden %>%
    filter(Botanical_garden == garden_name) %>%
    pull(Int_frequency_network) %>%
    .[[1]]
  
  phenology_matrix <- prob_matrices_by_garden %>%
    filter(Botanical_garden == garden_name) %>%
    pull(Prob_matrix) %>%
    .[[1]]
  
  # Distances (use Euclidean to mirror your Mantel template; change to "bray" if appropriate)
  d1 <- dist(interaction_network)
  d2 <- dist(phenology_matrix)
  
  # Safety: k cannot exceed n - 1
  k_eff <- min(k, nrow(interaction_network) - 1)
  
  X <- pcoa_scores(d1, k_eff)
  Y <- pcoa_scores(d2, k_eff)
  
  proc <- procrustes(X, Y, symmetric = TRUE)
  prot <- protest(X, Y, permutations = permutations, symmetric = TRUE)
  
  m2 <- proc$ss
  r  <- sqrt(1 - m2)
  
  tibble(
    Botanical_garden = garden_name,
    Test = "VisitRate-Phenology",
    k = k_eff,
    Procrustes_m2 = as.numeric(m2),
    Procrustes_r  = as.numeric(r),
    PROTEST_Pval  = as.numeric(prot$signif),
    procrustes_obj = list(proc),
    protest_obj    = list(prot)
  )
}

# ======================================================
# Run (choose final k)
# Set this to your plateau value (e.g., 10 or 11)
k_final <- 10

gardens <- unique(prob_matrices_by_garden$Botanical_garden)
results_proc_pheno <- map_dfr(gardens, ~ procrustes_visitRate_phenology(.x, k = k_final, permutations = 999))

# Save full results (with stored objects)
saveRDS(results_proc_pheno, "Data/Working_files/PROTEST_pheno_full_result.rds")

# Save light summary
results_proc_pheno_table <- results_proc_pheno %>%
  select(Botanical_garden, Test, k, Procrustes_m2, Procrustes_r, PROTEST_Pval)

saveRDS(results_proc_pheno_table, "Data/Working_files/PROTEST_pheno_summary.rds")

# Plot final Procrustes r per garden
ggplot(results_proc_pheno_table, aes(x = Botanical_garden, y = Procrustes_r)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  theme_minimal() +
  labs(
    x = "Botanical Garden",
    y = "Procrustes r",
    title = "VisitationRate–Phenology Procrustes (PROTEST)"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  coord_cartesian(ylim = c(0, 1))

# ======================================================
# Curve / plateau check: Procrustes r vs k (per garden)
# ======================================================
k_grid <- 2:40
scan_perms <- 99

curve_df_pheno <- map_dfr(gardens, function(g) {
  map_dfr(k_grid, function(kk) {
    procrustes_visitRate_phenology(g, k = kk, permutations = scan_perms) %>%
      select(Botanical_garden, k, Procrustes_r)
  })
})

ggplot(curve_df_pheno, aes(x = k, y = Procrustes_r)) +
  geom_line() +
  facet_wrap(~ Botanical_garden) +
  theme_minimal() +
  labs(
    x = "k (number of PCoA axes)",
    y = "Procrustes r",
    title = "Plateau check per garden: Procrustes r vs k (VisitRate–Phenology)"
  ) +
  coord_cartesian(ylim = c(0, 1))
 