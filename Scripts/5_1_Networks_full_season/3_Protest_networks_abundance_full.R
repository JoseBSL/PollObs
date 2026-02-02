library(dplyr)
library(purrr)
library(ggplot2)
library(vegan)

# Load data
prob_matrices_by_garden <- readRDS("Data/Working_files/abundance_networks_only_phenobs.rds")

# PCoA scores (for Procrustes/PROTEST)
pcoa_scores <- function(d, k) cmdscale(d, k = k, eig = TRUE, add = TRUE)$points

# Procrustes + PROTEST for one garden
procrustes_visitRate_abundance <- function(garden_name, k, permutations = 999) {
  
  visit_rate_network <- prob_matrices_by_garden %>%
    filter(Botanical_garden == garden_name) %>%
    pull(Int_frequency_network) %>%
    .[[1]]
  
  abundance_network <- prob_matrices_by_garden %>%
    filter(Botanical_garden == garden_name) %>%
    pull(Prob_matrix) %>%
    .[[1]]
  
  d1 <- dist(visit_rate_network)
  d2 <- dist(abundance_network)
  
  k_eff <- min(k, nrow(visit_rate_network) - 1)
  
  X <- pcoa_scores(d1, k_eff)
  Y <- pcoa_scores(d2, k_eff)
  
  proc <- procrustes(X, Y, symmetric = TRUE)
  prot <- protest(X, Y, permutations = permutations, symmetric = TRUE)
  
  m2 <- proc$ss
  r  <- sqrt(1 - m2)
  
  tibble(
    Botanical_garden = garden_name,
    k = k_eff,
    Procrustes_m2 = as.numeric(m2),
    Procrustes_r  = as.numeric(r),
    PROTEST_Pval  = as.numeric(prot$signif),
    procrustes_obj = list(proc),
    protest_obj    = list(prot)
  )
}

# Gardens
gardens <- unique(prob_matrices_by_garden$Botanical_garden)

# -------------------------
# Final run at chosen k
# -------------------------
k_final <- 15

results_proc <- map_dfr(gardens, ~ procrustes_visitRate_abundance(.x, k = k_final, permutations = 999))

saveRDS(results_proc, "Data/Working_files/PROTEST_abund_full_result.rds")

results_proc_table <- results_proc %>%
  select(Botanical_garden, k, Procrustes_m2, Procrustes_r, PROTEST_Pval)

saveRDS(results_proc_table, "Data/Working_files/PPROTEST_abund_summary.rds")

ggplot(results_proc_table, aes(x = Botanical_garden, y = Procrustes_r)) +
  geom_col(fill = "steelblue") +
  theme_minimal() +
  labs(
    x = "Botanical Garden",
    y = "Procrustes r",
    title = "VisitRate–Abundance Procrustes (PROTEST)"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  coord_cartesian(ylim = c(0, 1))

# -------------------------
# Curve / plateau check
# -------------------------
k_grid <- 2:40
scan_perms <- 99

curve_df <- map_dfr(gardens, function(g) {
  map_dfr(k_grid, function(kk) {
    procrustes_visitRate_abundance(g, k = kk, permutations = scan_perms) %>%
      select(Botanical_garden, k, Procrustes_r)
  })
})

ggplot(curve_df, aes(x = k, y = Procrustes_r)) +
  geom_line() +
  facet_wrap(~ Botanical_garden) +
  theme_minimal() +
  labs(
    x = "k (number of PCoA axes)",
    y = "Procrustes r",
    title = "Plateau check per garden: Procrustes r vs k"
  ) +
  coord_cartesian(ylim = c(0, 1))
