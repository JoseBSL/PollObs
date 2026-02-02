# ======================================================
# Procrustes + PROTEST by SEASON: VisitRate–Trait
# + k-curve / plateau check
# Distance: Bray-Curtis (vegdist) with eps fix for empty rows
# ======================================================

library(dplyr)
library(purrr)
library(ggplot2)
library(vegan)

# Read seasonal TRAIT network data
trait_matrices_by_garden <- readRDS(
  "Data/Working_files/trait_probability_networks_only_phenobs_by_season.rds"
)

# Helpers
pcoa_scores <- function(d, k) cmdscale(d, k = k, eig = TRUE, add = TRUE)$points

make_bray_safe <- function(M, eps = 1e-10) {
  M <- as.matrix(M)
  M[!is.finite(M)] <- 0
  empty <- rowSums(M) == 0
  if (any(empty)) M[empty, ] <- M[empty, ] + eps
  M
}

# ---- Procrustes r only (fast) for k-curve ----
procrustes_r_visitRate_trait_season <- function(garden_name, season_name, k) {
  
  visit_rate_network <- trait_matrices_by_garden %>%
    filter(Botanical_garden == garden_name, Season == season_name) %>%
    pull(Int_frequency_network) %>%
    .[[1]]
  
  trait_network <- trait_matrices_by_garden %>%
    filter(Botanical_garden == garden_name, Season == season_name) %>%
    pull(Trait_prob) %>%
    .[[1]]
  
  if (!is.null(rownames(visit_rate_network)) && !is.null(rownames(trait_network))) {
    common <- intersect(rownames(visit_rate_network), rownames(trait_network))
    visit_rate_network <- visit_rate_network[common, , drop = FALSE]
    trait_network      <- trait_network[common, , drop = FALSE]
  }
  
  visit_rate_network <- make_bray_safe(visit_rate_network)
  trait_network      <- make_bray_safe(trait_network)
  
  n <- nrow(visit_rate_network)
  if (n < 3) {
    return(tibble(
      Botanical_garden = garden_name,
      Season = season_name,
      k = NA_integer_,
      Procrustes_r = NA_real_
    ))
  }
  
  d1 <- dist(visit_rate_network)
  d2 <- dist(trait_network)
  
  k_eff <- min(k, n - 1)
  
  X <- pcoa_scores(d1, k_eff)
  Y <- pcoa_scores(d2, k_eff)
  
  r <- sqrt(1 - procrustes(X, Y, symmetric = TRUE)$ss)
  
  tibble(
    Botanical_garden = garden_name,
    Season = season_name,
    k = k_eff,
    Procrustes_r = as.numeric(r)
  )
}

# ---- Full Procrustes + PROTEST at chosen k ----
protest_visitRate_trait_season <- function(garden_name, season_name, k, permutations = 999) {
  
  visit_rate_network <- trait_matrices_by_garden %>%
    filter(Botanical_garden == garden_name, Season == season_name) %>%
    pull(Int_frequency_network) %>%
    .[[1]]
  
  trait_network <- trait_matrices_by_garden %>%
    filter(Botanical_garden == garden_name, Season == season_name) %>%
    pull(Trait_prob) %>%
    .[[1]]
  
  if (!is.null(rownames(visit_rate_network)) && !is.null(rownames(trait_network))) {
    common <- intersect(rownames(visit_rate_network), rownames(trait_network))
    visit_rate_network <- visit_rate_network[common, , drop = FALSE]
    trait_network      <- trait_network[common, , drop = FALSE]
  }
  
  visit_rate_network <- make_bray_safe(visit_rate_network)
  trait_network      <- make_bray_safe(trait_network)
  
  n <- nrow(visit_rate_network)
  if (n < 3) {
    return(tibble(
      Botanical_garden = garden_name,
      Season = season_name,
      k = NA_integer_,
      Procrustes_m2 = NA_real_,
      Procrustes_r  = NA_real_,
      PROTEST_Pval  = NA_real_
    ))
  }
  
  d1 <- vegdist(visit_rate_network, method = "bray")
  d2 <- vegdist(trait_network, method = "bray")
  
  k_eff <- min(k, n - 1)
  
  X <- pcoa_scores(d1, k_eff)
  Y <- pcoa_scores(d2, k_eff)
  
  proc <- procrustes(X, Y, symmetric = TRUE)
  prot <- protest(X, Y, permutations = permutations, symmetric = TRUE)
  
  tibble(
    Botanical_garden = garden_name,
    Season = season_name,
    k = k_eff,
    Procrustes_m2 = as.numeric(proc$ss),
    Procrustes_r  = as.numeric(sqrt(1 - proc$ss)),
    PROTEST_Pval  = as.numeric(prot$signif)
  )
}

# Garden × season combos
garden_season_combos <- trait_matrices_by_garden %>%
  select(Botanical_garden, Season) %>%
  distinct()

# -------------------------
# Final run at chosen k
# -------------------------
k_final <- 15

results_protest_trait_season <- pmap_dfr(
  list(garden_season_combos$Botanical_garden, garden_season_combos$Season),
  ~ protest_visitRate_trait_season(..1, ..2, k = k_final, permutations = 999)
) %>%
  mutate(Season = factor(Season, levels = c("Early", "Mid", "Late")))

saveRDS(results_protest_trait_season, "Data/Working_files/PROTEST_trait_season_result.rds")

ggplot(results_protest_trait_season, aes(x = Season, y = Procrustes_r, fill = Season)) +
  geom_col(width = 0.75) +
  facet_wrap(~Botanical_garden, nrow = 1) +
  theme_minimal() +
  coord_cartesian(ylim = c(0, 1)) +
  labs(
    x = "Season",
    y = "Procrustes r",
    title = "VisitRate–Trait Procrustes (PROTEST) by garden and season"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")

# -------------------------
# k-curve / plateau check (fast, no permutations)
# -------------------------
k_grid <- 2:40

curve_df_trait <- pmap_dfr(
  list(garden_season_combos$Botanical_garden, garden_season_combos$Season),
  ~ map_dfr(k_grid, function(kk) {
    procrustes_r_visitRate_trait_season(..1, ..2, k = kk)
  })
) %>%
  mutate(Season = factor(Season, levels = c("Early", "Mid", "Late")))

# Curves per garden, colored by season
ggplot(curve_df_trait, aes(x = k, y = Procrustes_r, color = Season, group = Season)) +
  geom_line() +
  facet_wrap(~ Botanical_garden) +
  theme_minimal() +
  labs(
    x = "k (PCoA axes)",
    y = "Procrustes r",
    title = "k-curve check: Procrustes r vs k (VisitRate–Trait, by season)"
  ) +
  coord_cartesian(ylim = c(0, 1))
