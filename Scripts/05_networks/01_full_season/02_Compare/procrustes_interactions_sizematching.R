library(dplyr)
library(purrr)
library(ggplot2)
library(vegan)

# -----------------------------
# Load data
# -----------------------------
trait_matrices_by_garden <- readRDS("Data/Working_files/trait_probability_networks_only_phenobs.rds")

# -----------------------------
# Helpers
# -----------------------------
pcoa_scores <- function(d, k) cmdscale(d, k = k, eig = TRUE, add = TRUE)$points

# Keep all rows, but avoid NA Bray distances from all-zero rows (0/0)
make_bray_safe <- function(M, eps = 1e-10) {
  M <- as.matrix(M)
  M[!is.finite(M)] <- 0
  empty <- rowSums(M) == 0
  if (any(empty)) M[empty, ] <- M[empty, ] + eps
  M
}

# -----------------------------
# Procrustes + PROTEST for one garden: VisitRate ~ Trait_prob (Bray)
# -----------------------------
procrustes_visitRate_trait <- function(garden_name, k, permutations = 999) {
  
  visit_rate_network <- trait_matrices_by_garden %>%
    filter(Botanical_garden == garden_name) %>%
    pull(Int_frequency_network) %>%
    .[[1]]
  
  trait_network <- trait_matrices_by_garden %>%
    filter(Botanical_garden == garden_name) %>%
    pull(Trait_prob) %>%
    .[[1]]
  
  # Align by rownames if present
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
      k = NA_integer_,
      Procrustes_m2 = NA_real_,
      Procrustes_r  = NA_real_,
      PROTEST_Pval  = NA_real_
    ))
  }
  
  d1 <- dist(visit_rate_network)
  d2 <- dist(trait_network)
  
  k_eff <- min(k, n - 1)
  
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
    PROTEST_Pval  = as.numeric(prot$signif)
  )
}

# -----------------------------
# Run (final k)
# -----------------------------
gardens <- unique(trait_matrices_by_garden$Botanical_garden)
k_final <- 20

results_proc <- map_dfr(gardens, ~ procrustes_visitRate_trait(.x, k = k_final, permutations = 999))

saveRDS(results_proc, "Data/Working_files/PROTEST_trait_full_result.rds")

ggplot(results_proc, aes(x = Botanical_garden, y = Procrustes_r)) +
  geom_col(fill = "darkorange") +
  theme_minimal() +
  labs(
    x = "Botanical Garden",
    y = "Procrustes r",
    title = "VisitRate–Trait Procrustes (PROTEST) — Bray-Curtis"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  coord_cartesian(ylim = c(0, 1))

# -----------------------------
# Plateau check (fast scan)
# -----------------------------
k_grid <- 2:40
scan_perms <- 99

curve_df <- map_dfr(gardens, function(g) {
  map_dfr(k_grid, function(kk) {
    procrustes_visitRate_trait(g, k = kk, permutations = scan_perms) %>%
      select(Botanical_garden, k, Procrustes_r)
  })
})

ggplot(curve_df, aes(x = k, y = Procrustes_r)) +
  geom_line() +
  facet_wrap(~ Botanical_garden) +
  theme_minimal() +
  labs(
    x = "k (PCoA axes)",
    y = "Procrustes r",
    title = "Plateau check per garden: Procrustes r vs k (VisitRate–Trait, Bray-Curtis)"
  ) +
  coord_cartesian(ylim = c(0, 1))
