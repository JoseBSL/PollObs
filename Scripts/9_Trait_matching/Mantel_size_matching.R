# ======================================================
# Mantel test (VISITATION RATE ONLY): VisitRate–TRAIT (size matching)
# Uses Flower width ↔ IT_mm Gaussian trait-matching matrix
# ======================================================

library(dplyr)
library(purrr)
library(tidyr)
library(ggplot2)
library(vegan)

# ------------------------------------------------------
# Load data: interaction/visit-rate networks + trait matrices
# ------------------------------------------------------
trait_matrices_by_garden <- readRDS("Data/Working_files/trait_networks_only_phenobs.rds")
# expects columns:
# - Botanical_garden
# - Int_frequency_network
# - Prob_matrix_traits  (your Gaussian trait-matching matrix)

# ------------------------------------------------------
# Function: Mantel (VisitRate–Trait)
# ------------------------------------------------------
mantel_visitRate_trait <- function(garden_name,
                                   trait_col = "Prob_matrix_traits") {
  
  df_g <- trait_matrices_by_garden %>%
    filter(Botanical_garden == garden_name)
  
  visit_rate_network <- df_g %>%
    pull(Int_frequency_network) %>%
    .[[1]]
  
  trait_network <- df_g %>%
    pull(!!sym(trait_col)) %>%
    .[[1]]
  
  # --- Safety checks: same dims and names ---
  if (is.null(visit_rate_network) || is.null(trait_network)) {
    return(tibble(
      Botanical_garden = garden_name,
      Test = "VisitRate-Trait",
      Mantel_corr = NA_real_,
      Mantel_Pval = NA_real_
    ))
  }
  
  # align (in case of minor ordering differences)
  common_rows <- intersect(rownames(visit_rate_network), rownames(trait_network))
  common_cols <- intersect(colnames(visit_rate_network), colnames(trait_network))
  
  visit_rate_network <- visit_rate_network[common_rows, common_cols, drop = FALSE]
  trait_network      <- trait_network[common_rows, common_cols, drop = FALSE]
  
  # if too small after intersect, skip
  if (nrow(visit_rate_network) < 2 || ncol(visit_rate_network) < 2) {
    return(tibble(
      Botanical_garden = garden_name,
      Test = "VisitRate-Trait",
      Mantel_corr = NA_real_,
      Mantel_Pval = NA_real_
    ))
  }
  
  # Mantel
  dist1 <- dist(visit_rate_network)
  dist2 <- dist(trait_network)
  
  mantel_result <- mantel(dist1, dist2, method = "pearson", permutations = 999)
  
  tibble(
    Botanical_garden = garden_name,
    Test = "VisitRate-Trait (Flower width ↔ IT_mm)",
    Mantel_corr = as.numeric(mantel_result$statistic),
    Mantel_Pval = as.numeric(mantel_result$signif)
  )
}

# ------------------------------------------------------
# Run for all gardens
# ------------------------------------------------------
gardens <- unique(trait_matrices_by_garden$Botanical_garden)
results_visitRate_trait <- map_dfr(gardens, mantel_visitRate_trait)

# Save results
#saveRDS(results_visitRate_trait, "Data/Working_files/Mantel_trait_full_result.rds")

# ------------------------------------------------------
# Plot
# ------------------------------------------------------
ggplot(results_visitRate_trait, aes(x = Botanical_garden, y = Mantel_corr)) +
  geom_col(fill = "steelblue") +
  theme_minimal() +
  labs(
    x = "Botanical Garden",
    y = "Mantel correlation",
    title = "VisitRate–Trait Mantel test (Flower width ↔ IT_mm)"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  coord_cartesian(ylim = c(0, 1))
