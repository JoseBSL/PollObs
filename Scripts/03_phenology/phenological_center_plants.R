library(dplyr)
library(purrr)
library(ggplot2)
library(patchwork)

# Read data
jena_phen    <- readRDS("Data/Phenology_data/clean_plant_phenobs_jena.rds")
halle_phen   <- readRDS("Data/Phenology_data/clean_plant_phenobs_halle.rds")
leipzig_phen <- readRDS("Data/Phenology_data/clean_plant_phenobs_leipzig.rds")

# Combine gardens
plant_data <- bind_rows(
  jena_phen %>% mutate(Garden = "Jena"),
  halle_phen %>% mutate(Garden = "Halle"),
  leipzig_phen %>% mutate(Garden = "Leipzig")
)

# Bootstrap one species center
boot_species_center <- function(df, n_boot = 1000) {
  
  df <- df %>%
    mutate(
      Flowering_intensity = if_else(
        Flowers_opening == "no",
        0,
        as.numeric(Flowering_intensity)
      )
    ) %>%
    filter(!is.na(Doy), Flowering_intensity > 0)
  
  if (nrow(df) < 3) {
    return(tibble(
      center = NA_real_,
      ci_low = NA_real_,
      ci_high = NA_real_,
      n_obs = nrow(df),
      boot_vals = list(NA_real_)
    ))
  }
  
  center_obs <- weighted.mean(df$Doy, df$Flowering_intensity, na.rm = TRUE)
  
  boot_vals <- replicate(n_boot, {
    idx <- sample(seq_len(nrow(df)), size = nrow(df), replace = TRUE)
    d_boot <- df[idx, , drop = FALSE]
    weighted.mean(d_boot$Doy, d_boot$Flowering_intensity, na.rm = TRUE)
  })
  
  tibble(
    center = center_obs,
    ci_low = as.numeric(quantile(boot_vals, 0.025, na.rm = TRUE)),
    ci_high = as.numeric(quantile(boot_vals, 0.975, na.rm = TRUE)),
    n_obs = nrow(df),
    boot_vals = list(boot_vals)
  )
}

# Species-level centers
species_centers <- plant_data %>%
  group_by(Garden, Species) %>%
  group_modify(~ boot_species_center(.x, n_boot = 1000)) %>%
  ungroup() %>%
  filter(!is.na(center), n_obs >= 3)

# Synchronization by garden
bootstrap_sync <- function(df, n_boot = 1000) {
  
  duration <- quantile(df$center, 0.95) - quantile(df$center, 0.05)
  
  if (nrow(df) < 2 || duration <= 0) {
    return(tibble(
      sync_mean = NA_real_,
      sync_low = NA_real_,
      sync_high = NA_real_,
      n_species = nrow(df),
      community_center = NA_real_
    ))
  }
  
  sync_vals <- replicate(n_boot, {
    sampled_centers <- map_dbl(df$boot_vals, ~ sample(.x, size = 1))
    dispersion <- sd(sampled_centers, na.rm = TRUE)
    1 - dispersion / duration
  })
  
  tibble(
    sync_mean = mean(sync_vals, na.rm = TRUE),
    sync_low  = as.numeric(quantile(sync_vals, 0.025, na.rm = TRUE)),
    sync_high = as.numeric(quantile(sync_vals, 0.975, na.rm = TRUE)),
    n_species = nrow(df),
    community_center = mean(df$center, na.rm = TRUE)
  )
}

sync_summary <- species_centers %>%
  group_by(Garden) %>%
  group_modify(~ bootstrap_sync(.x, n_boot = 1000)) %>%
  ungroup()

sync_summary


plot_species_centers <- function(species_centers, sync_summary, garden_name) {
  
  d <- species_centers %>%
    filter(Garden == garden_name) %>%
    arrange(center) %>%
    mutate(Species = factor(Species, levels = rev(Species)))
  
  center_line <- sync_summary %>%
    filter(Garden == garden_name) %>%
    pull(community_center)
  
  sync_label <- sync_summary %>%
    filter(Garden == garden_name) %>%
    transmute(
      label = paste0(
        "Sync = ",
        round(sync_mean, 2),
        " [",
        round(sync_low, 2), ", ",
        round(sync_high, 2), "]"
      )
    ) %>%
    pull(label)
  
  ggplot(d, aes(x = center, y = Species)) +
    geom_segment(
      aes(x = ci_low, xend = ci_high, yend = Species),
      linewidth = 0.5,
      color = "grey50"
    ) +
    geom_point(size = 2) +
    geom_vline(xintercept = center_line, linetype = "dashed") +
    annotate(
      "text",
      x = Inf, y = Inf,
      label = sync_label,
      hjust = 1.1, vjust = 1.5,
      size = 3.2
    ) +
    theme_minimal() +
    labs(
      x = "Phenological center (DOY)",
      y = NULL,
      title = garden_name
    ) +
    theme(
      axis.text.y = element_text(face = "italic", size = 7),
      panel.grid.major.y = element_blank()
    )
}

p_jena <- plot_species_centers(species_centers, sync_summary, "Jena")
p_halle <- plot_species_centers(species_centers, sync_summary, "Halle")
p_leipzig <- plot_species_centers(species_centers, sync_summary, "Leipzig")

p_jena + p_halle + p_leipzig