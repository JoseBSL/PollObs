# FIGURE 2 (TRAITS / SIZE MATCHING, points-only) — robust + ordered gardens within each Season row
# - x: Procrustes r
# - y: Season (Early / Mid / Late / Full season)
# - colour: Botanical garden (viridis plasma) + legend
# - temporal complexity legend (Weekly / Weekly aggregated / Full season) via SIZE
# - reduced jitter ONLY for weekly points
# - error bars (mean ± 95% CI) ONLY for weekly points

library(dplyr)
library(ggplot2)
library(lubridate)
library(viridis)
library(tibble)

# --- Load inputs ---
trait_week   <- readRDS("Data/Working_files/PROTEST_trait_week_result.rds") %>%
  filter(Test == "Int_frequency_network")
trait_season <- readRDS("Data/Working_files/PROTEST_trait_season_result.rds")
trait_full   <- readRDS("Data/Working_files/PROTEST_trait_full_result.rds")

groupped_dates <- readRDS("Data/Working_files/groupped_dates.rds") %>%
  mutate(Sampling_week = isoweek(Date))

# ---- Levels ----
season_levels <- c("Early","Mid","Late")
season_levels_full <- c(season_levels, "Full")
season_labels_full <- c("Early season","Mid season","Late season","Full season")

garden_levels <- c("Halle","Jena","Leipzig")
garden_offset <- c(Halle = -0.18, Jena = 0.00, Leipzig = +0.18)

# ---- Season per garden x week (dominant Season) ----
week_season <- groupped_dates %>%
  mutate(Season = factor(Season, levels = season_levels)) %>%
  count(Botanical_garden, Sampling_week, Season, name = "n") %>%
  group_by(Botanical_garden, Sampling_week) %>%
  slice_max(n, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(Botanical_garden, Sampling_week, Season)

# ---- Weekly points (TRAITS) ----
weekly_pts <- trait_week %>%
  select(-any_of("Season")) %>%
  left_join(week_season, by = c("Botanical_garden","Sampling_week")) %>%
  transmute(
    Procrustes_r = as.numeric(Procrustes_r),
    Season = factor(as.character(Season), levels = season_levels_full),
    Botanical_garden = factor(as.character(Botanical_garden), levels = garden_levels)
  ) %>%
  filter(is.finite(Procrustes_r), !is.na(Season), !is.na(Botanical_garden))

# ---- Weekly mean ± 95% CI (ONLY weekly) ----
weekly_sum <- weekly_pts %>%
  group_by(Season, Botanical_garden) %>%
  summarise(
    n = n(),
    mean_r = mean(Procrustes_r, na.rm = TRUE),
    se = sd(Procrustes_r, na.rm = TRUE) / sqrt(n),
    lwr = pmax(mean_r - 1.96 * se, 0),
    upr = pmin(mean_r + 1.96 * se, 1),
    .groups = "drop"
  )

# ---- Seasonal aggregated points (TRAITS) ----
seasonal_pts <- trait_season %>%
  transmute(
    Procrustes_r = as.numeric(Procrustes_r),
    Season = factor(as.character(Season), levels = season_levels_full),
    Botanical_garden = factor(as.character(Botanical_garden), levels = garden_levels)
  ) %>%
  filter(is.finite(Procrustes_r), !is.na(Season), !is.na(Botanical_garden))

# ---- Full-season points (TRAITS) ----
full_pts <- trait_full %>%
  transmute(
    Procrustes_r = as.numeric(Procrustes_r),
    Season = factor("Full", levels = season_levels_full),
    Botanical_garden = factor(as.character(Botanical_garden), levels = garden_levels)
  ) %>%
  filter(is.finite(Procrustes_r), !is.na(Botanical_garden))

# ---- helper: y positions with garden offsets; reduced jitter only for weekly ----
add_ypos <- function(df, jitter = 0) {
  df %>%
    mutate(
      Season = factor(Season, levels = season_levels_full),
      Botanical_garden = factor(Botanical_garden, levels = garden_levels),
      y_base = as.numeric(Season),
      y = y_base + unname(garden_offset[as.character(Botanical_garden)]) +
        if (jitter > 0) runif(n(), -jitter, +jitter) else 0
    )
}

set.seed(1)
wk_pts     <- add_ypos(weekly_pts, jitter = 0.035)
wk_mean_ci <- add_ypos(weekly_sum, jitter = 0)
wk_agg     <- add_ypos(seasonal_pts, jitter = 0)
wk_full    <- add_ypos(full_pts, jitter = 0)

# ---- separators ----
sep_df <- tibble(
  y = c(1.5, 2.5, 3.5),
  kind = c("light","light","strong")
)

panel_trait_swapped <- ggplot() +
  
  # separators (black solid)
  geom_hline(
    data = sep_df,
    aes(yintercept = y),
    linetype = "solid",
    colour = "gray82",
    inherit.aes = FALSE
  ) +
  scale_linewidth_manual(
    values = c(light = 0.5, strong = 1.0),
    guide = "none"
  ) +
  
  # WEEKLY cloud
#  geom_point(
#    data = wk_pts,
#    aes(
#      x = Procrustes_r, y = y,
#      colour = Botanical_garden,
#      size = "Weekly"
#    ),
#    shape = 16,
#    alpha = 0.4
#  ) +
  
  # WEEKLY mean ± 95% CI (ONLY weekly)
  geom_errorbarh(
    data = wk_mean_ci,
    aes(xmin = lwr, xmax = upr, y = y, colour = Botanical_garden),
    inherit.aes = FALSE,
    height = 0.045,
    linewidth = 1.1,
    alpha = 0.85
  ) +
  geom_point(
    data = wk_mean_ci,
    aes(
      x = mean_r, y = y,
      colour = Botanical_garden,
      size = "Weekly"
    ),
    inherit.aes = FALSE,
    shape = 16,
    alpha = 1
  ) +
  
  # WEEKLY aggregated
  geom_point(
    data = wk_agg,
    aes(
      x = Procrustes_r, y = y,
      colour = Botanical_garden,
      size = "Weekly aggregated"
    ),
    inherit.aes = FALSE,
    shape = 16,
    alpha = 0.9
  ) +
  
  # FULL season
  geom_point(
    data = wk_full,
    aes(
      x = Procrustes_r, y = y,
      colour = Botanical_garden,
      size = "Full season"
    ),
    inherit.aes = FALSE,
    shape = 16,
    alpha = 1
  ) +
  
  # axes
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 0.5, 1)) +
  scale_y_continuous(
    breaks = seq_along(season_levels_full),
    labels = season_labels_full,
    limits = c(0.5, length(season_levels_full) + 0.5)
  ) +
  
  # garden colours
  scale_colour_viridis_d(
    option = "plasma",
    begin = 0.08, end = 0.82,
    name = "Botanical garden",
    drop = FALSE
  ) +
  
  # Temporal complexity legend via SIZE (different-sized dots)
  scale_size_manual(
    name   = "Temporal aggregation",
    breaks = c("Weekly", "Weekly aggregated", "Full season"),
    values = c(
      "Weekly" = 2.4,
      "Weekly aggregated" = 4.7,
      "Full season" = 6.6
    ),
    drop = FALSE
  ) +
  
  guides(
    # Temporal legend first
    size = guide_legend(
      order = 1,
      override.aes = list(
        colour = "grey30",
        alpha  = 1,
        shape  = 16
      )
    ),
    # Garden legend second
    colour = guide_legend(
      order = 2,
      override.aes = list(shape = 16, alpha = 1, size = 5.0)
    )
  ) +
  
  theme_minimal() +
  coord_cartesian(clip = "off") +
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.6),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks.x = element_line(color = "black", linewidth = 0.5),
    axis.ticks.length = unit(0.2, "cm"),
    legend.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold", size = 14, vjust = 1),
    axis.title = element_text(face = "bold"),
    plot.margin = margin(t = 20, r = 20, b = 20, l = 20)
  ) +
  labs(
    x = "Procrustes r",
    y = NULL,
    title = "b) Trait-matching"
  )

panel_trait_swapped

saveRDS(panel_trait_swapped, "Data/Working_files/panel_trait_swapped.rds")
