library(dplyr)
library(ggplot2)
library(tibble)
library(tidyr)
library(lubridate)
library(viridisLite)

# ---- Load inputs ----
abund_week <- readRDS("Data/Working_files/PROTEST_abund_week_result.rds") %>%
  filter(Test == "Int_frequency_network")

abund_season <- readRDS("Data/Working_files/PROTEST_abund_season_result.rds")

abund_full <- readRDS("Data/Working_files/PROTEST_abund_full_result.rds")

groupped_dates <- readRDS("Data/Working_files/groupped_dates.rds") %>%
  mutate(Sampling_week = isoweek(Date))

# ---- Levels ----
season_levels <- c("Early", "Mid", "Late")
garden_levels <- c("Halle", "Jena", "Leipzig")

# ---- Garden colours ----
garden_cols <- c(
  "Halle"   = "#0D0887FF",
  "Jena"    = "#B12A90FF",
  "Leipzig" = "#FCA636FF"
)

# ---- Season per garden x week: dominant Season ----
week_season <- groupped_dates %>%
  mutate(
    Botanical_garden = factor(Botanical_garden, levels = garden_levels),
    Season = factor(Season, levels = season_levels),
    Sampling_week = as.numeric(Sampling_week)
  ) %>%
  count(Botanical_garden, Sampling_week, Season, name = "n") %>%
  group_by(Botanical_garden, Sampling_week) %>%
  slice_max(n, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(Botanical_garden, Sampling_week, Season) %>%
  filter(!is.na(Botanical_garden), !is.na(Season))

# ---- Weekly values ----
weekly_bars <- abund_week %>%
  select(-any_of("Season")) %>%
  mutate(
    Botanical_garden = factor(Botanical_garden, levels = garden_levels),
    Sampling_week = as.numeric(Sampling_week),
    Procrustes_r = as.numeric(Procrustes_r)
  ) %>%
  left_join(week_season, by = c("Botanical_garden", "Sampling_week")) %>%
  filter(
    !is.na(Botanical_garden),
    is.finite(Sampling_week),
    is.finite(Procrustes_r),
    !is.na(Season)
  )

# ---- All week ranges per garden ----
all_weeks <- week_season %>%
  group_by(Botanical_garden) %>%
  summarise(
    week_min = min(Sampling_week, na.rm = TRUE),
    week_max = max(Sampling_week, na.rm = TRUE),
    .groups = "drop"
  )

# ---- Weekly values with missing weeks retained ----
weekly_bars_full <- week_season %>%
  distinct(Botanical_garden, Sampling_week, Season) %>%
  group_by(Botanical_garden) %>%
  complete(
    Sampling_week = seq(min(Sampling_week), max(Sampling_week), by = 1)
  ) %>%
  ungroup() %>%
  left_join(
    week_season,
    by = c("Botanical_garden", "Sampling_week"),
    suffix = c("", ".season")
  ) %>%
  mutate(
    Season = coalesce(Season, Season.season)
  ) %>%
  select(-Season.season) %>%
  left_join(
    weekly_bars %>%
      select(Botanical_garden, Sampling_week, Procrustes_r),
    by = c("Botanical_garden", "Sampling_week")
  ) %>%
  mutate(
    Botanical_garden = factor(Botanical_garden, levels = garden_levels),
    has_value = is.finite(Procrustes_r),
    Procrustes_r_plot = if_else(has_value, Procrustes_r, 0)
  )

# ---- Week ranges per garden x season ----
season_week_ranges <- week_season %>%
  group_by(Botanical_garden, Season) %>%
  summarise(
    week_min = min(Sampling_week, na.rm = TRUE),
    week_max = max(Sampling_week, na.rm = TRUE),
    .groups = "drop"
  )

# ---- Seasonal values ----
season_bars <- abund_season %>%
  mutate(
    Botanical_garden = factor(Botanical_garden, levels = garden_levels),
    Season = factor(as.character(Season), levels = season_levels),
    Procrustes_r = as.numeric(Procrustes_r)
  ) %>%
  left_join(season_week_ranges, by = c("Botanical_garden", "Season")) %>%
  filter(
    !is.na(Botanical_garden),
    !is.na(Season),
    is.finite(Procrustes_r),
    is.finite(week_min),
    is.finite(week_max)
  )

# ---- Full-season values ----
full_bar <- abund_full %>%
  mutate(
    Botanical_garden = factor(Botanical_garden, levels = garden_levels),
    Procrustes_r = as.numeric(Procrustes_r)
  ) %>%
  left_join(all_weeks, by = "Botanical_garden") %>%
  filter(
    !is.na(Botanical_garden),
    is.finite(Procrustes_r),
    is.finite(week_min),
    is.finite(week_max)
  )

# ---- Plot ----
panel_abundance_hist <- ggplot() +
  
  # weekly bars, including missing weeks as faint bars
  geom_col(
    data = weekly_bars_full,
    aes(
      x = Sampling_week,
      y = Procrustes_r_plot,
      fill = Botanical_garden,
      alpha = has_value
    ),
    width = 0.52,
    colour = NA
  ) +
  
  # seasonal values
  geom_segment(
    data = season_bars,
    aes(
      x = week_min - 0.45,
      xend = week_max + 0.45,
      y = Procrustes_r,
      yend = Procrustes_r,
      colour = Botanical_garden
    ),
    linewidth = 0.8,
    lineend = "round"
  ) +
  
  # full-season value
  geom_segment(
    data = full_bar,
    aes(
      x = week_min - 0.50,
      xend = week_max + 0.50,
      y = Procrustes_r,
      yend = Procrustes_r,
      colour = Botanical_garden
    ),
    linewidth = 0.85,
    linetype = "22",
    lineend = "round"
  ) +
  
  # seasonal labels
  geom_text(
    data = season_bars,
    aes(
      x = (week_min + week_max) / 2,
      y = Procrustes_r,
      label = Season
    ),
    vjust = -0.45,
    size = 3.1,
    fontface = "plain",
    colour = "black"
  ) +
  
  # full labels
  geom_text(
    data = full_bar,
    aes(
      x = week_max + 0.25,
      y = Procrustes_r,
      label = "Full"
    ),
    hjust = 0,
    vjust = -0.2,
    size = 3.1,
    fontface = "plain",
    colour = "black"
  ) +
  
  facet_wrap(
    ~Botanical_garden,
    ncol = 1,
    strip.position = "top"
  ) +
  
  scale_fill_manual(
    values = garden_cols,
    guide = "none"
  ) +
  
  scale_colour_manual(
    values = garden_cols,
    guide = "none"
  ) +
  
  scale_alpha_manual(
    values = c(`TRUE` = 0.48, `FALSE` = 0.08),
    guide = "none"
  ) +
  
  scale_x_continuous(
    breaks = seq(
      floor(min(weekly_bars_full$Sampling_week, na.rm = TRUE)),
      ceiling(max(weekly_bars_full$Sampling_week, na.rm = TRUE)),
      by = 1
    ),
    limits = c(
      min(weekly_bars_full$Sampling_week, na.rm = TRUE) - 0.6,
      max(weekly_bars_full$Sampling_week, na.rm = TRUE) + 1.2
    ),
    expand = c(0, 0)
  ) +
  
  scale_y_continuous(
    limits = c(0, 1.05),
    breaks = c(0, 0.5, 1),
    expand = c(0, 0)
  ) +
  
  labs(
    x = "Sampling week",
    y = "Procrustes r",
    title = "Abundance"
  ) +
  
  theme_classic(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 12, hjust = 0),
    
    axis.title = element_text(size = 11, face = "bold"),
    axis.text = element_text(size = 9, colour = "black"),
    
    strip.background = element_blank(),
    strip.text = element_text(face = "bold", size = 11, colour = "black"),
    
    panel.grid = element_blank(),
    axis.line = element_blank(),
    
    panel.border = element_rect(
      colour = "black",
      fill = NA,
      linewidth = 0.65
    ),
    
    axis.ticks = element_line(colour = "black", linewidth = 0.4),
    axis.ticks.length = unit(1.4, "mm"),
    
    panel.spacing = unit(0.55, "lines"),
    plot.margin = margin(6, 14, 6, 6)
  )

panel_abundance_hist