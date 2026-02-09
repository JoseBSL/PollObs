
library(dplyr)
library(ggplot2)

r2_season_blocks = readRDS("Data/Working_files/r2_season_blocks.rds")
r2_weekly        = readRDS("Data/Working_files/r2_weekly.rds")
r2_full_season   = readRDS("Data/Working_files/r2_full_season.rds")

# --- Keep only the predictors you want (edit if needed) ---
keep_terms <- c("Floral abundance", "Pollinator abundance")
r2_season_blocks <- r2_season_blocks %>% filter(Term %in% keep_terms)
r2_weekly        <- r2_weekly %>% filter(Term %in% keep_terms)
r2_full_season   <- r2_full_season %>% filter(Term %in% keep_terms)

# -----------------------------
# Build a common x-axis Period
# -----------------------------
# Weekly Periods: W01, W02, ...
r2_weekly_plot <- r2_weekly %>%
  mutate(
    Week_num = as.integer(as.character(Week)),
    Period = sprintf("W%02d", Week_num),
    Analysis = "Weekly"
  ) %>%
  select(Analysis, Period, Term, Delta_R2)

week_levels <- r2_weekly_plot %>%
  distinct(Period) %>%
  arrange(as.integer(sub("W", "", Period))) %>%
  pull(Period)

# Season blocks Periods: Early/Mid/Late
r2_season_blocks_plot <- r2_season_blocks %>%
  mutate(
    Period = as.character(Season),
    Analysis = "Season blocks"
  ) %>%
  select(Analysis, Period, Term, Delta_R2)

# Full season Period: Full
r2_full_season_plot <- r2_full_season %>%
  mutate(
    Period = "Full",
    Analysis = "Full season"
  ) %>%
  select(Analysis, Period, Term, Delta_R2)

# Combine
r2_combined <- bind_rows(r2_full_season_plot, r2_season_blocks_plot, r2_weekly_plot) %>%
  mutate(
    Period = factor(Period, levels = c("Full", "Early", "Mid", "Late", week_levels), ordered = TRUE),
    Term = factor(Term, levels = keep_terms)
  )

# -----------------------------
# Plot
# -----------------------------
ggplot() +
  
  # Weekly trajectories
  geom_line(
    data = r2_combined %>% filter(Analysis == "Weekly"),
    aes(x = Period, y = Delta_R2, color = Term, group = Term),
    linewidth = 1.1
  ) +
  geom_point(
    data = r2_combined %>% filter(Analysis == "Weekly"),
    aes(x = Period, y = Delta_R2, color = Term),
    size = 2
  ) +
  
  # Season block summaries
  geom_point(
    data = r2_combined %>% filter(Analysis == "Season blocks"),
    aes(x = Period, y = Delta_R2, color = Term, shape = Analysis),
    size = 4
  ) +
  
  # Full season summary
  geom_point(
    data = r2_combined %>% filter(Analysis == "Full season"),
    aes(x = Period, y = Delta_R2, color = Term, shape = Analysis),
    size = 5
  ) +
  
  scale_shape_manual(values = c(
    "Season blocks" = 18,
    "Full season" = 8
  )) +
  scale_color_viridis_d() +
  
  labs(
    x = "Time period",
    y = "Δ marginal R²",
    color = "Predictor",
    shape = "Summary level",
    title = "Relative importance across temporal scales"
  ) +
  
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
