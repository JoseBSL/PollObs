# FIGURE 2 ONLY: Boxplot + weekly jitter + dashed full line + season-value asterisks

library(dplyr)
library(ggplot2)
library(lubridate)

# -----------------------
# Load inputs
# -----------------------
pheno_week   <- readRDS("Data/Working_files/Mantel_pheno_week_result.rds")
pheno_season <- readRDS("Data/Working_files/Mantel_pheno_season_result.rds")
pheno_full   <- readRDS("Data/Working_files/Mantel_pheno_full_result.rds")

groupped_dates <- readRDS("Data/Working_files/groupped_dates.rds") %>%
  mutate(Sampling_week = isoweek(Date))

# Season colors and order
season_cols <- c(
  Early = "#66c2a5",
  Mid   = "#fee08b",
  Late  = "#f46d43"
)

season_cols_darker <- c(
  Early = "#3fa78a",   # darker green-teal
  Mid   = "#f2c94c",   # deeper warm yellow
  Late  = "#e4572e"    # richer orange-red
)
season_levels <- c("Early", "Mid", "Late")

# -----------------------
# 1) Unique Season per (garden, week)
# -----------------------
week_season <- groupped_dates %>%
  mutate(Season = factor(Season, levels = season_levels)) %>%
  count(Botanical_garden, Sampling_week, Season, name = "n") %>%
  group_by(Botanical_garden, Sampling_week) %>%
  slice_max(n, n = 1, with_ties = FALSE) %>%   # dominant season in that week
  ungroup() %>%
  select(Botanical_garden, Sampling_week, Season)

# -----------------------
# 2) Weekly data for boxplot
# -----------------------
pheno_week_box <- pheno_week %>%
  select(-any_of("Season")) %>%
  left_join(week_season, by = c("Botanical_garden", "Sampling_week")) %>%
  mutate(Season = factor(Season, levels = season_levels))

# -----------------------
# 3) Full reference lines (one per garden)
# -----------------------
pheno_full_line <- pheno_full %>%
  select(Botanical_garden, Mantel_corr)

# -----------------------
# 4) Season-level "asterisk" markers (one per garden × season)
# -----------------------
pheno_season_star <- pheno_season %>%
  mutate(Season = factor(Season, levels = season_levels)) %>%
  select(Botanical_garden, Season, Mantel_corr)

# -----------------------
# 5) Plot
# -----------------------
fig2 <- ggplot(pheno_week_box, aes(x = Season, y = Mantel_corr, fill = Season)) +
  
  # Boxplots (weekly distribution)
  geom_boxplot(alpha = 0.7, width = 0.65, outlier.shape = NA) +
  
  # Weekly jitter points (same fill)
  geom_jitter(
    aes(fill = Season),
    shape = 21,
    color = "grey30",
    width = 0.12,
    alpha = 0.5,
    size = 1.6
  ) +
  
  # Full-network reference line (dashed grey)
  geom_hline(
    data = pheno_full_line,
    aes(yintercept = Mantel_corr),
    linetype = "dashed",
    color = "grey40",
    linewidth = 0.8,
    inherit.aes = FALSE
  ) +
  
  # Season-level value marker (asterisk/star)
  geom_point(
    data = pheno_season_star,
    aes(x = Season, y = Mantel_corr, color = Season),
    inherit.aes = FALSE,
    shape = 8,          # star/asterisk
    size = 2,
    stroke = 1.1
  ) +
  
  facet_wrap(~Botanical_garden, ncol = 1) +
  theme_minimal() +
  coord_cartesian(ylim = c(0, 1)) +
  labs(
    x = "Season",
    y = "Mantel test",
    fill = "Season"
  ) +
  scale_fill_manual(values = season_cols, drop = FALSE) +
  scale_color_manual(values = season_cols_darker, drop = FALSE)+
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5))

print(fig2)

# Optional save:
# ggsave("Figure2_boxplot_full_with_season_star.png", fig2, width = 6, height = 8, dpi = 300)
