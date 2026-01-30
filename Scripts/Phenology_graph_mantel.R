# FIGURE 2 (clean): violin + weekly dotplot + seasonal points + full-season dashed line + unified legend

library(dplyr)
library(ggplot2)
library(lubridate)

# --- Load inputs ---
pheno_week   <- readRDS("Data/Working_files/Mantel_pheno_week_result.rds")
pheno_season <- readRDS("Data/Working_files/Mantel_pheno_season_result.rds")
pheno_full   <- readRDS("Data/Working_files/Mantel_pheno_full_result.rds")

groupped_dates <- readRDS("Data/Working_files/groupped_dates.rds") %>%
  mutate(Sampling_week = isoweek(Date))

# --- Season colors + order ---
season_cols   <- c(Early = "#66c2a5", Mid = "#fee08b", Late = "#f46d43")
season_levels <- c("Early", "Mid", "Late")

# --- Unique Season per (garden, week) ---
week_season <- groupped_dates %>%
  mutate(Season = factor(Season, levels = season_levels)) %>%
  count(Botanical_garden, Sampling_week, Season, name = "n") %>%
  group_by(Botanical_garden, Sampling_week) %>%
  slice_max(n, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(Botanical_garden, Sampling_week, Season)

# --- Weekly data used for violin + dotplot ---
pheno_week_box <- pheno_week %>%
  select(-any_of("Season")) %>%
  left_join(week_season, by = c("Botanical_garden", "Sampling_week")) %>%
  mutate(Season = factor(Season, levels = season_levels))

# --- Seasonal points (one per garden × season) ---
pheno_season_dot <- pheno_season %>%
  mutate(Season = factor(Season, levels = season_levels)) %>%
  select(Botanical_garden, Season, Mantel_corr)

# --- Outside facet labels ---
panel_labels <- data.frame(
  Botanical_garden = c("Halle", "Jena", "Leipzig"),
  label = c("d) Halle", "e) Jena", "f) Leipzig"),
  x = -Inf, y = Inf
)

# --- Dummy to force "Weekly" entry in size legend (dotplot doesn't create it reliably) ---
dummy_weekly <- pheno_week_box %>%
  distinct(Botanical_garden, Season) %>%
  mutate(Mantel_corr = 0) %>%
  slice(1)

# --- Plot ---
panel2 <- ggplot(pheno_week_box, aes(x = Season, y = Mantel_corr, fill = Season)) +
  
  geom_violin(alpha = 0.45, width = 0.65, colour = NA) +
  
  # Full-season dashed line (in legend)
  geom_hline(
    data = pheno_full,
    aes(yintercept = Mantel_corr, linetype = "Full season"),
    color = "black",
    linewidth = 0.4,
    inherit.aes = FALSE
  ) +
  
  # Weekly dotplot
  geom_dotplot(
    aes(fill = Season),
    binaxis = "y",
    stackdir = "center",
    dotsize = 1.35,
    alpha = 0.85,
    binwidth = 0.04,
    stackratio = 1.25
  ) +
  
  # Dummy invisible point for the "Weekly" size legend entry
  geom_point(
    data = dummy_weekly,
    aes(x = Season, y = Mantel_corr, size = "Weekly"),
    inherit.aes = FALSE,
    alpha = 0
  ) +
  
  # Seasonal point (nudged right)
  geom_point(
    data = pheno_season_dot,
    aes(x = Season, y = Mantel_corr, fill = Season, size = "Seasonal"),
    inherit.aes = FALSE,
    shape = 23,
    stroke = 0.5,
    alpha = 0.95,
    position = position_nudge(x = +0.22)
  ) +
  
  facet_wrap(~Botanical_garden, ncol = 1) +
  theme_minimal() +
  coord_cartesian(ylim = c(0, 1), clip = "off") +
  
  geom_text(
    data = panel_labels,
    aes(x = x, y = y, label = label),
    inherit.aes = FALSE,
    hjust = -0.05,
    vjust = -0.6,
    fontface = "bold",
    size = 3
  ) +
  
  scale_y_continuous(breaks = c(0, 0.5, 1), limits = c(0, 1)) +
  scale_fill_manual(values = season_cols, drop = FALSE) +
  
  # Temporal complexity legend: points
  scale_size_manual(
    name   = "Temporal complexity",
    breaks = c("Weekly", "Seasonal"),
    labels = c(Weekly = "Weekly", Seasonal = "Weekly aggregated"),
    values = c(Weekly = 1.4, Seasonal = 1.8)
  ) +
  
  # Temporal complexity legend: line
  scale_linetype_manual(
    name   = "Temporal complexity",
    breaks = c("Full season"),
    values = c("Full season" = "dashed")
  ) +
  
  # Unify legend block (line under dots, no second title)
  guides(
    fill  = "none",
    color = "none",
    size = guide_legend(
      order = 1,
      title = "Temporal complexity",
      override.aes = list(
        shape = c(21, 23),
        color = c("black", "black"),
        fill  = c("grey70", "grey70"),
        alpha = c(1, 1)
      )
    ),
    linetype = guide_legend(
      order = 2,
      title = NULL,
      override.aes = list(color = "black", linewidth = 0.8)
    )
  ) +
  
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    panel.spacing = unit(1, "lines"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.6),
    axis.ticks.y = element_line(color = "black", linewidth = 0.5),
    axis.ticks.length = unit(0.2, "cm"),
    strip.background = element_blank(),
    strip.text = element_blank(),
    strip.placement = "outside",
    plot.margin = margin(t = 20 , r = 20, b = 20, l = 20),
    panel.grid.minor = element_blank(),
    legend.position = "none",   # ← THIS LINE
    plot.title = element_text(face = "bold", size = 10, vjust = 3)
  ) +
  ylab("Mantel test") +
  xlab(NULL) +
  ggtitle("Visitation rate – Phenology")

panel2
