# FIGURE 2 (TRAIT / SIZEMATCHING): violin + weekly dotplot + seasonal points + full-season dashed line + unified legend
# WORKING with PROTEST outputs (uses Procrustes_r)

library(dplyr)
library(ggplot2)
library(lubridate)

# --- Load inputs ---
trait_week   <- readRDS("Data/Working_files/PROTEST_trait_week_result.rds")
trait_season <- readRDS("Data/Working_files/PROTEST_trait_season_result.rds")
trait_full   <- readRDS("Data/Working_files/PROTEST_trait_full_result.rds")

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
trait_week_box <- trait_week %>%
  select(-any_of("Season")) %>%
  left_join(week_season, by = c("Botanical_garden", "Sampling_week")) %>%
  mutate(
    Season = factor(Season, levels = season_levels),
    Procrustes_r = as.numeric(Procrustes_r)
  ) %>%
  filter(is.finite(Procrustes_r))

# --- Seasonal points (one per garden × season) ---
trait_season_dot <- trait_season %>%
  mutate(
    Season = factor(Season, levels = season_levels),
    Procrustes_r = as.numeric(Procrustes_r)
  ) %>%
  select(Botanical_garden, Season, Procrustes_r) %>%
  filter(is.finite(Procrustes_r))

# --- Full-season dashed line (one per garden) ---
trait_full_line <- trait_full %>%
  mutate(Procrustes_r = as.numeric(Procrustes_r)) %>%
  select(Botanical_garden, Procrustes_r) %>%
  filter(is.finite(Procrustes_r))

# --- Outside facet labels ---
panel_labels <- data.frame(
  Botanical_garden = c("Halle", "Jena", "Leipzig"),
  label = c("g) Halle", "h) Jena", "i) Leipzig"),
  x = -Inf, y = Inf
)

# --- Dummy to force "Weekly" entry in size legend ---
dummy_weekly <- trait_week_box %>%
  distinct(Botanical_garden, Season) %>%
  mutate(Procrustes_r = 0) %>%
  slice(1)

# --- Plot ---
panel3 <- ggplot(trait_week_box, aes(x = Season, y = Procrustes_r, fill = Season)) +
  
  geom_violin(alpha = 0.45, width = 0.65, colour = NA) +
  
  # Full-season dashed line (per facet)
  geom_hline(
    data = trait_full_line,
    aes(yintercept = Procrustes_r, linetype = "Full season"),
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
    aes(x = Season, y = Procrustes_r, size = "Weekly"),
    inherit.aes = FALSE,
    alpha = 0
  ) +
  
  # Seasonal point (nudged right)
  geom_point(
    data = trait_season_dot,
    aes(x = Season, y = Procrustes_r, fill = Season, size = "Seasonal"),
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
    legend.box = "vertical",
    legend.box.just = "left",
    legend.spacing = unit(1, "pt"),
    legend.key.width  = unit(1, "cm"),
    legend.key.height = unit(0.45, "cm"),
    legend.margin = margin(t = 0, r = 0, b = 0, l = 0),
    legend.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold", size = 10, vjust = 3)
  ) +
  ylab("PROTEST (Procrustes r)") +
  xlab(NULL) +
  ggtitle("Visitation rate – Sizematching")

panel3
