
# FIGURE 2 (TRAIT / SIZE MATCHING): Mantel output version

library(dplyr)
library(ggplot2)
library(lubridate)

# --- Load inputs ---
trait_week   <- readRDS("Data/Working_files/Mantel_trait_week_result.rds")
trait_season <- readRDS("Data/Working_files/Mantel_trait_season_result.rds")
trait_full   <- readRDS("Data/Working_files/Mantel_trait_full_result.rds")

groupped_dates <- readRDS("Data/Working_files/groupped_dates.rds") %>%
  mutate(Sampling_week = isoweek(Date))

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

# --- Weekly data ---
trait_week_box <- trait_week %>%
  select(-any_of("Season")) %>%
  left_join(week_season, by = c("Botanical_garden", "Sampling_week")) %>%
  mutate(
    Season   = factor(Season, levels = season_levels),
    Mantel_r = as.numeric(Mantel_corr)
  ) %>%
  filter(is.finite(Mantel_r))

# --- Seasonal points ---
trait_season_dot <- trait_season %>%
  mutate(
    Season   = factor(Season, levels = season_levels),
    Mantel_r = as.numeric(Mantel_corr)
  ) %>%
  select(Botanical_garden, Season, Mantel_r) %>%
  filter(is.finite(Mantel_r))

# --- Full season dashed line ---
trait_full_line <- trait_full %>%
  mutate(Mantel_r = as.numeric(Mantel_corr)) %>%
  select(Botanical_garden, Mantel_r) %>%
  filter(is.finite(Mantel_r))

panel_labels <- data.frame(
  Botanical_garden = c("Halle", "Jena", "Leipzig"),
  label = c("g) Halle", "h) Jena", "i) Leipzig"),
  x = -Inf, y = Inf
)

dummy_weekly <- trait_week_box %>%
  distinct(Botanical_garden, Season) %>%
  mutate(Mantel_r = 0) %>%
  slice(1)

# --- Plot ---
panel3 <- ggplot(trait_week_box, aes(x = Season, y = Mantel_r, fill = Season)) +
  
  geom_violin(alpha = 0.45, width = 0.5, colour = NA, adjust = 1.2,
              scale = "width", trim = TRUE, cut = 0) +
  
  geom_hline(
    data = trait_full_line,
    aes(yintercept = Mantel_r, linetype = "Full season"),
    color = "black", linewidth = 0.4, inherit.aes = FALSE
  ) +
  
  geom_dotplot(
    aes(fill = Season),
    binaxis = "y", stackdir = "center",
    dotsize = 1.35, alpha = 0.85,
    binwidth = 0.04, stackratio = 1.25
  ) +
  
  geom_point(
    data = dummy_weekly,
    aes(x = Season, y = Mantel_r, size = "Weekly"),
    inherit.aes = FALSE, alpha = 0
  ) +
  
  geom_point(
    data = trait_season_dot,
    aes(x = Season, y = Mantel_r, fill = Season, size = "Seasonal"),
    inherit.aes = FALSE,
    shape = 23, stroke = 0.5, alpha = 0.95,
    position = position_nudge(x = +0.5)
  ) +
  
  facet_wrap(~Botanical_garden, ncol = 1) +
  theme_minimal() +
  coord_cartesian(ylim = c(0, 1), clip = "off") +
  
  geom_text(
    data = panel_labels,
    aes(x = x, y = y, label = label),
    inherit.aes = FALSE,
    hjust = -0.05, vjust = -0.6,
    fontface = "bold", size = 5
  ) +
  
  scale_y_continuous(breaks = c(0, 0.5, 1), limits = c(0, 1)) +
  scale_fill_manual(values = season_cols, drop = FALSE) +
  
  scale_size_manual(
    name   = "Temporal complexity",
    breaks = c("Weekly", "Seasonal"),
    labels = c(Weekly = "Weekly", Seasonal = "Weekly aggregated"),
    values = c(Weekly = 1.4, Seasonal = 2.4)
  ) +
  
  scale_linetype_manual(
    name   = "Temporal complexity",
    breaks = c("Full season"),
    values = c("Full season" = "dashed")
  ) +
  
  guides(
    fill  = "none",
    color = "none",
    size = guide_legend(
      order = 1,
      title = "Temporal complexity",
      override.aes = list(
        shape = c(21, 23),
        size  = c(2.5, 2.5),
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
    panel.spacing = unit(1.8, "lines"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.6),
    axis.ticks.y = element_line(color = "black", linewidth = 0.5),
    axis.ticks.length = unit(0.2, "cm"),
    strip.background = element_blank(),
    strip.text = element_blank(),
    strip.placement = "outside",
    plot.margin = margin(t = 60, r = 20, b = 20, l = 20),
    panel.grid.minor = element_blank(),
    legend.box = "vertical",
    legend.box.just = "left",
    legend.spacing = unit(-10, "pt"),
    legend.key.width  = unit(1, "cm"),
    legend.key.height = unit(0.45, "cm"),
    legend.margin = margin(t = -10, r = 0, b = 0, l = 0),
    legend.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold", size = 10, vjust = 4.5)
  ) +
  ylab(NULL) +
  xlab(NULL) +
  ggtitle("Visitation rate – Size matching")

panel3

saveRDS(panel3, "Data/Working_files/Figures2_panel3.rds")
