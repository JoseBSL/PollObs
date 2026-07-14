# ============================================================
# Phenology PROTEST plot
# Weekly mean + weekly min/max ribbon + seasonal/full lines
# ============================================================

library(ggplot2)
library(dplyr)
library(lubridate)
library(grid)

# ---- Load inputs ----

trait_week <- readRDS("Data/Working_files/PROTEST_trait_week_result.rds") %>%
  filter(!Sampling_week %in% c(12, 13)) %>%
  filter(Test == "Int_frequency_network") %>% 
  filter(Botanical_garden == "Jena")

trait_season <- readRDS("Data/Working_files/PROTEST_trait_season_result.rds") %>% 
  filter(Botanical_garden == "Jena")

trait_full <- readRDS("Data/Working_files/PROTEST_trait_full_result.rds") %>% 
  filter(Botanical_garden == "Jena")

# ---- Settings ----

season_levels <- c("Early", "Mid", "Late")
garden_levels <- c("Halle", "Jena", "Leipzig")
type_levels <- c("Weekly", "Weekly aggregated", "Full season")

label_y_pos <- unit(-1.7, "lines")

# ---- Colours ----

weekly_line_col <- "#4C78A8"
weekly_fill_col <- "#8FB6E8"


season_line_col <- "#A06AA1"
season_fill_col <- "#C9A3C9"

full_line_col <- "#C46A32"
full_fill_col <- "#E28A45"

legend_cols <- c(
  "Weekly" = weekly_line_col,
  "Weekly aggregated" = season_line_col,
  "Full season" = full_line_col
)

legend_fills <- c(
  "Weekly" = weekly_fill_col,
  "Weekly aggregated" = season_fill_col,
  "Full season" = full_fill_col
)

legend_linetypes <- c(
  "Weekly" = "solid",
  "Weekly aggregated" = "longdash",
  "Full season" = "dotted"
)

# ---- Week lookup ----

week_lookup <- trait_week %>%
  distinct(Sampling_week) %>%
  arrange(Sampling_week) %>%
  mutate(
    Sampling_week_plot = if_else(
      Sampling_week >= 30,
      Sampling_week - 1,
      Sampling_week
    )
  )

week_seasons <- week_lookup %>%
  arrange(Sampling_week_plot) %>%
  mutate(
    week_index = row_number(),
    Season = case_when(
      week_index <= 5  ~ "Early",
      week_index <= 12 ~ "Mid",
      week_index <= 21 ~ "Late",
      TRUE ~ NA_character_
    ),
    Season = factor(Season, levels = season_levels)
  ) %>%
  filter(!is.na(Season))

season_week_bounds <- week_seasons %>%
  group_by(Season) %>%
  summarise(
    plot_week_min = min(Sampling_week_plot),
    plot_week_max = max(Sampling_week_plot),
    .groups = "drop"
  )

global_week_min <- min(season_week_bounds$plot_week_min, na.rm = TRUE)
global_week_max <- max(season_week_bounds$plot_week_max, na.rm = TRUE)

season_centers <- season_week_bounds %>%
  mutate(x = (plot_week_min + plot_week_max) / 2)

season_dividers <- tibble(x = c(20.5, 27.5))

x_axis <- week_lookup %>%
  distinct(Sampling_week_plot, Sampling_week) %>%
  arrange(Sampling_week_plot)

# ---- Weekly summary ----

weekly_summary <- trait_week %>%
  left_join(week_lookup, by = "Sampling_week") %>%
  group_by(Sampling_week_plot) %>%
  summarise(
    mean_r = mean(Procrustes_r, na.rm = TRUE),
    min_r  = min(Procrustes_r, na.rm = TRUE),
    max_r  = max(Procrustes_r, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(Type = factor("Weekly", levels = type_levels))

# ---- Seasonal summary ----

season_bars <- trait_season %>%
  filter(!is.na(Procrustes_r)) %>%
  mutate(
    Season = factor(Season, levels = season_levels),
    Botanical_garden = factor(Botanical_garden, levels = garden_levels)
  ) %>%
  left_join(season_week_bounds, by = "Season") %>%
  filter(!is.na(plot_week_min), !is.na(plot_week_max))

season_summary <- season_bars %>%
  group_by(Season, plot_week_min, plot_week_max) %>%
  summarise(
    mean_r = mean(Procrustes_r, na.rm = TRUE),
    min_r  = min(Procrustes_r, na.rm = TRUE),
    max_r  = max(Procrustes_r, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(Type = factor("Weekly aggregated", levels = type_levels))

# ---- Full-season summary ----

full_summary <- trait_full %>%
  filter(!is.na(Procrustes_r)) %>%
  summarise(
    mean_r = mean(Procrustes_r, na.rm = TRUE),
    min_r  = min(Procrustes_r, na.rm = TRUE),
    max_r  = max(Procrustes_r, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    plot_week_min = global_week_min,
    plot_week_max = global_week_max,
    Type = factor("Full season", levels = type_levels)
  )

# ---- Plot ----

main_trait_plot <- ggplot() +
  
  geom_rect(
    data = full_summary,
    aes(
      xmin = plot_week_min,
      xmax = plot_week_max,
      ymin = min_r,
      ymax = max_r,
      fill = Type
    ),
    inherit.aes = FALSE,
    alpha = 0.055
  ) +
  
  geom_rect(
    data = season_summary,
    aes(
      xmin = plot_week_min,
      xmax = plot_week_max,
      ymin = min_r,
      ymax = max_r,
      fill = Type
    ),
    inherit.aes = FALSE,
    alpha = 0.10
  ) +
  
  geom_ribbon(
    data = weekly_summary,
    aes(
      x = Sampling_week_plot,
      ymin = min_r,
      ymax = max_r,
      fill = Type
    ),
    inherit.aes = FALSE,
    alpha = 0.22
  ) +
  
  geom_segment(
    data = full_summary,
    aes(
      x = plot_week_min,
      xend = plot_week_max,
      y = mean_r,
      yend = mean_r,
      colour = Type,
      linetype = Type
    ),
    linewidth = 1
  ) +
  
  geom_segment(
    data = season_summary,
    aes(
      x = plot_week_min,
      xend = plot_week_max,
      y = mean_r,
      yend = mean_r,
      colour = Type,
      linetype = Type
    ),
    linewidth = 1
  ) +
  
  geom_errorbar(
    data = weekly_summary,
    aes(
      x = Sampling_week_plot,
      ymin = min_r,
      ymax = max_r
    ),
    width = 0.08,
    linewidth = 0.4,
    colour = weekly_line_col,
    alpha = 0.7
  ) +
  
  geom_line(
    data = weekly_summary,
    aes(
      x = Sampling_week_plot,
      y = mean_r,
      colour = Type,
      linetype = Type,
      group = 1
    ),
    linewidth = 1.05
  ) +
  
  geom_point(
    data = weekly_summary,
    aes(
      x = Sampling_week_plot,
      y = mean_r,
      colour = Type
    ),
    shape = 21,
    fill = "white",
    size = 4,
    stroke = 1
  ) +
  
  geom_vline(
    data = season_dividers,
    aes(xintercept = x),
    linewidth = 0.35,
    colour = "black"
  ) +
  
  annotation_custom(
    textGrob("Early", y = label_y_pos, gp = gpar(fontsize = 10, fontface = "bold")),
    xmin = season_centers$x[season_centers$Season == "Early"],
    xmax = season_centers$x[season_centers$Season == "Early"],
    ymin = -Inf,
    ymax = -Inf
  ) +
  
  annotation_custom(
    textGrob("Mid", y = label_y_pos, gp = gpar(fontsize = 10, fontface = "bold")),
    xmin = season_centers$x[season_centers$Season == "Mid"],
    xmax = season_centers$x[season_centers$Season == "Mid"],
    ymin = -Inf,
    ymax = -Inf
  ) +
  
  annotation_custom(
    textGrob("Late", y = label_y_pos, gp = gpar(fontsize = 10, fontface = "bold")),
    xmin = season_centers$x[season_centers$Season == "Late"],
    xmax = season_centers$x[season_centers$Season == "Late"],
    ymin = -Inf,
    ymax = -Inf
  ) +
  
  scale_fill_manual(
    values = legend_fills,
    breaks = type_levels,
    name = "Min-max range"
  ) +
  
  scale_colour_manual(
    values = legend_cols,
    breaks = type_levels,
    name = "Mean"
  ) +
  
  scale_linetype_manual(
    values = legend_linetypes,
    breaks = type_levels,
    name = "Mean"
  ) +
  
  scale_x_continuous(
    breaks = x_axis$Sampling_week_plot,
    labels = x_axis$Sampling_week,
    limits = c(global_week_min - 0.7, global_week_max + 0.8),
    expand = c(0, 0)
  ) +
  
  scale_y_continuous(
    limits = c(0, 1.05),
    breaks = c(0, 0.5, 1),
    expand = c(0, 0)
  ) +
  
  coord_cartesian(clip = "off") +
  
  labs(
    x = "Sampling week",
    y = "Procrustes r",
    title = "f) Trait-matching"
  ) +
  
  guides(
    colour = guide_legend(order = 1),
    linetype = guide_legend(order = 1),
    fill = guide_legend(order = 2)
  ) +
  
  theme_classic(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 12, hjust = 0.5, margin = margin(b = 2)),
    axis.title = element_text(size = 11, face = "bold"),
    axis.text = element_text(size = 9, colour = "black"),
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.6),
    axis.line = element_blank(),
    panel.grid = element_blank(),
    legend.position = "right",
    legend.box = "vertical",
    legend.title = element_text(face = "bold", size = 9),
    legend.text = element_text(size = 8.5),
    axis.ticks = element_line(colour = "black", linewidth = 0.35),
    axis.ticks.length = unit(1.4, "mm"),
    axis.title.x = element_text(margin = margin(t = 12)),
    plot.margin = margin(3, 2, 8, 7)
  )

main_trait_plot


saveRDS(main_trait_plot, "Data/Working_files/main_trait_plot_protest_Jena.rds")
