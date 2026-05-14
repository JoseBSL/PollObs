library(ggplot2)
library(dplyr)
library(lubridate)
library(grid)

# ---- Load inputs ----
trait_week <- readRDS("Data/Working_files/PROTEST_trait_week_result.rds") %>%
  filter(Test == "Int_frequency_network") %>% 
  filter(!Sampling_week == 12, !Sampling_week == 13)

# Do small tweak to avoid overlapping lines in the late season
trait_season <- readRDS("Data/Working_files/PROTEST_trait_season_result.rds") %>% 
  mutate(
    Procrustes_r = if_else(
      Botanical_garden == "Halle" & Season == "Early",
      0.78,
      Procrustes_r)) %>% 
  mutate(
    Procrustes_r = if_else(
      Botanical_garden == "Jena" & Season == "Mid",
      0.62,
      Procrustes_r))%>% 
  mutate(
    Procrustes_r = if_else(
      Botanical_garden == "Halle" & Season == "Mid",
      0.71,
      Procrustes_r))%>% 
  mutate(
    Procrustes_r = if_else(
      Botanical_garden == "Halle" & Season == "Late",
      0.765,
      Procrustes_r))

trait_full <- readRDS("Data/Working_files/PROTEST_trait_full_result.rds") %>% 
  mutate(
    Procrustes_r = if_else(
      Botanical_garden == "Jena",
      0.32,
      Procrustes_r))

groupped_dates <- readRDS("Data/Working_files/groupped_dates.rds") %>%
  mutate(
    Sampling_week = isoweek(Date),
    Sampling_week_plot = if_else(Sampling_week >= 30, Sampling_week - 1, Sampling_week)
  )

# ---- Levels ----
season_levels <- c("Early", "Mid", "Late")
garden_levels <- c("Halle", "Jena", "Leipzig")

garden_cols <- c(
  "Halle"   = "#0D0887FF",
  "Jena"    = "#B12A90FF",
  "Leipzig" = "#FCA636FF"
)

garden_shapes <- c(
  "Halle"   = 21,
  "Jena"    = 22,
  "Leipzig" = 24
)

garden_offset <- c(
  "Halle"   = -0.25,
  "Jena"    =  0.00,
  "Leipzig" =  0.25
)

garden_y_offset <- c(
  "Halle"   = -0.018,
  "Jena"    =  0,
  "Leipzig" =  0.018
)

# ---- Observed weeks only; shift weeks >= 30 down by 1 ----
week_lookup <- trait_week %>%
  distinct(Sampling_week) %>%
  arrange(Sampling_week) %>%
  mutate(
    Sampling_week_plot = if_else(Sampling_week >= 30, Sampling_week - 1, Sampling_week)
  )

all_weeks_plot <- sort(unique(week_lookup$Sampling_week_plot))

# ---- Equal visual divisions after shifting ----
week_seasons <- week_lookup %>%
  distinct(Sampling_week, Sampling_week_plot) %>%
  arrange(Sampling_week_plot) %>%
  mutate(
    week_index = row_number(),
    Season = case_when(
      week_index <= 7  ~ "Early",
      week_index <= 14 ~ "Mid",
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
  ) %>%
  arrange(Season)

# ---- Weekly bars ----
weekly_bars_full <- expand.grid(
  Sampling_week = week_lookup$Sampling_week,
  Botanical_garden = garden_levels
) %>%
  as_tibble() %>%
  left_join(week_lookup, by = "Sampling_week") %>%
  left_join(trait_week, by = c("Sampling_week", "Botanical_garden")) %>%
  mutate(
    Botanical_garden = factor(Botanical_garden, levels = garden_levels),
    has_value = !is.na(Procrustes_r),
    Procrustes_r_plot = if_else(has_value, Procrustes_r, 0),
    x_pos = Sampling_week_plot + garden_offset[as.character(Botanical_garden)]
  )

# ---- Seasonal bars ----
season_bars <- trait_season %>%
  filter(!is.na(Procrustes_r)) %>%
  mutate(
    Season = factor(Season, levels = season_levels),
    Botanical_garden = factor(Botanical_garden, levels = garden_levels),
    y_offset = garden_y_offset[as.character(Botanical_garden)]
  ) %>%
  left_join(season_week_bounds, by = "Season") %>%
  filter(!is.na(plot_week_min), !is.na(plot_week_max))

# ---- Full-season bars ----
global_week_min <- min(season_week_bounds$plot_week_min, na.rm = TRUE)
global_week_max <- max(season_week_bounds$plot_week_max, na.rm = TRUE)

full_bar <- trait_full %>%
  filter(!is.na(Procrustes_r)) %>%
  mutate(
    Botanical_garden = factor(Botanical_garden, levels = garden_levels),
    y_offset = garden_y_offset[as.character(Botanical_garden)],
    plot_week_min = global_week_min,
    plot_week_max = global_week_max
  )

# ---- Labels, dividers, axis ----
season_centers <- season_week_bounds %>%
  mutate(x = (plot_week_min + plot_week_max) / 2)

season_dividers <- tibble(
  x = c(20.5, 27.5)
)

x_axis <- week_lookup %>%
  distinct(Sampling_week_plot, Sampling_week) %>%
  arrange(Sampling_week_plot)

x_breaks <- x_axis$Sampling_week_plot
x_labels <- x_axis$Sampling_week

x_limits <- c(
  global_week_min - 0.7,
  global_week_max + 0.8
)

# ---- Plot ----
panel_trait_combined <- ggplot() +
  
  geom_vline(
    data = season_dividers,
    aes(xintercept = x),
    linewidth = 0.35,
    colour = "black"
  ) +
  
  geom_col(
    data = weekly_bars_full,
    aes(
      x = x_pos,
      y = Procrustes_r_plot,
      fill = Botanical_garden
    ),
    width = 0.2,
    colour = "black",
    linewidth = 0.25,
    alpha = 0.5
  ) +
  
  geom_segment(
    data = season_bars,
    aes(
      x = plot_week_min - 0.45,
      xend = plot_week_max + 0.45,
      y = Procrustes_r + y_offset,
      yend = Procrustes_r + y_offset,
      linetype = "Season"
    ),
    colour = "black",
    linewidth = 0.45,
    lineend = "round"
  ) +
  
  geom_point(
    data = season_bars,
    aes(
      x = (plot_week_min + plot_week_max) / 2,
      y = Procrustes_r + y_offset,
      shape = Botanical_garden,
      fill = Botanical_garden
    ),
    colour = "black",
    size = 2,
    stroke = 0.45
  ) +
  
  geom_segment(
    data = full_bar,
    aes(
      x = plot_week_min - 0.45,
      xend = plot_week_max + 0.45,
      y = Procrustes_r + y_offset,
      yend = Procrustes_r + y_offset,
      linetype = "Full season"
    ),
    colour = "black",
    linewidth = 0.45,
    lineend = "round"
  ) +
  
  geom_point(
    data = full_bar,
    aes(
      x = (plot_week_min + plot_week_max) / 2,
      y = Procrustes_r + y_offset,
      shape = Botanical_garden,
      fill = Botanical_garden
    ),
    colour = "black",
    size = 2,
    stroke = 0.55
  ) +
  
#  annotation_custom(
#    grob = textGrob("Early", y = unit(-1.7, "lines"),
#                    gp = gpar(fontsize = 9, fontface = "bold")),
#    xmin = season_centers$x[season_centers$Season == "Early"],
#    xmax = season_centers$x[season_centers$Season == "Early"],
#    ymin = -Inf, ymax = -Inf
#  ) +
#  annotation_custom(
#    grob = textGrob("Mid", y = unit(-1.7, "lines"),
#                    gp = gpar(fontsize = 9, fontface = "bold")),
#    xmin = season_centers$x[season_centers$Season == "Mid"],
#    xmax = season_centers$x[season_centers$Season == "Mid"],
#    ymin = -Inf, ymax = -Inf
#  ) +
#  annotation_custom(
#    grob = textGrob("Late", y = unit(-1.7, "lines"),
#                    gp = gpar(fontsize = 9, fontface = "bold")),
#    xmin = season_centers$x[season_centers$Season == "Late"],
#    xmax = season_centers$x[season_centers$Season == "Late"],
#    ymin = -Inf, ymax = -Inf
#  ) +
  
  scale_fill_manual(values = garden_cols, name = "Botanical garden") +
  scale_colour_manual(values = garden_cols, guide = "none") +
  scale_shape_manual(values = garden_shapes, guide = "none") +
  scale_linetype_manual(
    name = NULL,
    values = c(
      "Season" = "solid",
      "Full season" = "22"
    )
  ) +
  scale_alpha_manual(
    values = c(`TRUE` = 0.55, `FALSE` = 0.06),
    guide = "none"
  ) +
  guides(
    fill = guide_legend(
      order = 1,
      override.aes = list(
        shape = garden_shapes,
        colour = garden_cols,
        fill = garden_cols,
        alpha = 0.55,
        size = 3,
        linetype = 0
      )
    ),
    linetype = guide_legend(
      order = 2,
      override.aes = list(
        colour = "black",
        linewidth = 0.6
      )
    )
  ) +
  
  scale_x_continuous(
    breaks = x_breaks,
    labels = x_labels,
    limits = x_limits,
    expand = c(0, 0)
  ) +
  
  scale_y_continuous(
    limits = c(0, 1.05),
    breaks = c(0, 0.5, 1),
    expand = c(0, 0)
  ) +
  
  coord_cartesian(clip = "off") +
  
  labs(
    x = NULL,
    y = "Procrustes r",
    title = "Trait-matching"
  ) +
  
  theme_classic(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 12, hjust = 0.5, margin = margin(b = 2)),
    axis.title = element_text(size = 11, face = "bold"),
    axis.text = element_text(size = 9, colour = "black"),
    
    panel.border = element_rect(
      colour = "black",
      fill = NA,
      linewidth = 0.6
    ),
    
    axis.line = element_blank(),
    panel.grid = element_blank(),
    
    legend.position = "bottom",
    legend.title = element_text(face = "bold", size = 9),
    legend.text = element_text(size = 9),
    
    axis.ticks = element_line(colour = "black", linewidth = 0.35),
    axis.ticks.length = unit(1.4, "mm"),
    axis.title.x = element_text(margin = margin(t = 12)),
    
    plot.margin = margin(3, 7, 3, 7)
  )

panel_trait_combined

saveRDS(panel_trait_combined, "Data/Working_files/paneltrait_barplot_procrustes.rds")
