# ---- Offsets for gardens within each week ----
garden_offset <- c(
  "Halle"   = -0.25,
  "Jena"    =  0.00,
  "Leipzig" =  0.25
)

weekly_bars_full <- weekly_bars_full %>%
  mutate(
    x_pos = Sampling_week + garden_offset[as.character(Botanical_garden)]
  )

season_bars <- season_bars %>%
  mutate(
    y_offset = c("Halle" = -0.015, "Jena" = 0, "Leipzig" = 0.015)[as.character(Botanical_garden)]
  )

full_bar <- full_bar %>%
  mutate(
    y_offset = c("Halle" = -0.015, "Jena" = 0, "Leipzig" = 0.015)[as.character(Botanical_garden)]
  )

panel_abundance_combined <- ggplot() +
  
  # weekly bars, dodged manually by garden
  geom_col(
    data = weekly_bars_full,
    aes(
      x = x_pos,
      y = Procrustes_r_plot,
      fill = Botanical_garden,
      alpha = has_value
    ),
    width = 0.22,
    colour = NA
  ) +
  
  # seasonal values, slightly offset vertically
  geom_segment(
    data = season_bars,
    aes(
      x = week_min - 0.45,
      xend = week_max + 0.45,
      y = Procrustes_r + y_offset,
      yend = Procrustes_r + y_offset,
      colour = Botanical_garden
    ),
    linewidth = 0.75,
    lineend = "round"
  ) +
  
  # full-season values
  geom_segment(
    data = full_bar,
    aes(
      x = week_min - 0.5,
      xend = week_max + 0.5,
      y = Procrustes_r + y_offset,
      yend = Procrustes_r + y_offset,
      colour = Botanical_garden
    ),
    linewidth = 0.9,
    linetype = "22",
    lineend = "round"
  ) +
  
  scale_fill_manual(values = garden_cols, name = "Botanical garden") +
  scale_colour_manual(values = garden_cols, guide = "none") +
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
      min(weekly_bars_full$Sampling_week, na.rm = TRUE) - 0.7,
      max(weekly_bars_full$Sampling_week, na.rm = TRUE) + 0.8
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
    
    panel.border = element_rect(
      colour = "black",
      fill = NA,
      linewidth = 0.65
    ),
    
    axis.line = element_blank(),
    panel.grid = element_blank(),
    
    legend.position = "top",
    legend.title = element_text(face = "bold", size = 9),
    legend.text = element_text(size = 9),
    
    axis.ticks = element_line(colour = "black", linewidth = 0.4),
    axis.ticks.length = unit(1.4, "mm"),
    
    plot.margin = margin(6, 10, 6, 6)
  )

panel_abundance_combined