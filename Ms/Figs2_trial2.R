# ---- Shared label height ----
label_y_pos <- unit(-1.7, "lines")


# ---- Weekly mean + range across gardens ----
weekly_summary <- weekly_bars_full %>%
  filter(has_value) %>%
  group_by(Sampling_week_plot) %>%
  summarise(
    mean_r = mean(Procrustes_r, na.rm = TRUE),
    min_r  = min(Procrustes_r, na.rm = TRUE),
    max_r  = max(Procrustes_r, na.rm = TRUE),
    x_pos  = mean(x_pos, na.rm = TRUE),
    .groups = "drop"
  )


# ---- Main plot: weekly mean + range only ----
main_pheno_plot <- ggplot(weekly_summary) +
#  geom_col(
#    aes(x = x_pos, y = mean_r),
#    width = 0.1,
#    fill = "grey70",
#    colour = "white",
#    linewidth = 0.1,
#    alpha = 0.7
#  ) +
  geom_errorbar(
    aes(x = x_pos, ymin = min_r, ymax = max_r),
    width = 0.08,
    linewidth = 0.45,
    colour = "black"
  ) +
  geom_line(
    aes(x = x_pos, y = mean_r, group = 1),
    linewidth = 0.35,
    colour = "grey40",
    alpha = 0.6
  ) +
  geom_point(
    aes(x = x_pos, y = mean_r),
    shape = 21,
    fill = "grey40",
    colour = "white",
    size = 4,
    stroke = 0.4
  ) +
  geom_vline(
    data = season_dividers,
    aes(xintercept = x),
    inherit.aes = FALSE,
    linewidth = 0.35,
    colour = "black"
  ) +
  annotation_custom(
    grob = textGrob("Early", y = label_y_pos,
                    gp = gpar(fontsize = 9, fontface = "bold")),
    xmin = season_centers$x[season_centers$Season == "Early"],
    xmax = season_centers$x[season_centers$Season == "Early"],
    ymin = -Inf, ymax = -Inf
  ) +
  annotation_custom(
    grob = textGrob("Mid", y = label_y_pos,
                    gp = gpar(fontsize = 9, fontface = "bold")),
    xmin = season_centers$x[season_centers$Season == "Mid"],
    xmax = season_centers$x[season_centers$Season == "Mid"],
    ymin = -Inf, ymax = -Inf
  ) +
  annotation_custom(
    grob = textGrob("Late", y = label_y_pos,
                    gp = gpar(fontsize = 9, fontface = "bold")),
    xmin = season_centers$x[season_centers$Season == "Late"],
    xmax = season_centers$x[season_centers$Season == "Late"],
    ymin = -Inf, ymax = -Inf
  ) +
  scale_x_continuous(
    breaks = x_breaks,
    labels = x_labels,
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
    title = "Phenology"
  ) +
  theme_classic(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 12, hjust = 0.5, margin = margin(b = 2)),
    axis.title = element_text(size = 11, face = "bold"),
    axis.text = element_text(size = 9, colour = "black"),
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.6),
    axis.line = element_blank(),
    panel.grid = element_blank(),
    legend.position = "none",
    axis.ticks = element_line(colour = "black", linewidth = 0.35),
    axis.ticks.length = unit(1.4, "mm"),
    axis.title.x = element_text(margin = margin(t = 12)),
    plot.margin = margin(3, 2, 50, 7)
  )


# ---- Middle plot: seasonal mean + range across gardens ----
season_summary <- season_bars %>%
  mutate(Season = factor(Season, levels = c("Early", "Mid", "Late"))) %>%
  group_by(Season) %>%
  summarise(
    mean_r = mean(Procrustes_r, na.rm = TRUE),
    min_r  = min(Procrustes_r, na.rm = TRUE),
    max_r  = max(Procrustes_r, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(season_x = 1)

season_labels <- data.frame(
  Season = factor(c("Early", "Mid", "Late"), levels = c("Early", "Mid", "Late")),
  season_x = 1,
  y = 0,
  label = c("Early", "Mid", "Late")
)

season_pheno_plot <- ggplot(season_summary) +
  geom_col(
    aes(x = season_x, y = mean_r),
    width = 0.18,
    fill = "grey70",
    colour = "white",
    linewidth = 0.25,
    alpha = 0.7
  ) +
  geom_errorbar(
    aes(x = season_x, ymin = min_r, ymax = max_r),
    width = 0.08,
    linewidth = 0.45,
    colour = "black"
  ) +
  geom_point(
    aes(x = season_x, y = mean_r),
    shape = 21,
    fill = "grey40",
    colour = "white",
    size = 4,
    stroke = 0.45
  ) +
  geom_text(
    data = season_labels,
    aes(x = season_x, y = y, label = label),
    inherit.aes = FALSE,
    fontface = "bold",
    size = 3,
    vjust = 4.2
  ) +
  facet_grid(. ~ Season) +
  scale_x_continuous(
    limits = c(0.7, 1.3),
    breaks = NULL,
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    limits = c(0, 1.05),
    breaks = c(0, 0.5, 1),
    expand = c(0, 0)
  ) +
  coord_cartesian(clip = "off") +
  labs(x = NULL, y = NULL, title = NULL) +
  theme_classic(base_size = 11) +
  theme(
    strip.background = element_blank(),
    strip.text = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.line = element_blank(),
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.6),
    panel.grid = element_blank(),
    legend.position = "none",
    plot.margin = margin(3, 2, 50, 0)
  )


# ---- Right plot: full season mean + range across gardens ----
full_summary <- full_bar %>%
  summarise(
    mean_r = mean(Procrustes_r, na.rm = TRUE),
    min_r  = min(Procrustes_r, na.rm = TRUE),
    max_r  = max(Procrustes_r, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(full_x = 1)

full_pheno_plot <- ggplot(full_summary) +
  geom_col(
    aes(x = full_x, y = mean_r),
    width = 0.18,
    fill = "grey70",
    colour = "white",
    linewidth = 0.25,
    alpha = 0.7
  ) +
  geom_errorbar(
    aes(x = full_x, ymin = min_r, ymax = max_r),
    width = 0.08,
    linewidth = 0.45,
    colour = "black"
  ) +
  geom_point(
    aes(x = full_x, y = mean_r),
    shape = 21,
    fill = "grey40",
    colour = "white",
    size = 4,
    stroke = 0.45
  ) +
  annotation_custom(
    grob = textGrob(
      "Full season",
      y = label_y_pos,
      gp = gpar(fontsize = 9, fontface = "bold")
    ),
    xmin = 1,
    xmax = 1,
    ymin = -Inf,
    ymax = -Inf
  ) +
  scale_x_continuous(
    limits = c(0.7, 1.3),
    breaks = NULL,
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    limits = c(0, 1.05),
    breaks = c(0, 0.5, 1),
    expand = c(0, 0)
  ) +
  coord_cartesian(clip = "off") +
  labs(x = NULL, y = NULL, title = NULL) +
  theme_classic(base_size = 11) +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.line = element_blank(),
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.6),
    panel.grid = element_blank(),
    legend.position = "none",
    plot.margin = margin(3, 2, 50, 0)
  )


# ---- Combine ----
panel_pheno_combined <- main_pheno_plot + season_pheno_plot + full_pheno_plot +
  plot_layout(
    ncol = 3,
    widths = c(5.5, 1.2, 0.4),
    guides = "collect"
  )

panel_pheno_combined