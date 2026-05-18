library(ggplot2)
library(dplyr)
library(scales)

# ---- Select strongest associations using RAW SES ----

pref_df_top <- pref_df %>%
  arrange(desc(weight)) %>%
  slice_head(n = 40) %>%
  mutate(
    weight_plot = rescale(
      pmin(weight, quantile(weight, 0.95)),
      to = c(1, 6)
    )
  )

# ---- Order families ----

plant_order <- pref_df_top %>%
  group_by(from) %>%
  summarise(total_weight = sum(weight), .groups = "drop") %>%
  arrange(desc(total_weight)) %>%
  pull(from)

poll_order <- pref_df_top %>%
  group_by(to) %>%
  summarise(total_weight = sum(weight), .groups = "drop") %>%
  arrange(desc(total_weight)) %>%
  pull(to)

# ---- Prepare dataframe ----

pref_dot <- pref_df_top %>%
  mutate(
    Plant_family = factor(from, levels = rev(plant_order)),
    Pollinator_family = factor(to, levels = poll_order)
  )

# ---- Plot ----

p2 <- ggplot(
  pref_dot,
  aes(
    x = Pollinator_family,
    y = Plant_family
  )
) +
  geom_hline(
    yintercept = seq_along(levels(pref_dot$Plant_family)),
    colour = "grey92",
    linewidth = 0.3
  ) +
  
  geom_vline(
    xintercept = seq_along(levels(pref_dot$Pollinator_family)),
    colour = "grey94",
    linewidth = 0.3
  ) +
  
  geom_point(
    aes(size = weight_plot),
    colour = "#7B3294",
    alpha = 0.85
  ) +
  
  scale_size_area(
    name = "|SES|",
    max_size = 6,
    breaks = c(2, 4, 6),
    labels = c("2", "4", "6")
  ) +
  
  labs(
    x = "Pollinator family",
    y = "Plant family"
  ) +
  
  coord_fixed(ratio = 1) +
  
  theme_classic(base_size = 11) +
  
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1,
      vjust = 1,
      size = 9
    ),
    
    axis.text.y = element_text(size = 9),
    
    axis.title = element_text(face = "bold"),
    
    legend.position = "right",
    
    panel.border = element_rect(
      colour = "grey30",
      fill = NA,
      linewidth = 0.4
    ),
    
    axis.ticks = element_blank()
  )

p2