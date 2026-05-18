library(ggplot2)
library(dplyr)
library(tidyr)
library(tibble)

# ---- Prepare SES dataframe ----

SES_df <- as.data.frame(SES) %>%
  rownames_to_column("Plant_family") %>%
  pivot_longer(
    -Plant_family,
    names_to = "Pollinator_genus",
    values_to = "z_score"
  ) %>%
  filter(!is.na(z_score)) %>%
  mutate(
    infra_over_represented = case_when(
      z_score < -critical_value ~ "Under-represented",
      z_score >  critical_value ~ "Over-represented",
      TRUE ~ "No statistical difference"
    ),
    infra_over_represented = factor(
      infra_over_represented,
      levels = c(
        "Under-represented",
        "No statistical difference",
        "Over-represented"
      )
    )
  )

# ---- Plot ----

p_ses <- ggplot(SES_df, aes(x = z_score)) +
  
  geom_histogram(
    aes(fill = infra_over_represented),
    bins = 1800,
    alpha = 0.75,
    colour = "white",
    linewidth = 0.15,
    position = "identity"
  ) +
  
  geom_vline(
    xintercept = c(-critical_value, critical_value),
    linetype = "longdash",
    colour = "grey35",
    linewidth = 0.65
  ) +
  
  geom_vline(
    xintercept = 0,
    linetype = "solid",
    colour = "#3B3B3B",
    linewidth = 0.5,
    alpha = 0.7
  ) +
  
  scale_fill_manual(
    name = "Observed against null",
    values = c(
      "Under-represented" = "#F1A340",
      "No statistical difference" = "grey85",
      "Over-represented" = "#7B3294"
    ),
    labels = c(
      "Under-represented" = "Less frequent than expected",
      "No statistical difference" = "No difference",
      "Over-represented" = "More frequent than expected"
    )
  ) +
  
  coord_cartesian(
    xlim = c(-6, 6),
    expand = FALSE
  ) +
  
  labs(
    x = "Standardized effect size (SES)",
    y = "Number of interaction pairs"
  ) +
  
  theme_bw(base_size = 11) +
  
  theme(
    panel.grid = element_blank(),
    
    panel.border = element_rect(
      colour = "grey35",
      fill = NA,
      linewidth = 1
    ),
    
    legend.position = "bottom",
    
    legend.title = element_text(
      face = "bold",
      size = 11
    ),
    
    legend.text = element_text(
      size = 10
    ),
    
    axis.title = element_text(
      face = "bold",
      size = 14
    ),
    
    axis.text = element_text(
      size = 12,
      colour = "black"
    ),
    
    plot.margin = margin(5, 5, 5, 5)
  )

p_ses