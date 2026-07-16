library(dplyr)
library(tidyr)
library(tibble)
library(ggplot2)

# Load data
raw_data <- readRDS(
  "Data/Working_files/interaction_data.rds"
)

SES_genus <- readRDS(
  "Data/Working_files/SES_genus.rds"
)

trait_pairs <- readRDS(
  "Data/Working_files/trait_pairs.rds"
)


# Plant-family lookup
plant_lookup <- raw_data %>%
  select(
    Plant_accepted_name,
    Plant_family
  ) %>%
  filter(
    !is.na(Plant_accepted_name),
    !is.na(Plant_family)
  ) %>%
  distinct()


# Pollinator-genus lookup
pollinator_lookup <- raw_data %>%
  select(
    Pollinator_accepted_name,
    Pollinator_genus
  ) %>%
  filter(
    !is.na(Pollinator_accepted_name),
    !is.na(Pollinator_genus),
    Pollinator_accepted_name != "None"
  ) %>%
  distinct()


# Add taxonomy to species-level trait pairs
trait_matching_genus <- trait_pairs %>%
  transmute(
    Plant_accepted_name = as.character(Plants),
    Pollinator_accepted_name = as.character(Pollinators),
    Trait_matching = as.numeric(T_gauss)
  ) %>%
  left_join(
    plant_lookup,
    by = "Plant_accepted_name"
  ) %>%
  left_join(
    pollinator_lookup,
    by = "Pollinator_accepted_name"
  ) %>%
  filter(
    !is.na(Plant_family),
    !is.na(Pollinator_genus),
    !is.na(Trait_matching)
  ) %>%
  group_by(
    Plant_family,
    Pollinator_genus
  ) %>%
  summarise(
    mean_trait_matching = mean(
      Trait_matching,
      na.rm = TRUE
    ),
    .groups = "drop"
  )


# Convert SES matrix to long format
SES_genus_long <- SES_genus %>%
  as.data.frame(
    check.names = FALSE
  ) %>%
  rownames_to_column(
    "Plant_family"
  ) %>%
  pivot_longer(
    cols = -Plant_family,
    names_to = "Pollinator_genus",
    values_to = "SES"
  )


# Join SES and trait matching
analysis_data <- SES_genus_long %>%
  left_join(
    trait_matching_genus,
    by = c(
      "Plant_family",
      "Pollinator_genus"
    )
  ) %>%
  filter(
    !is.na(SES),
    !is.na(mean_trait_matching),
    is.finite(SES),
    is.finite(mean_trait_matching)
  ) %>%
  mutate(
    Association = case_when(
      SES < -1.96 ~ "Avoidance",
      SES >  1.96 ~ "Preference",
      TRUE        ~ "Neutral"
    ),
    Association = factor(
      Association,
      levels = c(
        "Avoidance",
        "Neutral",
        "Preference"
      )
    )
  )


# Boxplot
plot_cols <- c(
  "Avoidance" = "#F1A340",
  "Neutral" = "grey85",
  "Preference" = "#7B3294"
)

ggplot(
  analysis_data,
  aes(
    x = Association,
    y = mean_trait_matching,
    fill = Association
  )
) +
  geom_boxplot(
    width = 0.62,
    outlier.shape = NA,
    linewidth = 0.8
  ) +
  geom_jitter(
    width = 0.13,
    height = 0,
    size = 1.7,
    alpha = 0.4,
    colour = "black"
  ) +
  scale_fill_manual(values = plot_cols) +
  labs(
    x = NULL,
    y = "Mean trait-matching probability"
  ) +
  theme_bw() +
  theme(
    legend.position = "none",
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 13, face = "bold"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  )