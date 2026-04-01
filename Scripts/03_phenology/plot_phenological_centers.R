library(dplyr)
library(ggplot2)
library(ggdist)

# Read processed data
pollinator_centers <- readRDS("Data/Working_files/pollinator_pheno_centers.rds")
plant_centers      <- readRDS("Data/Working_files/plant_pheno_centers.rds")

#--------------------------------------------------
# Combine plant (by garden) + pollinator centers
#--------------------------------------------------

plot_data <- bind_rows(
  plant_centers %>%
    transmute(Group = paste(Garden, "plants"), center),
  pollinator_centers %>%
    transmute(Group = "Pollinators", center)
) %>%
  mutate(
    Group = factor(
      Group,
      levels = c("Pollinators", "Jena plants", "Halle plants", "Leipzig plants")
    )
  )

#--------------------------------------------------
# Compute summaries (median)
#--------------------------------------------------

plot_summary <- plot_data %>%
  group_by(Group) %>%
  summarise(
    q50 = quantile(center, 0.50, na.rm = TRUE),
    .groups = "drop"
  )

#--------------------------------------------------
# Plot
#--------------------------------------------------

library(dplyr)
library(ggplot2)
library(ggdist)

# Read processed data
pollinator_centers <- readRDS("Data/Working_files/pollinator_pheno_centers.rds")
plant_centers      <- readRDS("Data/Working_files/plant_pheno_centers.rds")

# Combine data
plot_data <- bind_rows(
  plant_centers %>%
    transmute(Group = paste(Garden, "plants"), center),
  pollinator_centers %>%
    transmute(Group = "Pollinators", center)
) %>%
  mutate(
    Group = factor(
      Group,
      levels = c("Pollinators", "Jena plants", "Halle plants", "Leipzig plants")
    )
  )

# Median per group
plot_summary <- plot_data %>%
  group_by(Group) %>%
  summarise(
    q50 = quantile(center, 0.50, na.rm = TRUE),
    .groups = "drop"
  )

# Define colors
my_colors <- c(
  "Pollinators"   = "grey70",
  "Jena plants"   = "#B12A90FF",
  "Halle plants"  = "#0D0887FF",
  "Leipzig plants"= "#FCA636FF"
)

# Plot
ggplot(plot_data, aes(x = Group, y = center, fill = Group)) +
  ggdist::stat_halfeye(
    adjust = 0.5,
    width = 0.6,
    .width = 0,
    justification = -0.2,
    point_colour = NA,
    alpha = 0.5
  ) +
  geom_boxplot(
    width = 0.15,
    outlier.shape = NA,
    alpha = 0.6
  ) +
  geom_point(
    shape = 95,
    size = 6,
    alpha = 0.2
  ) +
  geom_point(
    data = plot_summary,
    aes(x = Group, y = q50),
    inherit.aes = FALSE,
    size = 3,
    color = "black"
  ) +
  scale_fill_manual(values = my_colors) +
  coord_cartesian(clip = "off") +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.title.x = element_blank()
  ) +
  labs(
    y = "Phenological center (DOY)")