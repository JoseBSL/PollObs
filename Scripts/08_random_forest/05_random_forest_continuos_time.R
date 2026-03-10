#==================================================
# Continuous Random Forest importance along season
#==================================================

#-----------------------------
# Load libraries
#-----------------------------
library(dplyr)
library(ranger)
library(purrr)
library(tibble)
library(ggplot2)

#-----------------------------
# Load and prepare data
#-----------------------------
rf_dat <- readRDS("Data/Working_files/rf_dat.rds")

rf_cont <- rf_dat %>%
  select(!Mean_Temperature) %>%
  rename(Flower_width = t_plant) %>%
  mutate(
    log_visit = log1p(VisitRate)
  ) %>%
  select(-VisitRate, -Botanical_garden)

#-----------------------------
# Define sliding windows
#-----------------------------
week_min <- min(rf_cont$Week, na.rm = TRUE)
week_max <- max(rf_cont$Week, na.rm = TRUE)

window_size <- 5
step_size <- 1

window_starts <- seq(
  from = week_min,
  to = week_max - window_size + 1,
  by = step_size
)

#-----------------------------
# Check number of observations per window
#-----------------------------
window_sizes <- map_df(window_starts, function(w0) {
  
  w1 <- w0 + window_size - 1
  
  tibble(
    Week_start = w0,
    Week_end = w1,
    Week_center = mean(c(w0, w1)),
    n = rf_cont %>%
      filter(Week >= w0, Week <= w1) %>%
      nrow()
  )
})

print(window_sizes)

ggplot(window_sizes, aes(x = Week_center, y = n)) +
  geom_line() +
  geom_point() +
  theme_classic(base_size = 14) +
  labs(
    x = "Week (window centre)",
    y = "Number of observations per window"
  )

#-----------------------------
# Fit one RF per window
# IMPORTANT:
# - Week is excluded as predictor
# - Botanical_garden has already been removed
#-----------------------------
rf_windows <- map_df(window_starts, function(w0) {
  
  w1 <- w0 + window_size - 1
  
  dat_win <- rf_cont %>%
    filter(Week >= w0, Week <= w1)
  
  # Avoid unstable windows
  if (nrow(dat_win) < 80) {
    return(NULL)
  }
  
  model_win <- ranger(
    log_visit ~ . - Week,
    data = dat_win,
    num.trees = 1000,
    importance = "permutation",
    seed = 123
  )
  
  tibble(
    Week_start = w0,
    Week_end = w1,
    Week_center = mean(c(w0, w1)),
    variable = names(model_win$variable.importance),
    importance = as.numeric(model_win$variable.importance),
    n = nrow(dat_win)
  )
})

#-----------------------------
# Relative importance by window
#-----------------------------
rf_windows <- rf_windows %>%
  group_by(Week_center) %>%
  mutate(
    importance_rel = importance / sum(importance)
  ) %>%
  ungroup()

#-----------------------------
# Select main variables for plotting
#-----------------------------
top_vars <- c(
  "Total_pollinator_abundance",
  "Floral_abundance",
  "T_gauss",
  "Flower_width"
)

rf_windows_plot <- rf_windows %>%
  filter(variable %in% top_vars)

#-----------------------------
# Plot 1: raw lines
#-----------------------------
ggplot(rf_windows_plot,
       aes(x = Week_center,
           y = importance_rel,
           colour = variable)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2) +
  scale_colour_manual(values = c(
    "Total_pollinator_abundance" = "#E64B35",
    "Floral_abundance" = "#4DBBD5",
    "T_gauss" = "#00A087",
    "Flower_width" = "#3C5488"
  )) +
  theme_classic(base_size = 14) +
  labs(
    x = "Week",
    y = "Relative variable importance",
    colour = "Driver"
  )

#-----------------------------
# Plot 2: smoothed version (recommended)
#-----------------------------
ggplot(rf_windows_plot,
       aes(x = Week_center,
           y = importance_rel,
           colour = variable)) +
  geom_point(size = 1.8, alpha = 0.7) +
  geom_smooth(
    method = "loess",
    span = 0.4,
    se = FALSE,
    linewidth = 1.2
  ) +
  scale_colour_manual(values = c(
    "Total_pollinator_abundance" = "#E64B35",
    "Floral_abundance" = "#4DBBD5",
    "T_gauss" = "#00A087",
    "Flower_width" = "#3C5488"
  )) +
  theme_classic(base_size = 14) +
  labs(
    x = "Week",
    y = "Relative variable importance",
    colour = "Driver"
  )











#-----------------------------
# Stacked importance plot
#-----------------------------

rf_stack <- rf_windows %>%
  filter(variable %in% c(
    "Total_pollinator_abundance",
    "Floral_abundance",
    "T_gauss",
    "Flower_width"
  )) %>%
  mutate(
    driver_type = case_when(
      variable %in% c("Total_pollinator_abundance", "Floral_abundance") ~ "Abundance",
      variable %in% c("T_gauss", "Flower_width") ~ "Traits"
    )
  )
ggplot(rf_stack,
       aes(x = Week_center,
           y = importance_rel,
           fill = variable)) +
  geom_area(alpha = 0.9) +
  scale_fill_manual(values = c(
    "Total_pollinator_abundance" = "#E64B35",
    "Floral_abundance" = "#F39B7F",
    "T_gauss" = "#00A087",
    "Flower_width" = "#4DBBD5"
  )) +
  theme_classic(base_size = 14) +
  labs(
    x = "Week",
    y = "Relative importance",
    fill = "Driver"
  )
