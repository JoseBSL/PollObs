################################################################################
# Continuous SHAP importance along season
################################################################################

library(dplyr)
library(ranger)
library(purrr)
library(tibble)
library(ggplot2)
library(fastshap)

################################################################################
# Load and prepare data
################################################################################

rf_cont = weekly_data %>%
  select(
    VisitRate,
    Botanical_garden,
    Week,
    Mean_Temperature,
    Floral_abundance,
    Total_pollinator_abundance,
    Mean_nectar_volume,
    t_plant,
    T_gauss,
    Overlap_days
  ) %>%
  select(-Mean_Temperature) %>%
  rename(Flower_width = t_plant) %>%
  mutate(
    log_visit = log1p(VisitRate)
  ) %>%
  tidyr::drop_na() %>%
  select(-c(VisitRate, Botanical_garden))

################################################################################
# Sliding windows
################################################################################

week_min <- min(rf_cont$Week, na.rm = TRUE)
week_max <- max(rf_cont$Week, na.rm = TRUE)

window_size <- 5
step_size <- 1

window_starts <- seq(
  from = week_min,
  to = week_max - window_size + 1,
  by = step_size
)

################################################################################
# Prediction wrapper for fastshap
################################################################################

predict_rf <- function(object, newdata) {
  predict(object, data = newdata)$predictions
}

################################################################################
# Fit RF + compute SHAP in each window
################################################################################

rf_windows <- map_df(window_starts, function(w0) {
  
  w1 <- w0 + window_size - 1
  
  dat_win <- rf_cont %>%
    filter(Week >= w0, Week <= w1)
  
  if (nrow(dat_win) < 80) return(NULL)
  
  # Fit RF
  model_win <- ranger(
    log_visit ~ . - Week,
    data = dat_win,
    num.trees = 1000,
    seed = 123
  )
  
  # Extract predictors
  predictors <- dat_win %>%
    select(all_of(model_win$forest$independent.variable.names))
  
  # Compute SHAP values
  shap_values <- fastshap::explain(
    object = model_win,
    X = predictors,
    pred_wrapper = predict_rf,
    nsim = 100
  )
  
  # Global SHAP importance
  mean_shap <- colMeans(abs(shap_values))
  
  tibble(
    Week_start = w0,
    Week_end = w1,
    Week_center = mean(c(w0, w1)),
    variable = names(mean_shap),
    importance = as.numeric(mean_shap),
    n = nrow(dat_win)
  )
})

################################################################################
# Relative importance by window
################################################################################

rf_windows <- rf_windows %>%
  group_by(Week_center) %>%
  mutate(
    importance_rel = importance / sum(importance)
  ) %>%
  ungroup()

################################################################################
# Select main variables
################################################################################

rf_stack <- rf_windows %>%
  filter(variable %in% c(
    "Total_pollinator_abundance",
    "Floral_abundance",
    "T_gauss",
    "Flower_width",
    "Overlap_days",
    "Mean_nectar_volume"
  ))

################################################################################
# Stacked SHAP importance plot
################################################################################

rf_stack = rf_stack %>%
  mutate(
    variable = recode(
      variable,
      Total_pollinator_abundance = "Pollinator abundance",
      Floral_abundance = "Flower abundance",
      T_gauss = "Trait matching",
      Flower_width = "Flower width",
      Overlap_days = "Phenology overlap",
      Mean_nectar_volume = "Nectar volume"))  

#Save data
saveRDS(rf_stack, "Data/Working_files/rf_stack.rds")


ggplot(rf_stack,
       aes(x = Week_center,
           y = importance_rel,
           fill = variable)) +
  geom_area(alpha = 0.95) +
  scale_fill_manual(values = c(
    "Pollinator abundance" = "#E64B35",
    "Flower abundance" = "#F39B7F",
    "Trait matching" = "#00A087",
    "Flower width" = "#4DBBD5",
    "Phenology overlap" = "grey49",
    "Nectar volume" = "#8491B4"
   
  )) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_classic(base_size = 14) +
  labs(
    x = "Week",
    y = "Relative SHAP importance",
    fill = "Driver"
  )
