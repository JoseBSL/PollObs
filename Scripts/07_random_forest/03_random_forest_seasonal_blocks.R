################################################################################
# Random Forest main variables//Seasonal blocks
################################################################################
# Load libraries
################################################################################
library(dplyr)
library(ranger)
library(fastshap)
library(tibble)
library(ggplot2)
library(Metrics)
library(rsample)
library(purrr)
################################################################################
# Load data
################################################################################
weekly_data = readRDS("Data/Working_files/weekly_data_for_modelling.rds")
################################################################################
# Prepare data
rf_dat = weekly_data %>%
  select(
    VisitRate,
    Botanical_garden,
    Week,
    Mean_Temperature,
    Floral_abundance,
    Total_pollinator_abundance,
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
  select(-c(VisitRate))

rf_dat = rf_dat %>%
  group_by(Botanical_garden) %>%
  arrange(Week, .by_group = TRUE) %>%
  mutate(
    Season_group = ntile(Week, 3),
    Season = case_when(
      Season_group == 1 ~ "Early",
      Season_group == 2 ~ "Mid",
      Season_group == 3 ~ "Late"
    ),
    Season = factor(Season, levels = c("Early","Mid","Late"))
  ) %>%
  select(-Season_group) %>%
  ungroup()

################################################################################
predict_rf <- function(object, newdata) {
  predict(object, data = newdata)$predictions
}

# Split data by season
season_data <- rf_dat %>%
  group_split(Season) %>%
  set_names(levels(rf_dat$Season))


# Fit one RF per season
set.seed(123)

rf_models_season <- season_data %>%
  map(~ ranger(
    log_visit ~ . - Season,
    data = .x,
    num.trees = 1500,
    importance = "permutation",
    seed = 123
  ))


set.seed(123)

shap_imp_season <- map2_df(
  rf_models_season,
  names(rf_models_season),
  function(model, season_name) {
    
    # extract the matching seasonal dataset
    dat_season <- season_data[[season_name]]
    
    # keep exactly the predictors used by the model
    predictors <- dat_season %>%
      select(all_of(model$forest$independent.variable.names))
    
    # compute SHAP values
    shap_values <- fastshap::explain(
      object = model,
      X = predictors,
      pred_wrapper = predict_rf,
      nsim = 100
    )
    
    # summarise global SHAP importance
    mean_shap <- colMeans(abs(shap_values))
    
    tibble(
      variable = names(mean_shap),
      mean_abs_shap = as.numeric(mean_shap),
      Season = season_name
    )
  }
)

# Relative SHAP importance within season
shap_imp_season = shap_imp_season %>%
  group_by(Season) %>%
  mutate(
    importance_rel = mean_abs_shap / sum(mean_abs_shap)
  ) %>%
  ungroup() %>%
  mutate(
    Season = factor(Season, levels = c("Early", "Mid", "Late")))

# Plot relative SHAP importance
ggplot(shap_imp_season,
       aes(x = variable, y = importance_rel, fill = Season)) +
  geom_col() +
  facet_wrap(~Season, scales = "fixed") +
  coord_flip() +
  theme_classic() +
  labs(
    x = NULL,
    y = "Relative SHAP importance" )
