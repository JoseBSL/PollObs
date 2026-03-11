################################################################################
# Random Forest
################################################################################
# Logic: After GAMS, we are going to explore variable importance
# and predictive power with more flexible alternatives than GAM

# Load libraries
library(dplyr)
library(ranger)
library(fastshap)
library(tibble)
library(ggplot2)
library(Metrics)
################################################################################
# Load data
################################################################################
weekly_data = readRDS("Data/Working_files/weekly_data_for_modelling.rds")

################################################################################
# Prepare data
################################################################################
rf_dat <- weekly_data %>%
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
  mutate(
    Botanical_garden = factor(Botanical_garden)
  ) %>%
  na.omit()

################################################################################
# Explore random forest
################################################################################
set.seed(123)
# Add random variable to check how well it performs
rf_explore = rf_dat %>%
  mutate(
    random_noise = runif(n()))
# Run model with all predictors
rf_model_explore = ranger(
  VisitRate ~ .,
  data = rf_explore,
  num.trees = 1500,
  importance = "permutation",
  seed = 123
)

################################################################################
# Calculate variable importance (shap values)
################################################################################
pred_fun = function(object, newdata) {
  predict(object, data = newdata)$predictions
}

# Obtain rf predictors
predictors = rf_explore %>%
  select(all_of(rf_model_explore$forest$independent.variable.names))

# Compute SHAP values
# It works similar to the AIC comparison conducted with GAM
shap_values = fastshap::explain(
  object = rf_model_explore,
  X = predictors,
  pred_wrapper = pred_fun,
  nsim = 100)

# Summarise global SHAP importance
shap_importance = tibble(
  variable = names(colMeans(abs(shap_values))),
  mean_abs_shap = colMeans(abs(shap_values))) %>%
  arrange(desc(mean_abs_shap))

shap_importance

# Plot Shap importance
ggplot(shap_importance, aes(x = reorder(variable, mean_abs_shap),
                     y = mean_abs_shap)) +
  geom_col() +
  coord_flip() +
  theme_classic() +
  labs(
    x = NULL,
    y = "Mean |SHAP value|")