################################################################################
# Random Forest main variables
################################################################################

# Logic:
# After GAMs, use Random Forest to explore variable importance
# and predictive performance with a flexible model.
# Response is log-transformed from the start:
#   log_visit = log1p(VisitRate)

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
  select(-c(VisitRate, Botanical_garden, Week))
################################################################################
# Run Random Forest
set.seed(123)

rf_model = ranger(
  log_visit ~ .,
  data = rf_dat,
  num.trees = 1500,
  importance = "permutation",
  seed = 123)

################################################################################
# Calculate variable importance (shap values)
################################################################################
pred_fun = function(object, newdata) {
  predict(object, data = newdata)$predictions
}

# Obtain rf predictors
predictors = rf_dat %>%
  select(all_of(rf_model$forest$independent.variable.names))

# Compute SHAP values
# It works similar to the AIC comparison conducted with GAM
shap_values = fastshap::explain(
  object = rf_model,
  X = predictors,
  pred_wrapper = pred_fun,
  nsim = 100)

# Summarise global SHAP importance
shap_importance = tibble(
  variable = names(colMeans(abs(shap_values))),
  mean_abs_shap = colMeans(abs(shap_values))
) %>%
  arrange(desc(mean_abs_shap))

# Save data
saveRDS(shap_importance, "Data/Working_files/shap_importance.rds")


print(shap_importance)
# Plot variable importance
ggplot(shap_importance, aes(x = reorder(variable, mean_abs_shap), y = mean_abs_shap)) +
  geom_col() +
  coord_flip() +
  theme_classic() +
  labs(x = NULL, y = "Mean |SHAP value|")

################################################################################
# TEST PREDICTION
################################################################################

# Train/test split 80-20%
set.seed(123)
split = initial_split(rf_dat, prop = 0.8, strata = log_visit)
train_dat = training(split) # 80%
test_dat  = testing(split) # 20%
# Fit model on training data
rf_model = ranger(
  log_visit ~ .,
  data = train_dat,
  num.trees = 1500,
  importance = "permutation",
  seed = 123
)
# Predict on test data
pred_test = predict(rf_model, data = test_dat)$predictions
# Predictive performance
# Variance
r2_rf   = cor(test_dat$log_visit, pred_test)^2
# Spearman
spearman_rf = cor(test_dat$log_visit, pred_test, method = "spearman")
# Error magnitude
rmse_rf = rmse(test_dat$log_visit, pred_test)
# Absolute error
mae_rf  = mae(test_dat$log_visit, pred_test)

gof_rf = tibble(
  Model = "Random Forest",
  R2 = r2_rf,
  Spearman = spearman_rf,
  RMSE = rmse_rf,
  MAE = mae_rf)


gof_rf

# R2 (variance explained)
r2_rf = 1 - sum((test_dat$log_visit - pred_test)^2) / 
  sum((test_dat$log_visit - mean(test_dat$log_visit))^2)
