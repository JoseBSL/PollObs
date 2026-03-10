#==================================================
# Random Forest using log-transformed response
#==================================================

# Logic:
# After GAMs, use Random Forest to explore variable importance
# and predictive performance with a flexible model.
# Response is log-transformed from the start:
#   log_visit = log1p(VisitRate)

#-----------------------------
# Load libraries
#-----------------------------
library(dplyr)
library(ranger)
library(fastshap)
library(tibble)
library(ggplot2)
library(Metrics)
library(rsample)

#-----------------------------
# Load and prepare data
#-----------------------------
rf_dat <- readRDS("Data/Working_files/rf_dat.rds")

rf_dat <- rf_dat %>%
  select(!Mean_Temperature) %>%
  rename(Flower_width = t_plant) %>%
  mutate(
    Botanical_garden = as.factor(Botanical_garden),
    log_visit = log1p(VisitRate)
  ) %>%
  select(-c(VisitRate, Botanical_garden, Week))

#==================================================
# PART A. EXPLORATORY RANDOM FOREST
#==================================================

#-----------------------------
# Fit exploratory RF
#-----------------------------
set.seed(123)

rf_explore <- rf_dat

rf_model_explore <- ranger(
  log_visit ~ .,
  data = rf_explore,
  num.trees = 1500,
  importance = "permutation",
  seed = 123
)

#-----------------------------
# SHAP values
#-----------------------------
pred_fun <- function(object, newdata) {
  predict(object, data = newdata)$predictions
}

X <- rf_explore %>%
  select(all_of(rf_model_explore$forest$independent.variable.names))

set.seed(123)

shap_values <- fastshap::explain(
  object = rf_model_explore,
  X = X,
  pred_wrapper = pred_fun,
  nsim = 100
)

#-----------------------------
# Global SHAP importance
#-----------------------------
shap_imp <- tibble(
  variable = names(colMeans(abs(shap_values))),
  mean_abs_shap = colMeans(abs(shap_values))
) %>%
  arrange(desc(mean_abs_shap))

print(shap_imp)

ggplot(shap_imp, aes(x = reorder(variable, mean_abs_shap), y = mean_abs_shap)) +
  geom_col() +
  coord_flip() +
  theme_classic() +
  labs(x = NULL, y = "Mean |SHAP value|")

#==================================================
# PART B. TEST PREDICTION
#==================================================

#-----------------------------
# Train/test split
#-----------------------------
set.seed(123)

split <- initial_split(rf_dat, prop = 0.8)

train_dat <- training(split)
test_dat  <- testing(split)

#-----------------------------
# Fit predictive RF
#-----------------------------
rf_model <- ranger(
  log_visit ~ .,
  data = train_dat,
  num.trees = 1500,
  importance = "permutation",
  seed = 123
)

#-----------------------------
# Predict on test data
#-----------------------------
pred_test <- predict(rf_model, data = test_dat)$predictions

#-----------------------------
# Predictive performance
#-----------------------------
r2_rf   <- cor(test_dat$log_visit, pred_test)^2
rmse_rf <- rmse(test_dat$log_visit, pred_test)
mae_rf  <- mae(test_dat$log_visit, pred_test)

gof_rf <- tibble(
  Model = "Random Forest",
  R2 = r2_rf,
  RMSE = rmse_rf,
  MAE = mae_rf
)

print(gof_rf)


