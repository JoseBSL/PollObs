library(xgboost)
library(caret)   # for train() and cross-validation
library(Metrics) # rmse()
library(dplyr)

# Load data
data_model_full <- readRDS("Data/Working_files/data_models_full_season_WITH_REGULAR_traits.rds")

# Extract data for a given garden

Botanical_garden_i <- "Leipzig"
data_model_garden <- data_model_full %>% # filter(Interactions > 0) %>%
  dplyr::filter(Botanical_garden == Botanical_garden_i)

# Prepare data: turn predictors into a matrix (xgboost needs numeric matrix)
X <- data_model_garden %>%
  dplyr::select(Plant_abundance, Pollinator_abundance,
         diff_real_traits, diff_latent_traits) %>%
  as.matrix()

y <- data_model_garden$Interactions  # response variable (counts)

# Combine into a caret training frame
train_data <- data.frame(y = y, X)

# Define hyperparameter grid
xgb_grid <- expand.grid(
  nrounds = c(200, 1200, 2000),
  max_depth = c(1, 2, 3, 5),
  eta = c(0.01, 0.001, 0.0001),
  gamma = c(0, 1, 10),
  colsample_bytree = 1,      # defaults
  min_child_weight = 1,
  subsample = 1
)

# Train control: k-fold CV
train_control <- trainControl(
  method = "cv",
  number = 5,
  verboseIter = TRUE
)

# Fit model with Poisson objective
set.seed(123)
xgb_model <- train(
  y ~ .,
  data = train_data,
  method = "xgbTree",
  trControl = train_control,
  tuneGrid = xgb_grid,
  verbose = TRUE,
  objective = "count:poisson",
  eval_metric = "poisson-nloglik"
)

xgb_model

# Best model performance--------------------------------------------------------
# Best hyperparameters
best_params <- xgb_model$bestTune
print(best_params)

# Predictions on the training set (CV already gives you approx. out-of-sample performance)
preds <- predict(xgb_model, newdata = data_model_garden)

# Compute RMSE and R² on these predictions
rmse_val <- rmse(data_model_garden$Interactions, preds)
r2_val <- R2(preds, data_model_garden$Interactions)  # caret's R2

cat("Approximate RMSE:", round(rmse_val, 3), "\n")
cat("Approximate R²:", round(r2_val, 3), "\n")

# Variable importance from cross-validated model
vip_cv <- varImp(xgb_model, scale = TRUE)
plot(vip_cv, main = "Variable Importance (CV model)")


# Another take on performance close to out-of-bag predictions------------------
set.seed(123)  # for reproducibility

# 1. Split into training (80%) and testing (20%)
train_index <- createDataPartition(data_model_garden$Interactions, p = 0.8, list = FALSE)
train_data <- data_model_garden[train_index, ]
test_data  <- data_model_garden[-train_index, ]

# 2. Train with bestTune parameters from your CV
best_params <- xgb_model$bestTune
print(best_params)

ctrl <- trainControl(method = "none")  # no CV, just fit once

final_model <- train(
  Interactions ~ scale(Plant_abundance) * scale(Pollinator_abundance) +
    scale(diff_real_traits) + scale(diff_latent_traits),
  data = train_data,
  method = "xgbTree",
  trControl = ctrl,
  tuneGrid = best_params
)

# 3. Predict on the held-out test set
preds_test <- predict(final_model, newdata = test_data)

# 4. Compute realistic generalization metrics
rmse_test <- rmse(test_data$Interactions, preds_test)
r2_test   <- R2(preds_test, test_data$Interactions)  # caret's R²

cat("Generalization RMSE (test set):", round(rmse_test, 3), "\n")
cat("Generalization R² (test set):", round(r2_test, 3), "\n")

# Variable importance from final retrained model
vip_final <- varImp(final_model, scale = TRUE)
plot(vip_final, main = "Variable Importance (Final retrained model)")
