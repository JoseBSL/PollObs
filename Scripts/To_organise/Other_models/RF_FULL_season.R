
library(ranger)
library(ggplot2)

# Load data
data_model_full <- readRDS("Data/Working_files/data_models_full_season_WITH_REGULAR_traits.rds")

# Extract data for a given garden

Botanical_garden_i <- "Leipzig"
data_model_garden <- data_model_full %>% # filter(Interactions > 0) %>%
  dplyr::filter(Botanical_garden == Botanical_garden_i)

# Make sure variables are scaled exactly as in your glmmTMB model
data_rf <- data_model_garden
# data_rf$Plant_abundance <- scale(data_rf$Plant_abundance)
# data_rf$Pollinator_abundance <- scale(data_rf$Pollinator_abundance)
# data_rf$diff_real_traits <- scale(data_rf$diff_real_traits)
# data_rf$diff_latent_traits <- scale(data_rf$diff_latent_traits)

# Fit regression forest
rf_model <- ranger(
  Interactions ~ Plant_abundance * Pollinator_abundance +
    diff_real_traits +
    diff_latent_traits,
  data = data_rf,
  num.trees = 500,
  importance = "permutation",  # or "impurity"
  seed = 123
)

rf_model

# R2 and RMSE on out-of-bag predictions
rf_model$r.squared
sqrt(rf_model$prediction.error)


# Variable importance plots
vi <- data.frame(
  Variable = names(rf_model$variable.importance),
  Importance = rf_model$variable.importance
)

ggplot(vi, aes(x = reorder(Variable, Importance), y = Importance)) +
  geom_col() +
  coord_flip() +
  theme_bw()
