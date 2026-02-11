
library(tidyverse)

# Load data
data_model_full <- readRDS("Data/Working_files/data_models_full_season_WITH_REGULAR_traits.rds")

# Extract data for a given garden

Botanical_garden_i <- "Leipzig"
data_model_garden <- data_model_full %>% # filter(Interactions > 0) %>%
  dplyr::filter(Botanical_garden == Botanical_garden_i)

# Base model 1------------------------------------------------------------------

n <- unique(data_model_garden$Plant) %>% length() # number of plants
m <- unique(data_model_garden$Pollinator) %>% length() # number of pollinators
N <- sum(data_model_garden$Interactions)  # total interactions

# Baseline 1 prediction
data_model_garden$int_pred_base_1 <- N / (n * m)

# Sanity check
N == round( sum(data_model_garden$int_pred_base_1),0)

# Model performance
# RMSE
rmse_b1 <- sqrt(mean((data_model_garden$Interactions - data_model_garden_B2$int_pred_base_1)^2))

# R2 (coefficient of determination)
r2_b1 <- 1 - sum((data_model_garden$Interactions - data_model_garden_B2$int_pred_base_1)^2) /
  sum((data_model_garden_B2$Interactions - mean(data_model_garden_B2$int_pred_base_1))^2)

rmse_b1
r2_b1


# Base model 2------------------------------------------------------------------
plant_int_marginals <- data_model_garden %>% group_by(Plant) %>%
  count(wt = Interactions) %>% rename(Plant_marginal = n)
pollinator_int_marginals <- data_model_garden %>% group_by(Pollinator) %>%
  count(wt = Interactions) %>% rename(Pollinator_marginal = n)

data_model_garden_B2 <- data_model_garden %>%
  left_join(plant_int_marginals, by = "Plant") %>%
  left_join(pollinator_int_marginals, by = "Pollinator")

data_model_garden_B2$int_pred_base_2 <- N * (data_model_garden_B2$Plant_marginal * data_model_garden_B2$Pollinator_marginal/N/N)

# Sanity check
N == round(sum(data_model_garden_B2$int_pred_base_2),0)

# Model performance
# RMSE
rmse_b2 <- sqrt(mean((data_model_garden_B2$Interactions - data_model_garden_B2$int_pred_base_2)^2))

# R2 (coefficient of determination)
r2_b2 <- 1 - sum((data_model_garden_B2$Interactions - data_model_garden_B2$int_pred_base_2)^2) /
  sum((data_model_garden_B2$Interactions - mean(data_model_garden_B2$int_pred_base_2))^2)

rmse_b2
r2_b2

# Base model 2 with external abundances-----------------------------------------
plant_total_abundance <- data_model_garden %>% 
  dplyr::select(Plant, Plant_abundance) %>% unique() %>%
  dplyr::select(Plant_abundance) %>% pull() %>% sum()

pollinator_total_abundance <- data_model_garden %>% 
  dplyr::select(Pollinator, Pollinator_abundance) %>% unique() %>%
  dplyr::select(Pollinator_abundance) %>% pull() %>% sum()

data_model_garden_B2$int_pred_base_2_ab <- N * (data_model_garden_B2$Plant_abundance * 
                                                  data_model_garden_B2$Pollinator_abundance/
                                                  plant_total_abundance / pollinator_total_abundance)

# Sanity check
N == round(sum(data_model_garden_B2$int_pred_base_2_ab),0)

# Model performance
# RMSE
rmse_b2_ab <- sqrt(mean((data_model_garden_B2$Interactions - data_model_garden_B2$int_pred_base_2_ab)^2))

# R2 (coefficient of determination)
r2_b2_ab <- 1 - sum((data_model_garden_B2$Interactions - data_model_garden_B2$int_pred_base_2_ab)^2) /
  sum((data_model_garden_B2$Interactions - mean(data_model_garden_B2$int_pred_base_2_ab))^2)

rmse_b2_ab
r2_b2_ab
