#-----------------------------
# Random Forest
#-----------------------------

# Logic: After GAMS, we are going to explore variable importance
# and predictive power with more flexible alternatives than GAM

# Load libraries
library(dplyr)
library(ranger)
library(fastshap)
library(tibble)
library(ggplot2)
library(Metrics)
#-----------------------------
# Load data
#-----------------------------
rf_dat = readRDS("Data/Working_files/rf_dat.rds")


set.seed(123)

rf_explore <- rf_dat %>%
  mutate(
    Botanical_garden = as.factor(Botanical_garden),
    random_noise = runif(n())
  )

rf_model_explore <- ranger(
  VisitRate ~ .,
  data = rf_explore,
  num.trees = 1500,
  importance = "permutation",
  seed = 123
)

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
