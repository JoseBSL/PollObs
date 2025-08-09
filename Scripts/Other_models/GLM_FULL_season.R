library(tidyverse)
library(performance)
library(glmmTMB)
library(MASS)
library(DHARMa)



data_model_full <- readRDS("Data/Working_files/data_models_full_season_WITH_REGULAR_traits.rds")

#Fit nb GLM

Botanical_garden_i <- "Jena"
data_model_garden <- data_model_full %>% 
  dplyr::filter(Botanical_garden == Botanical_garden_i)

# Check collinearity
car::vif(lm(Interactions ~ scale(Plant_abundance) *
              scale(Pollinator_abundance) +
              scale(diff_real_traits) +
              scale(diff_latent_traits),
            data = data_model_garden))

nb_model <- glmmTMB::glmmTMB(Interactions ~ scale(Plant_abundance) *
                               scale(Pollinator_abundance) +
                               scale(diff_real_traits) + scale(diff_latent_traits),
                             data = data_model_garden,
                             family = nbinom2,
                             zi = ~ scale(Plant_abundance) +
                               scale(Pollinator_abundance) +
                               scale(diff_real_traits) + scale(diff_latent_traits))
summary(nb_model)

pois_start <- coef(glm(Interactions ~ scale(Plant_abundance) *
                         scale(Pollinator_abundance) +
                         scale(diff_real_traits) +
                         scale(diff_latent_traits),
                       data = data_model_garden,
                       family = poisson))

nb_model2 <- glmmTMB(Interactions ~ scale(Plant_abundance) *
                       scale(Pollinator_abundance) +
                       scale(diff_real_traits) +
                       scale(diff_latent_traits),
                     family = nbinom2,
                     data = data_model_garden)
summary(nb_model2)

performance::check_collinearity(nb_model)
performance::check_collinearity(nb_model2)
performance::r2(nb_model)
performance::r2(nb_model2)

m_null <- update(nb_model, . ~ 1)  # intercept-only model
ll_full <- logLik(nb_model)
ll_null <- logLik(m_null)

R2_McFadden <- 1 - (ll_full / ll_null)
R2_McFadden

R2_corr <- cor(predict(nb_model), mydata$response_var)^2

testDispersion(nb_model)
simulateResiduals(fittedModel = nb_model, plot = T)
# testDispersion(nb_model2)
# simulateResiduals(fittedModel = nb_model2, plot = T)