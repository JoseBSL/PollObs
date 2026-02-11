############################################################ #
#Model FULL SEASON:
############################################################ #
library(ggplot2)
library(glmmTMB)
library(DHARMa)
library(ggeffects)
library(performance)
library(stringr)

full_season_data = readRDS("Data/Working_files/full_season_data.rds")

full_season_data %>% 
  filter(is.na(Pheno_probability)) %>% 
  distinct(Pollinator) %>% 
  pull()

full_season_data = full_season_data %>% 
  mutate(Pheno_probability_z = scale(Pheno_probability))

model1 = glmmTMB(VisitRate ~  log_flower_z * 
                   log_poll_z +
                   Botanical_garden + Pheno_probability_z + 
                   (1 | Pair),
                 family = Gamma(link = "log"),
                 data = full_season_data)

summary(model1)
simulationOutput <- simulateResiduals(fittedModel = model1)
plot(simulationOutput)
r2(model1)
check_collinearity(model1)


# ---- 1) Full model marginal R²
r2_full <- r2_nakagawa(model1)$R2_marginal

# ---- 2) Reduced models (respect interaction hierarchy)
m_no_inter  <- update(model1, . ~ . - log_flower_z:log_poll_z)

# If you drop a main effect, drop the interaction too (hierarchy)
m_no_flower <- update(model1, . ~ . - log_flower_z - log_flower_z:log_poll_z)
m_no_poll   <- update(model1, . ~ . - log_poll_z   - log_flower_z:log_poll_z)

m_no_garden <- update(model1, . ~ . - Botanical_garden)

# Drop phenology term
m_no_pheno  <- update(model1, . ~ . - Pheno_probability_z)

# ---- 3) Extract marginal R² and compute ΔR²
r2_table <- tibble(
  Dropped_term = c(
    "Interaction (log_flower_z × log_poll_z)",
    "Floral abundance (log_flower_z + interaction)",
    "Pollinator abundance (log_poll_z + interaction)",
    "Botanical garden",
    "Phenological overlap"
  ),
  Reduced_model_R2 = c(
    r2_nakagawa(m_no_inter)$R2_marginal,
    r2_nakagawa(m_no_flower)$R2_marginal,
    r2_nakagawa(m_no_poll)$R2_marginal,
    r2_nakagawa(m_no_garden)$R2_marginal,
    r2_nakagawa(m_no_pheno)$R2_marginal
  )
) %>%
  mutate(
    Full_model_R2 = r2_full,
    Delta_R2 = Full_model_R2 - Reduced_model_R2
  ) %>%
  arrange(desc(Delta_R2))

print(r2_table)

# ---- 4) Standardize names (same labels every run)
r2_table_std <- r2_table %>%
  mutate(
    Term = case_when(
      str_detect(Dropped_term, "^Floral")        ~ "Floral abundance",
      str_detect(Dropped_term, "^Pollinator")    ~ "Pollinator abundance",
      str_detect(Dropped_term, "^Botanical")     ~ "Botanical garden",
      str_detect(Dropped_term, "^Interaction")   ~ "Interaction",
      str_detect(Dropped_term, "^Phenological")  ~ "Phenological overlap",
      TRUE ~ Dropped_term
    ),
    Term = factor(Term, levels = c(
      "Pollinator abundance",
      "Floral abundance",
      "Phenological overlap",
      "Botanical garden",
      "Interaction"
    ))
  )

print(r2_table_std)

# ---- 5) Optional: keep only the predictors you want to compare/export
r2_full_season <- r2_table_std %>%
  filter(Term %in% c("Floral abundance", "Pollinator abundance", "Phenological overlap"))

saveRDS(r2_full_season, "Data/Working_files/r2_full_season.rds")

# ---- 6) Plot (all terms)
ggplot(r2_table_std,
       aes(x = reorder(Term, Delta_R2), y = Delta_R2)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    x = "Variable removed",
    y = "Change in marginal R²",
    title = "Relative importance of predictors (Δ marginal R²)"
  ) +
  theme_minimal()

# ---- 7) Plot (bio terms only)
ggplot(r2_full_season,
       aes(x = reorder(Term, Delta_R2), y = Delta_R2)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    x = "Variable removed",
    y = "Change in marginal R²",
    title = "Relative importance (bio predictors only)"
  ) +
  theme_minimal()
