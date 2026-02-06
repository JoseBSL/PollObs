library(performance)
library(dplyr)

# -----------------------------
# 1. Full model marginal R²
# -----------------------------
r2_full <- r2_nakagawa(model1)$R2_marginal

# -----------------------------
# 2. Reduced models
# (respect interaction hierarchy)
# -----------------------------

m_no_inter <- update(model1, . ~ . - log_flower:log_poll_z)

m_no_flower <- update(model1, . ~ . - log_flower - log_flower:log_poll_z)

m_no_poll <- update(model1, . ~ . - log_poll_z - log_flower:log_poll_z)

m_no_garden <- update(model1, . ~ . - Botanical_garden)

# -----------------------------
# 3. Extract marginal R² for each
# -----------------------------
r2_table <- tibble(
  Dropped_variable = c(
    "Interaction (log_flower × log_poll_z)",
    "Floral abundance",
    "Pollinator abundance",
    "Botanical garden"
  ),
  Reduced_model_R2 = c(
    r2_nakagawa(m_no_inter)$R2_marginal,
    r2_nakagawa(m_no_flower)$R2_marginal,
    r2_nakagawa(m_no_poll)$R2_marginal,
    r2_nakagawa(m_no_garden)$R2_marginal
  )
) %>%
  mutate(
    Full_model_R2 = r2_full,
    Delta_R2 = Full_model_R2 - Reduced_model_R2
  ) %>%
  arrange(desc(Delta_R2))

r2_table


library(ggplot2)

ggplot(r2_table,
       aes(x = reorder(Dropped_variable, Delta_R2),
           y = Delta_R2)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    x = "Variable removed",
    y = "Change in marginal R²",
    title = "Relative importance of predictors"
  ) +
  theme_minimal()

# -----------------------------
# Function to compute ΔR² for one garden
# -----------------------------
importance_by_garden <- function(df) {
  
  # Fit garden-specific model
  m_full <- glmmTMB(
    VisitRate ~ log_flower * log_poll_z +
      (1 | Week) +
      (1 | Pair),
    family = Gamma(link = "log"),
    data = df
  )
  
  r2_full <- r2_nakagawa(m_full)$R2_marginal
  
  # Reduced models
  m_no_inter  <- update(m_full, . ~ . - log_flower:log_poll_z)
  m_no_flower <- update(m_full, . ~ . - log_flower - log_flower:log_poll_z)
  m_no_poll   <- update(m_full, . ~ . - log_poll_z - log_flower:log_poll_z)
  
  tibble(
    Garden = unique(df$Botanical_garden),
    Variable = c("Interaction", "Floral abundance", "Pollinator abundance"),
    Delta_R2 = c(
      r2_full - r2_nakagawa(m_no_inter)$R2_marginal,
      r2_full - r2_nakagawa(m_no_flower)$R2_marginal,
      r2_full - r2_nakagawa(m_no_poll)$R2_marginal
    )
  )
}

# -----------------------------
# Run across gardens
# -----------------------------
garden_importance <- model_data %>%
  filter(!is.na(VisitRate)) %>%
  group_split(Botanical_garden) %>%
  map_dfr(importance_by_garden)

garden_importance


ggplot(garden_importance,
       aes(x = Variable, y = Delta_R2, fill = Variable)) +
  geom_col() +
  facet_wrap(~ Garden) +
  theme_minimal() +
  labs(
    y = "Change in marginal R²",
    x = "Predictor importance"
  ) +
  theme(legend.position = "none")

