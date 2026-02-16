############################################################ #
# Model data with GAM:
############################################################ #
# Load libraries
library(mgcv)
library(gratia)
library(dplyr)
library(ggplot2)
library(tidyr)
############################################################ #
# Load data 
weekly_data = readRDS("Data/Working_files/weekly_data.rds")
############################################################ #
# The following model
# Evaluates how poll and plant abundances influence visitation rate
# Botanical garden is considered a random effect
# ti permite que cambie el efecto de las abundancias con el tiempo
# s(Pair, bs="re") es el intercepto aleatorio por pareja
############################################################ #
# Scale variables before modelling
weekly_data = weekly_data %>%
  mutate(
    Week = as.numeric(Week),
    log_flower_z = as.numeric(log_flower_z),
    log_poll_z = as.numeric(log_poll_z),
    Botanical_garden = factor(Botanical_garden),
    Pair = factor(Pair),
    T_gauss_z = as.numeric(scale(T_gauss))
  )
# Run GAM
m_temporal <- gam(
  VisitRate ~ log_flower_z * log_poll_z + T_gauss_z +
    Botanical_garden +
    s(Week, Botanical_garden, bs="fs", k=8) +
    ti(log_flower_z, Week, k=c(5,8)) +
    ti(log_poll_z,   Week, k=c(5,8)) +
    ti(T_gauss_z, Week, k=c(5,8)) + 
    s(Pair, bs="re"),
  family = Gamma(link="log"),
  data = weekly_data,
  method="REML")

# Add here modelling checks

# Prepare table with modelling output

coef_table <- summary(m_temporal)$p.table
coef_table
dev.off()
plot(m_temporal,shade = TRUE, residuals = FALSE)

season_importance <- data.frame(
  Variable = c("Flower", "Pollinator", "Interaction", "Trait_matching"),
  Effect = c(
    coef_table["log_flower_z","Estimate"],
    coef_table["log_poll_z","Estimate"],
    coef_table["log_flower_z:log_poll_z","Estimate"],
    coef_table["T_gauss_z","Estimate"]
  ),
  SE = c(
    coef_table["log_flower_z","Std. Error"],
    coef_table["log_poll_z","Std. Error"],
    coef_table["log_flower_z:log_poll_z","Std. Error"],
    coef_table["T_gauss_z","Std. Error"]
  ),
  p = c(
    coef_table["log_flower_z","Pr(>|t|)"],
    coef_table["log_poll_z","Pr(>|t|)"],
    coef_table["log_flower_z:log_poll_z","Pr(>|t|)"],
    coef_table["T_gauss_z","Pr(>|t|)"]
  )
)

season_importance

# Plot variable importance
ggplot(season_importance, aes(x = Variable, y = abs(Effect))) +
  geom_col() +
  labs(
    x = NULL,
    y = "|Effect size| (standardized)",
    title = "Relative importance across the full season"
  ) +
  theme_bw()

summary(m_temporal)$s.table


# ======================================================
# PASO 3 (adaptado): weekly β(t) para Flower, Pollinator y Trait matching
# ======================================================

pair0   <- levels(factor(weekly_data$Pair))[1]
gardens <- levels(factor(weekly_data$Botanical_garden))

# grid base: semanas x jardines
# (IMPORTANTE: fijamos el resto en valores de referencia)
base_grid <- expand.grid(
  Week = sort(unique(weekly_data$Week)),
  Botanical_garden = gardens
) %>%
  mutate(
    Pair = pair0,
    log_poll_z   = 0,
    log_flower_z = 0,
    T_gauss_z    = 0   # <-- añade trait (para que predict() tenga la columna)
  )

# -----------------------------
# Flower weekly effect (+/- 1 SD en flower)
# -----------------------------
grid_f_up <- base_grid %>% mutate(log_flower_z =  1)
grid_f_dn <- base_grid %>% mutate(log_flower_z = -1)

pred_f_up <- predict(m_temporal, newdata = grid_f_up, type = "link", se.fit = TRUE)
pred_f_dn <- predict(m_temporal, newdata = grid_f_dn, type = "link", se.fit = TRUE)

flower_time <- base_grid %>%
  mutate(
    effect_link = (pred_f_up$fit - pred_f_dn$fit) / 2,
    se_link     = sqrt(pred_f_up$se.fit^2 + pred_f_dn$se.fit^2) / 2,
    lwr         = effect_link - 1.96 * se_link,
    upr         = effect_link + 1.96 * se_link,
    Variable    = "Flower"
  )

# -----------------------------
# Pollinator weekly effect (+/- 1 SD en poll)
# -----------------------------
grid_p_up <- base_grid %>% mutate(log_poll_z =  1)
grid_p_dn <- base_grid %>% mutate(log_poll_z = -1)

pred_p_up <- predict(m_temporal, newdata = grid_p_up, type = "link", se.fit = TRUE)
pred_p_dn <- predict(m_temporal, newdata = grid_p_dn, type = "link", se.fit = TRUE)

poll_time <- base_grid %>%
  mutate(
    effect_link = (pred_p_up$fit - pred_p_dn$fit) / 2,
    se_link     = sqrt(pred_p_up$se.fit^2 + pred_p_dn$se.fit^2) / 2,
    lwr         = effect_link - 1.96 * se_link,
    upr         = effect_link + 1.96 * se_link,
    Variable    = "Pollinator"
  )

# -----------------------------
# Trait matching weekly effect (+/- 1 SD en T_gauss_z)
# -----------------------------
grid_t_up <- base_grid %>% mutate(T_gauss_z =  1)
grid_t_dn <- base_grid %>% mutate(T_gauss_z = -1)

pred_t_up <- predict(m_temporal, newdata = grid_t_up, type = "link", se.fit = TRUE)
pred_t_dn <- predict(m_temporal, newdata = grid_t_dn, type = "link", se.fit = TRUE)

trait_time <- base_grid %>%
  mutate(
    effect_link = (pred_t_up$fit - pred_t_dn$fit) / 2,
    se_link     = sqrt(pred_t_up$se.fit^2 + pred_t_dn$se.fit^2) / 2,
    lwr         = effect_link - 1.96 * se_link,
    upr         = effect_link + 1.96 * se_link,
    Variable    = "Trait_matching"
  )

# -----------------------------
# Unir
# -----------------------------
beta_time <- bind_rows(flower_time, poll_time, trait_time)

# Plot weekly curves
ggplot(beta_time, aes(x = Week, y = effect_link, colour = Variable, fill = Variable)) +
  geom_line(linewidth = 1.1) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.2, colour = NA) +
  facet_wrap(~ Botanical_garden) +
  theme_bw() +
  labs(
    y = "Effect per +1 SD (link scale)",
    title = "Temporal variation in importance (week-varying effects)"
  )

# -----------------------------
# Seasonal bins (Early/Mid/Late)
# -----------------------------
beta_time <- beta_time %>%
  mutate(
    Season = cut(
      Week,
      breaks = quantile(Week, probs = c(0, 0.33, 0.66, 1), na.rm = TRUE),
      labels = c("Early", "Mid", "Late"),
      include.lowest = TRUE
    )
  )

season_summary <- beta_time %>%
  group_by(Variable, Botanical_garden, Season) %>%
  summarise(
    Mean_effect = mean(effect_link),
    SE_effect   = sd(effect_link) / sqrt(n()),
    .groups     = "drop"
  )

season_summary

ggplot(season_summary, aes(Season, Mean_effect, fill = Variable)) +
  geom_col(position = "dodge") +
  facet_wrap(~ Botanical_garden) +
  theme_bw() +
  labs(
    y = "Mean effect size",
    title = "Importance across phenological stages"
  )
