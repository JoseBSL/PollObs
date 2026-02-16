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
    T_gauss_z = as.numeric(scale(T_gauss)),
    Overlap_days_z = as.numeric(scale(Overlap_days))
    
  )
# Run GAM
m_temporal <- gam(
  VisitRate ~ log_flower_z * log_poll_z + 
    T_gauss_z +
    Overlap_days_z + 
    Botanical_garden +
    s(Week, Botanical_garden, bs="fs", k=8) +
    ti(log_flower_z, Week, k=c(5,8)) +
    ti(log_poll_z,   Week, k=c(5,8)) +
    ti(T_gauss_z, Week, k=c(5,8)) + 
    ti(Overlap_days_z, Week, k=c(5,8)) +
    s(Pair, bs="re"),
  family = gaussian(link="log"),
  data = weekly_data,
  method="REML")

#Gamma(link="log")
# Add here modelling checks
library(DHARMa)

set.seed(1)
sim_res <- simulateResiduals(
  fittedModel = m_temporal,
  n = 500
)

plot(sim_res)                 # panel general
testUniformity(sim_res)       # ¿residuos ~ Uniform(0,1)?
testDispersion(sim_res)       # sobre/sub-dispersión
testOutliers(sim_res)         # outliers
# Prepare table with modelling output
coef_table <- summary(m_temporal)$p.table
print(coef_table)

# safer than dev.off()
while (dev.cur() > 1) dev.off()
plot(m_temporal, shade = TRUE, residuals = FALSE)

season_importance <- data.frame(
  Variable = c("Flower", "Pollinator", "Interaction", "Trait_matching", "Phenology_overlap"),
  Effect = c(
    coef_table["log_flower_z","Estimate"],
    coef_table["log_poll_z","Estimate"],
    coef_table["log_flower_z:log_poll_z","Estimate"],
    coef_table["T_gauss_z","Estimate"],
    coef_table["Overlap_days_z","Estimate"]
  ),
  SE = c(
    coef_table["log_flower_z","Std. Error"],
    coef_table["log_poll_z","Std. Error"],
    coef_table["log_flower_z:log_poll_z","Std. Error"],
    coef_table["T_gauss_z","Std. Error"],
    coef_table["Overlap_days_z","Std. Error"]
  ),
  p = c(
    coef_table["log_flower_z","Pr(>|t|)"],
    coef_table["log_poll_z","Pr(>|t|)"],
    coef_table["log_flower_z:log_poll_z","Pr(>|t|)"],
    coef_table["T_gauss_z","Pr(>|t|)"],
    coef_table["Overlap_days_z","Pr(>|t|)"]
  )
)

season_importance

# Plot variable importance
ggplot(season_importance, aes(x = Variable, y = abs(Effect))) +
  geom_col() +
  labs(
    x = NULL,
    y = "|Effect size| (standardized; link scale)",
    title = "Relative importance across the full season"
  ) +
  theme_bw()

print(summary(m_temporal)$s.table)

# ======================================================
# PASO 3 (adaptado): weekly β(t) para Flower, Pollinator y Trait matching
# ======================================================

pair0   <- levels(factor(weekly_data$Pair))[1]
gardens <- levels(factor(weekly_data$Botanical_garden))

# grid base: weeks x gardens (reference values for other predictors)
base_grid <- expand.grid(
  Week = sort(unique(weekly_data$Week)),
  Botanical_garden = gardens
) %>%
  mutate(
    Pair = pair0,
    log_poll_z     = 0,
    log_flower_z   = 0,
    T_gauss_z      = 0,
    Overlap_days_z = 0
  )

# helper: compute +/-1 SD effect for one variable
effect_pm1 <- function(var, label){
  up <- base_grid
  dn <- base_grid
  up[[var]] <-  1
  dn[[var]] <- -1
  
  p_up <- predict(m_temporal, newdata = up, type = "link", se.fit = TRUE)
  p_dn <- predict(m_temporal, newdata = dn, type = "link", se.fit = TRUE)
  
  base_grid %>%
    mutate(
      effect_link = (p_up$fit - p_dn$fit) / 2,
      se_link     = sqrt(p_up$se.fit^2 + p_dn$se.fit^2) / 2,
      lwr         = effect_link - 1.96 * se_link,
      upr         = effect_link + 1.96 * se_link,
      Variable    = label
    )
}

flower_time <- effect_pm1("log_flower_z",   "Flower")
poll_time   <- effect_pm1("log_poll_z",     "Pollinator")
trait_time  <- effect_pm1("T_gauss_z",      "Trait_matching")
overlap_time<- effect_pm1("Overlap_days_z", "Phenology_overlap")

beta_time <- bind_rows(flower_time, poll_time, trait_time, overlap_time)

# Weekly plot
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
