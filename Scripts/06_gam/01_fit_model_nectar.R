############################################################
# FINAL supported model only
############################################################
# Load libraries
library(mgcv)
library(dplyr)
library(ggplot2)
library(DHARMa)
library(tibble)
library(viridis)
library(patchwork)
library(forcats)   # needed for fct_rev()

################################################################################
# Load data
weekly_data = readRDS("Data/Working_files/weekly_data_for_modelling.rds")
colnames(weekly_data)

################################################################################
# Prepare data
dat = weekly_data %>%
  mutate(
    Week = as.numeric(Week),
    Botanical_garden = factor(Botanical_garden),
    Pair = factor(Pair)
  ) %>%
  arrange(Pair, Week) %>%
  mutate(
    AR.start = c(TRUE, Pair[-1] != Pair[-n()]),
    log_flower = log1p(Floral_abundance),
    log_poll = log1p(Total_pollinator_abundance),
    log_flower_z = as.numeric(scale(log_flower)),
    log_poll_z = as.numeric(scale(log_poll)),
    T_gauss_z = as.numeric(scale(T_gauss)),
    Flower_width_z = as.numeric(scale(Flower_width)),
    Overlap_days_z = as.numeric(scale(Overlap_days)),
    Mean_nectar_volume = as.numeric(Mean_nectar_volume),
    Mean_nectar_volume_z = as.numeric(scale(Mean_nectar_volume)),
    Total_time_plant_week = as.numeric(Total_time_plant_week)
  )

################################################################################
# FINAL supported model
################################################################################
m_final_ar1 = bam(
  Total_pair_interactions ~ 
    log_flower_z +
    log_poll_z +
    Flower_width_z +
    Mean_nectar_volume_z +          # <-- added
    log_flower_z:log_poll_z +
    log_poll_z:Flower_width_z +
    T_gauss_z +
    Overlap_days_z +
    Botanical_garden +
    s(Week, Botanical_garden, bs = "fs", k = 8) +
    s(Pair, bs = "re") +
    offset(log(Total_time_plant_week)),
  family = nb(),
  data = dat,
  method = "fREML",
  rho = 0.3,
  AR.start = dat$AR.start,
  discrete = TRUE
)

################################################################################
# Diagnostics
################################################################################
concurvity(m_final_ar1, full = TRUE)
gam.check(m_final_ar1)

set.seed(1)
sim_res = simulateResiduals(fittedModel = m_final_ar1, n = 500)
plot(sim_res)
testUniformity(sim_res)
testDispersion(sim_res)
testOutliers(sim_res, type = "bootstrap")

################################################################################
# Coefficients table
################################################################################
coef_tab = as.data.frame(summary(m_final_ar1)$p.table) %>%
  rownames_to_column("term")

################################################################################
# Check variable relevance with AIC
################################################################################
terms_to_test = c(
  "log_flower_z",
  "log_poll_z",
  "Flower_width_z",
  "Mean_nectar_volume_z",          # <-- added
  "T_gauss_z",
  "Overlap_days_z",
  "log_flower_z:log_poll_z",
  "log_poll_z:Flower_width_z"
)

get_delta_aic = function(model, term) {
  m_drop = update(model, as.formula(paste(". ~ . -", term)))
  AIC(m_drop) - AIC(model)
}

delta_tab = data.frame(
  term = terms_to_test,
  deltaAIC = sapply(terms_to_test, get_delta_aic, model = m_final_ar1)
)

lab_map = c(
  log_flower_z                = "Flower abundance",
  log_poll_z                  = "Pollinator abundance",
  Flower_width_z              = "Flower width",
  Mean_nectar_volume_z        = "Mean nectar volume",   # <-- added
  T_gauss_z                   = "Trait matching",
  Overlap_days_z              = "Phenology overlap",
  `log_flower_z:log_poll_z`   = "Flower abund. × Pollinator abund.",
  `log_poll_z:Flower_width_z` = "Flower width × Pollinator abund."
)

delta_tab = delta_tab %>%
  mutate(Variable = lab_map[term]) %>%
  arrange(deltaAIC) %>%
  mutate(Variable = factor(Variable, levels = Variable))

################################################################################
# Relative importance plot (coefficients ±95% CI, link scale)
################################################################################
imp = coef_tab %>%
  filter(term %in% names(lab_map)) %>%
  transmute(
    term,
    Variable  = unname(lab_map[term]),
    Estimate  = Estimate,
    CI_low    = Estimate - 1.96 * `Std. Error`,
    CI_high   = Estimate + 1.96 * `Std. Error`,
    absE      = abs(Estimate),
    is_interaction = grepl(":", term)
  ) %>%
  arrange(absE) %>%
  mutate(Variable = fct_rev(factor(Variable, levels = Variable)))

xlim = range(c(imp$CI_low, imp$CI_high), na.rm = TRUE)

p_imp_mag = ggplot(imp, aes(x = Estimate, y = Variable)) +
  geom_vline(xintercept = 0, linetype = 2, linewidth = 0.7, colour = "grey40") +
  geom_errorbarh(aes(xmin = CI_low, xmax = CI_high),
                 height = 0, linewidth = 1.1, colour = "grey30",
                 lineend = "round") +
  geom_point(aes(fill = absE),
             shape = 21, size = 4, colour = "black", stroke = 0.6) +
  scale_fill_viridis_c(option = "magma", direction = -1, name = "Relative importance") +
  coord_cartesian(xlim = xlim) +
  labs(
    x = "Effect (log link scale, ±95% CI)",
    y = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "right",
    axis.text.y = element_text(
      face = ifelse(imp$is_interaction, "bold", "plain")
    )
  )

print(p_imp_mag)