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
library(forcats)

################################################################################
# Load data
################################################################################
weekly_data = readRDS("Data/Working_files/weekly_data_for_modelling.rds")

################################################################################
# Prepare data
# Select main variables and standardize data
################################################################################
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
    Mean_nectar_volume_z +               # <- added
    log_flower_z:log_poll_z +            # abundance effect depends on pollinator abundance
    log_poll_z:Flower_width_z +          # width advantage depends on pollinator abundance
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

coef_tab = as.data.frame(summary(m_final_ar1)$p.table) %>%
  rownames_to_column("term")

################################################################################
# Save modelling output
################################################################################
saveRDS(m_final_ar1, "Data/Working_files/m_final_ar1.rds")
saveRDS(dat, "Data/Working_files/dat_model_input.rds")
saveRDS(coef_tab, "Data/Working_files/m_final_coefficients.rds")

################################################################################
# Check variable relevance with AIC
################################################################################
terms_to_test = c(
  "log_flower_z",
  "log_poll_z",
  "Flower_width_z",
  "Mean_nectar_volume_z",               # <- added
  "T_gauss_z",
  "Overlap_days_z",
  "log_flower_z:log_poll_z",
  "log_poll_z:Flower_width_z"
)

# Function to compare models with and without variables
get_delta_aic = function(model, term) {
  m_drop = update(model, as.formula(paste(". ~ . -", term)))
  AIC(m_drop) - AIC(model)
}

# Create table with output
delta_tab = data.frame(
  term = terms_to_test,
  deltaAIC = sapply(terms_to_test, get_delta_aic, model = m_final_ar1)
)

# Rename variables
lab_map = c(
  log_flower_z                = "Flower abundance",
  log_poll_z                  = "Pollinator abundance",
  Flower_width_z              = "Flower width",
  Mean_nectar_volume_z        = "Mean nectar volume",   # <- added
  T_gauss_z                   = "Trait matching",
  Overlap_days_z              = "Phenology overlap",
  `log_flower_z:log_poll_z`   = "Flower abund. × Pollinator abund.",
  `log_poll_z:Flower_width_z` = "Flower width × Pollinator abund."
)

# Rename variables and reorganize data for plotting
delta_tab = delta_tab %>%
  mutate(Variable = lab_map[term]) %>%
  arrange(desc(deltaAIC)) %>%
  mutate(Variable = factor(Variable, levels = Variable))

################################################################################
# Save data
################################################################################
saveRDS(delta_tab, "Data/Working_files/delta_tab.rds")

################################################################################
# Relative importance plot (coefficients ±95% CI, link scale)
################################################################################
coef_tab = as.data.frame(summary(m_final_ar1)$p.table) %>%
  rownames_to_column("term")

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
  arrange(desc(Estimate)) %>%                     # <- clearer visual order
  mutate(Variable = factor(Variable, levels = Variable))

xlim = range(c(imp$CI_low, imp$CI_high), na.rm = TRUE)

p_imp_mag = ggplot(imp, aes(x = Estimate, y = Variable)) +
  geom_vline(xintercept = 0, linetype = 2, linewidth = 0.7, colour = "grey40") +
  geom_errorbar(
    aes(xmin = CI_low, xmax = CI_high),
    width = 0,
    linewidth = 1.1,
    colour = "grey30",
    lineend = "round",
    orientation = "y"
  ) +
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
    legend.position = "right"
  )

print(p_imp_mag)

# -----------------------------
# Prediction curves on response scale (population-level)
# - exclude Pair RE + garden smooth + garden fixed effect
# -----------------------------
bg0   = levels(dat$Botanical_garden)[1]   # placeholder only
week0 = median(dat$Week, na.rm = TRUE)    # placeholder only
pair0 = levels(dat$Pair)[1]               # placeholder only
exclude_all = c("s(Pair)", "s(Week,Botanical_garden)", "Botanical_garden")

# Pollinator levels for curves (10th, 75th, 90th percentiles)
poll_vals = as.numeric(quantile(dat$log_poll_z, probs = c(0.10, 0.75, 0.90), na.rm = TRUE))
poll_labs = c("Low", "High", "Very high")

# ---- Curve A: visitation vs flower abundance ----
F_seq = seq(
  min(dat$log_flower_z, na.rm = TRUE),
  max(dat$log_flower_z, na.rm = TRUE),
  length.out = 140
)

newF = expand.grid(
  log_flower_z = F_seq,
  log_poll_z = poll_vals,
  Flower_width_z = 0,
  Mean_nectar_volume_z = 0,   # <- added
  T_gauss_z = 0,
  Overlap_days_z = 0,
  Botanical_garden = bg0,
  Week = week0,
  Pair = pair0,
  Total_time_plant_week = 1
)

prF = predict(m_final_ar1, newdata = newF, type = "response", se.fit = TRUE, exclude = exclude_all)
newF$fit = prF$fit
newF$lwr = pmax(0, prF$fit - 1.96 * prF$se.fit)
newF$upr = prF$fit + 1.96 * prF$se.fit
newF$Poll_group = factor(newF$log_poll_z, levels = poll_vals, labels = poll_labs)

# ---- Curve B: visitation vs flower width ----
W_seq = seq(
  min(dat$Flower_width_z, na.rm = TRUE),
  max(dat$Flower_width_z, na.rm = TRUE),
  length.out = 100
)

newW = expand.grid(
  Flower_width_z = W_seq,
  log_poll_z = poll_vals,
  log_flower_z = 0,
  Mean_nectar_volume_z = 0,   # <- added
  T_gauss_z = 0,
  Overlap_days_z = 0,
  Botanical_garden = bg0,
  Week = week0,
  Pair = pair0,
  Total_time_plant_week = 1
)

prW = predict(m_final_ar1, newdata = newW, type = "response", se.fit = TRUE, exclude = exclude_all)
newW$fit = prW$fit
newW$lwr = pmax(0, prW$fit - 1.96 * prW$se.fit)
newW$upr = prW$fit + 1.96 * prW$se.fit
newW$Poll_group = factor(newW$log_poll_z, levels = poll_vals, labels = poll_labs)

# ---- Curve C: visitation vs mean nectar volume ----
N_seq = seq(
  min(dat$Mean_nectar_volume_z, na.rm = TRUE),
  max(dat$Mean_nectar_volume_z, na.rm = TRUE),
  length.out = 100
)

newN = expand.grid(
  Mean_nectar_volume_z = N_seq,
  log_poll_z = poll_vals,
  log_flower_z = 0,
  Flower_width_z = 0,
  T_gauss_z = 0,
  Overlap_days_z = 0,
  Botanical_garden = bg0,
  Week = week0,
  Pair = pair0,
  Total_time_plant_week = 1
)

prN = predict(m_final_ar1, newdata = newN, type = "response", se.fit = TRUE, exclude = exclude_all)
newN$fit = prN$fit
newN$lwr = pmax(0, prN$fit - 1.96 * prN$se.fit)
newN$upr = prN$fit + 1.96 * prN$se.fit
newN$Poll_group = factor(newN$log_poll_z, levels = poll_vals, labels = poll_labs)

# Shared y limits
y_lim = range(c(newF$lwr, newF$upr, newW$lwr, newW$upr, newN$lwr, newN$upr), na.rm = TRUE)

# Build plots (same y scale)
p_f_abundance = ggplot(newF, aes(x = log_flower_z, y = fit, colour = Poll_group, fill = Poll_group)) +
  geom_line(linewidth = 1.1) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.2, colour = NA) +
  scale_colour_viridis_d(option = "plasma", end = 0.9) +
  scale_fill_viridis_d(option = "plasma", end = 0.9) +
  coord_cartesian(ylim = y_lim) +
  theme_bw(base_size = 12) +
  labs(
    x = "Flower abundance (z)",
    y = "Predicted visitation",
    colour = "Pollinator abundance",
    fill   = "Pollinator abundance",
    subtitle = "Flower abund. × Pollinator abund."
  ) +
  guides(fill = "none") +
  theme(
    plot.subtitle = element_text(face = "bold")
  )

p_f_width = ggplot(newW, aes(x = Flower_width_z, y = fit, colour = Poll_group, fill = Poll_group)) +
  geom_line(linewidth = 1.2) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.18, colour = NA) +
  scale_colour_viridis_d(option = "plasma", end = 0.9, drop = FALSE) +
  scale_fill_viridis_d(option = "plasma", end = 0.9, drop = FALSE) +
  coord_cartesian(ylim = y_lim) +
  theme_bw(base_size = 12) +
  labs(
    x = "Flower width (z)",
    y = "Predicted visitation",
    colour = "Pollinator abundance",
    fill   = "Pollinator abundance",
    subtitle = "Flower width × Pollinator abund."
  ) +
  guides(fill = "none") +
  theme(
    plot.subtitle = element_text(face = "bold")
  )

# Force ONE legend by removing it from the left plot
p_imp_mag
p_f_abundance_noleg = p_f_abundance + theme(legend.position = "none")

p_f_width_leg = p_f_width + theme(legend.position = "right")

panel2 <- plot_spacer() + p_f_abundance_noleg + p_f_width_leg +
  plot_layout(widths = c(0.01, 1.5, 1.5))

top_row <- plot_spacer() + p_imp_mag +
  plot_layout(widths = c(0.05, 2))

top_row/panel2 
