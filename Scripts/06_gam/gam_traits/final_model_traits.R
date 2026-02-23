############################################################
# Refined workflow: FINAL supported model only
# + DHARMa diagnostics
# + Relative-importance coefficient plot
# + Conditional-slope plot (link scale)
# + Prediction curves (response scale):
#     - population-level (exclude Pair + garden smooth + garden fixed effect)
#     - SAME y-limits
#     - ONE legend (kept only in right plot)
############################################################

library(mgcv)
library(dplyr)
library(ggplot2)
library(DHARMa)
library(tibble)
library(viridis)
library(patchwork)

# -----------------------------
# Load + prep data
# -----------------------------
weekly_data <- readRDS("Data/Working_files/weekly_data_floral_display.rds")

dat <- weekly_data %>%
  mutate(
    Week = as.numeric(Week),
    Botanical_garden = factor(Botanical_garden),
    Pair = factor(Pair)
  ) %>%
  arrange(Pair, Week) %>%
  mutate(AR.start = c(TRUE, Pair[-1] != Pair[-n()]))

# keep ONLY flower width (standardized)
stopifnot("Flower_width" %in% names(dat))
dat <- dat %>% mutate(Flower_width_z = as.numeric(scale(Flower_width)))

# -----------------------------
# FINAL supported model
# -----------------------------
m_final_ar1 <- bam(
  Total_pair_interactions ~ 
    log_flower_z +
    log_poll_z +
    Flower_width_z +
    log_flower_z:log_poll_z +       # abundance effect depends on pollinator abundance
    log_poll_z:Flower_width_z +     # width advantage depends on pollinator abundance
    T_gauss_z +
    Overlap_days_z +
    Botanical_garden +
    s(Week, Botanical_garden, bs="fs", k=8) +
    s(Pair, bs="re") +
    offset(log(Total_time_plant_week)),
  family = nb(),
  data = dat,
  method="fREML",
  rho=0.3,
  AR.start=dat$AR.start,
  discrete=TRUE
)

# -----------------------------
# Diagnostics
# -----------------------------
concurvity(m_final_ar1, full = TRUE)
gam.check(m_final_ar1)

set.seed(1)
sim_res <- simulateResiduals(fittedModel = m_final_ar1, n = 500)
plot(sim_res)
testUniformity(sim_res)
testDispersion(sim_res)
testOutliers(sim_res, type = "bootstrap")

# -----------------------------
# Relative importance plot (coefficients ±95% CI, link scale)
# -----------------------------
coef_tab <- as.data.frame(summary(m_final_ar1)$p.table) %>%
  rownames_to_column("term")

lab_map <- c(
  log_flower_z                = "Flower abundance",
  log_poll_z                  = "Pollinator abundance",
  T_gauss_z                   = "Trait matching",
  Overlap_days_z              = "Phenology overlap",
  Flower_width_z              = "Flower width",
  `log_flower_z:log_poll_z`   = "Flower abund. × Pollinator abund.",
  `log_poll_z:Flower_width_z` = "Flower width x Pollinator abund."
)

coef_tab <- as.data.frame(summary(m_final_ar1)$p.table) %>%
  rownames_to_column("term")

imp <- coef_tab %>%
  filter(term %in% names(lab_map)) %>%
  transmute(
    term,
    Variable  = lab_map[term] |> unname(),
    Estimate  = Estimate,
    CI_low    = Estimate - 1.96 * `Std. Error`,
    CI_high   = Estimate + 1.96 * `Std. Error`,
    absE      = abs(Estimate),
    is_interaction = grepl(":", term)   # <- detect interaction
  ) %>%
  arrange(absE) %>%
  mutate(Variable = fct_rev(factor(Variable, levels = Variable)))

xlim <- range(c(imp$CI_low, imp$CI_high), na.rm = TRUE)

#Manual fix
imp$is_interaction = c(FALSE,FALSE,FALSE,TRUE,FALSE,TRUE,FALSE)

p_imp_mag <- ggplot(imp, aes(x = Estimate, y = Variable)) +
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
# -----------------------------
# Prediction curves on response scale (population-level)
# - exclude Pair RE + garden smooth + garden fixed effect
# -----------------------------
bg0   <- levels(dat$Botanical_garden)[1]   # placeholder only
week0 <- median(dat$Week, na.rm = TRUE)    # placeholder only
pair0 <- levels(dat$Pair)[1]               # placeholder only
exclude_all <- c("s(Pair)", "s(Week,Botanical_garden)", "Botanical_garden")

# Pollinator levels for curves (10th, 75th, 90th percentiles)
poll_vals <- as.numeric(quantile(dat$log_poll_z, probs = c(0.10, 0.75, 0.90), na.rm = TRUE))
poll_labs <- c("Low", "High", "Very high")

# ---- Curve A: visitation vs flower abundance ----
F_seq <- seq(min(dat$log_flower_z, na.rm=TRUE),
             max(dat$log_flower_z, na.rm=TRUE), length.out=140)

newF <- expand.grid(
  log_flower_z = F_seq,
  log_poll_z = poll_vals,
  Flower_width_z = 0,
  T_gauss_z = 0,
  Overlap_days_z = 0,
  Botanical_garden = bg0,
  Week = week0,
  Pair = pair0,
  Total_time_plant_week = 1
)

prF <- predict(m_final_ar1, newdata = newF, type = "response", se.fit = TRUE, exclude = exclude_all)
newF$fit <- prF$fit
newF$lwr <- prF$fit - 1.96 * prF$se.fit
newF$upr <- prF$fit + 1.96 * prF$se.fit
newF$Poll_group <- factor(newF$log_poll_z, levels = poll_vals, labels = poll_labs)

# ---- Curve B: visitation vs flower width ----
W_seq <- seq(min(dat$Flower_width_z, na.rm=TRUE),
             max(dat$Flower_width_z, na.rm=TRUE), length.out=100)

newW <- expand.grid(
  Flower_width_z = W_seq,
  log_poll_z = poll_vals,
  log_flower_z = 0,
  T_gauss_z = 0,
  Overlap_days_z = 0,
  Botanical_garden = bg0,
  Week = week0,
  Pair = pair0,
  Total_time_plant_week = 1
)

prW <- predict(m_final_ar1, newdata = newW, type = "response", se.fit = TRUE, exclude = exclude_all)
newW$fit <- prW$fit
newW$lwr <- prW$fit - 1.96 * prW$se.fit
newW$upr <- prW$fit + 1.96 * prW$se.fit
newW$Poll_group <- factor(newW$log_poll_z, levels = poll_vals, labels = poll_labs)

# Shared y limits
y_lim <- range(c(newF$lwr, newF$upr, newW$lwr, newW$upr), na.rm = TRUE)

# Build plots (same y scale)
p_f_abundance <- ggplot(newF, aes(x = log_flower_z, y = fit, colour = Poll_group, fill = Poll_group)) +
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
    title  = NULL
  ) +
  guides(fill = "none") +   # keep only colour legend
  labs(
    subtitle = "Flower abund. × Pollinator abund."
  )+
  theme(
    plot.subtitle = element_text(face = "bold")
  )
p_f_width <- ggplot(newW, aes(x = Flower_width_z, y = fit, colour = Poll_group, fill = Poll_group)) +
  geom_line(linewidth = 1.2) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.18, colour = NA) +
  scale_colour_viridis_d(option = "plasma", end = 0.9, drop = FALSE) +  # <- key
  scale_fill_viridis_d(option = "plasma", end = 0.9, drop = FALSE) +    # <- key
  coord_cartesian(ylim = y_lim) +
  theme_bw(base_size = 12) +
  labs(
    x = "Flower width (z)",
    y = "Predicted visitation",
    colour = "Pollinator abundance",
    fill   = "Pollinator abundance",
    title  = NULL
  ) +
  guides(fill = "none")  +  # <- force legend format
  labs(
    subtitle = "Flower width × Pollinator abund."
  ) +
  theme(
    plot.subtitle = element_text(face = "bold")
  )
# Force ONE legend by removing it from the left plot
p_imp_mag
p_f_abundance_noleg <- p_f_abundance + theme(legend.position = "none")

p_f_width_leg = p_f_width + theme(legend.position = "right")
  
panel2 <- plot_spacer() + p_f_abundance_noleg + p_f_width_leg +
  plot_layout(widths = c(0.01, 1.5, 1.5))

top_row <- plot_spacer() + p_imp_mag +
  plot_layout(widths = c(0.05, 2))

top_row/panel2 




