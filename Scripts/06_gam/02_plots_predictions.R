############################################################
# 02_plots_predictions.R
# - Load model + data (saved from modelling chunk)
# - Coefficient plot (β ± 95% CI, link scale)
# - ΔAIC drop-one plot aligned to coefficient plot order
# - Prediction curves with correct CIs (link -> response)
# - Combine panels
############################################################

library(dplyr)
library(ggplot2)
library(forcats)
library(tibble)
library(viridis)
library(patchwork)
library(tidyr)
# -----------------------------
# Load saved objects
# -----------------------------
m_final_ar1 <- readRDS("Data/Working_files/m_final_ar1.rds")
dat         <- readRDS("Data/Working_files/dat_model_input.rds")
delta_tab   <- readRDS("Data/Working_files/delta_tab.rds")  # term, deltaAIC
# -----------------------------
# Label map (terms must match model)
# -----------------------------
lab_map <- c(
  log_flower_z                = "Flower abundance",
  log_poll_z                  = "Pollinator abundance",
  T_gauss_z                   = "Trait matching",
  Overlap_days_z              = "Phenology overlap",
  Flower_width_z              = "Flower width",
  `log_flower_z:log_poll_z`   = "Flower abund. × Pollinator abund.",
  `log_poll_z:Flower_width_z` = "Flower width × Pollinator abund."
)

# -----------------------------
# Coefficient table -> imp
# (parametric coefficients only)
# -----------------------------
coef_tab <- as.data.frame(summary(m_final_ar1)$p.table) %>%
  rownames_to_column("term")

imp <- coef_tab %>%
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

xlim <- range(c(imp$CI_low, imp$CI_high), na.rm = TRUE)

# -----------------------------
# Join ΔAIC & force identical y-order
# -----------------------------
imp2 <- imp %>%
  left_join(delta_tab %>% select(term, deltaAIC), by = "term") %>%
  mutate(
    deltaAIC = replace_na(deltaAIC, 0),
    Variable = factor(Variable, levels = levels(imp$Variable))
  )

# Shared palette (same gradient, independent scaling in each plot)
fill_magma <- scale_fill_viridis_c(option = "magma", direction = -1)

imp2$Variable <- factor(imp2$Variable, levels = rev(levels(factor(imp2$Variable))))
# -----------------------------
# Panel A: β plot (link scale)
# -----------------------------
p_imp_mag2 <- ggplot(imp2, aes(x = Estimate, y = Variable)) +
  geom_vline(xintercept = 0, linetype = 2, linewidth = 0.7, colour = "grey40") +
  geom_errorbarh(aes(xmin = CI_low, xmax = CI_high),
                 height = 0, linewidth = 1.5, colour = "grey30",
                 lineend = "round") +
  geom_point(aes(fill=is_interaction), colour = "black",
             shape = 21, size = 5, stroke = 0.6) +
  scale_fill_manual(
    values = c("FALSE" = "#B12A90",   # main effects
               "TRUE"  = "#FCA636"),  # interactions (plasma orange)
    guide = "none"
  )+
#  fill_magma +
  scale_x_continuous(
    breaks = c(0, 0.4, 0.8))+
  labs(
    x = "Standardized effect (log link)",
    y = NULL,
    fill = "Magnitude"
  ) +
  theme_minimal(base_size = 15) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    legend.title = element_text(face = "bold"),
    legend.position = "right",
    axis.text.y = element_text(size=18,face = ifelse(imp2$is_interaction, "bold", "plain")),
    plot.margin = margin(5.5, 0, 5.5, 5.5)
  )

# -----------------------------
# Panel B: ΔAIC plot (drop-one)
# -----------------------------

aic_breaks = c(0,50,100)


fill_magma_aic <- scale_fill_viridis_c(
  option = "magma",
  direction = -1,
  breaks = aic_breaks
)
p_aic <- ggplot(imp2, aes(y = Variable, x = deltaAIC)) +
  geom_vline(xintercept = 0, linetype = 2, linewidth = 0.7, colour = "grey40") +
  geom_col(aes(fill=is_interaction),width = 0.65, colour = "black") +
  scale_fill_manual(
    values = c("FALSE" = "#B12A90",   # main effects
               "TRUE"  = "#FCA636"),  # interactions (plasma orange)
    guide = "none"
  )+
 # fill_magma_aic +
  labs(
    x = expression("Model contribution (" * Delta * AIC * ")"),
    y = NULL,
    fill = expression(bold(Delta*AIC))) +
  theme_minimal(base_size = 15) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.y = element_blank(),
    legend.title = element_text(face = "bold"),
    legend.position = "right",
    legend.box.margin = margin(l = -10),   # pull legend closer (try -5 to -15)
    legend.margin = margin(l = -5),
    plot.margin = margin(8.5, 0, 5.5, 0),
    axis.ticks.y = element_line(colour = "black"),
    axis.ticks.length = unit(4, "pt")
  )

top_panel <- (plot_spacer() | p_imp_mag2 |plot_spacer()| p_aic) + plot_layout(widths = c(0.1, 3.2,0.7, 3.2))

# -----------------------------
# Predictions (population-level)
# IMPORTANT: correct CIs -> predict(type="link") then exp()
# -----------------------------
bg0   <- levels(dat$Botanical_garden)[1]
week0 <- median(dat$Week, na.rm = TRUE)
pair0 <- levels(dat$Pair)[1]
exclude_all <- c("s(Pair)", "s(Week,Botanical_garden)", "Botanical_garden")

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

prF <- predict(m_final_ar1, newdata = newF, type = "link", se.fit = TRUE, exclude = exclude_all)
newF$fit <- exp(prF$fit)
newF$lwr <- exp(prF$fit - 1.96 * prF$se.fit)
newF$upr <- exp(prF$fit + 1.96 * prF$se.fit)
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

prW <- predict(m_final_ar1, newdata = newW, type = "link", se.fit = TRUE, exclude = exclude_all)
newW$fit <- exp(prW$fit)
newW$lwr <- exp(prW$fit - 1.96 * prW$se.fit)
newW$upr <- exp(prW$fit + 1.96 * prW$se.fit)
newW$Poll_group <- factor(newW$log_poll_z, levels = poll_vals, labels = poll_labs)

y_lim <- range(c(newF$lwr, newF$upr, newW$lwr, newW$upr), na.rm = TRUE)

p_f_abundance <- ggplot(
  newF,
  aes(x = log_flower_z, y = fit,
      colour = Poll_group, linetype = Poll_group)
) +
  geom_line(linewidth = 1.5) +
  geom_ribbon(
    aes(ymin = lwr, ymax = upr, fill = Poll_group),
    alpha = 0.18,
    colour = NA
  ) +
  coord_cartesian(ylim = y_lim) +
  scale_linetype_manual(
    breaks = c("Low", "High", "Very high"),
    values = c("Low" = "dotted",
               "High" = "dashed",
               "Very high" = "solid")
  ) +
  scale_colour_manual(
    breaks = c("Low", "High", "Very high"),
    values = c("Low" = "#B35806",
               "High" = "#F46D43",
               "Very high" = "#FCA636")
  ) +
  scale_fill_manual(
    breaks = c("Low", "High", "Very high"),
    values = c("Low" = "#B35806",
               "High" = "#F46D43",
               "Very high" = "#FCA636"),
    guide = "none"
  ) +
  theme_bw(base_size = 15) +
  labs(
    x = "Flower abundance (z)",
    y = "Predicted visitation",
    linetype = "Pollinator abundance",
    colour = "Pollinator abundance",
    subtitle = "Flower abund. × Pollinator abund."
  ) +
  theme(
    plot.subtitle = element_text(face = "bold"),
    legend.title = element_text(face = "bold"),
    panel.border = element_rect(size = 1.2),
    legend.key.width = unit(1.6, "cm"),
    legend.key = element_blank(),
    legend.background = element_blank()
  ) +
  guides(
    colour = "none",
    fill = "none",
    linetype = guide_legend(
      override.aes = list(
        colour = c("#B35806", "#F46D43", "#FCA636"),
        linewidth = 1.5,
        fill = NA
      )
    )
  )

p_f_width <- ggplot(
  newW,
  aes(x = Flower_width_z, y = fit,
      colour = Poll_group, fill = Poll_group, linetype = Poll_group)
) +
  geom_line(linewidth = 1.5) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.18, colour = NA) +
  scale_linetype_manual(
    breaks = c("Low", "High", "Very high"),
    values = c("Low" = "dotted",
               "High" = "dashed",
               "Very high" = "solid")
  ) +
  scale_colour_manual(
    breaks = c("Low", "High", "Very high"),
    values = c("Low" = "#B35806",
               "High" = "#F46D43",
               "Very high" = "#FCA636")
  ) +
  scale_fill_manual(
    breaks = c("Low", "High", "Very high"),
    values = c("Low" = "#B35806",
               "High" = "#F46D43",
               "Very high" = "#FCA636"),
    guide = "none"
  ) +
  guides(
    colour = guide_legend(override.aes = list(fill = NA)),
    linetype = guide_legend(override.aes = list(fill = NA))
  ) +
  coord_cartesian(ylim = y_lim) +
  theme_bw(base_size = 15) +
  labs(
    x = "Flower width (z)",
    y = "Predicted visitation",
    linetype = "Pollinator\nabundance",
    colour = "Pollinator\nabundance",
    subtitle = "Flower width × Pollinator abund."
  ) +
  theme(
    plot.subtitle = element_text(face = "bold"),
    legend.title = element_text(face = "bold"),
    panel.border = element_rect(size = 1.2),
    legend.key.width = unit(1.6, "cm"),
    legend.key = element_rect(fill = NA, colour = NA),
    legend.background = element_rect(fill = NA, colour = NA)
  )

# ONE legend only (keep right plot legend)
p_f_abundance_noleg <- p_f_abundance + theme(legend.position = "none")
p_f_width_leg <- p_f_width + theme(legend.position = "right")

panel2 <- plot_spacer() +
  p_f_abundance_noleg +
  plot_spacer() +
  p_f_width_leg +
  plot_layout(widths = c(0.5, 10, 1, 10))

# -----------------------------
# Final layout
# -----------------------------
top_panel / panel2

saveRDS(top_panel, "Data/Working_files/panel1_gam.rds")
saveRDS(panel2, "Data/Working_files/panel2_gam.rds")

