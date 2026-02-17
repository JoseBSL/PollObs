############################################################ #
# Model data with GAM with temporal variation of abundances:
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
m_final = gam(
  Total_pair_interactions ~ 
    log_flower_z + log_poll_z +
    T_gauss_z + Overlap_days_z +
    Botanical_garden +
    s(Week, Botanical_garden, bs="fs", k=8) +
    ti(log_flower_z, Week, k=c(5,8)) +
    ti(log_poll_z,   Week, k=c(5,8)) +
    s(Pair, bs="re") +
    offset(log(Total_time_plant_week)),
  family = nb(theta=2, link="log"),
  data = weekly_data,
  method = "REML")
############################################################ #
concurvity(m_final, full=TRUE)
summary(m_final)$s.table
gam.check(m_final)
sim_res <- simulateResiduals(
  fittedModel = m_final,
  n = 500)

plot(sim_res)
testUniformity(sim_res)
testDispersion(sim_res)
testOutliers(sim_res, type="bootstrap")
# ---- Base prediction grid (weeks x gardens; drivers at mean; effort = 1) ----
week_seq <- sort(unique(weekly_data$Week))
gardens  <- levels(weekly_data$Botanical_garden)

base_grid <- expand.grid(
  Week = week_seq,
  Botanical_garden = gardens
) %>%
  mutate(
    log_flower_z = 0,
    log_poll_z = 0,
    T_gauss_z = 0,
    Overlap_days_z = 0,
    Total_time_plant_week = 1,
    Pair = levels(weekly_data$Pair)[1]
  )

# simulate coefficients
set.seed(1)
B <- 400
coef_sims <- MASS::mvrnorm(B, mu = coef(m_final), Sigma = vcov(m_final))

# helper: compute integrated ±1 SD effect using lpmatrix
effect_pm1_lpmat <- function(var){
  up <- base_grid; dn <- base_grid
  up[[var]] <-  1
  dn[[var]] <- -1
  
  X_up <- predict(m_final, newdata = up, type = "lpmatrix", exclude = "s(Pair)")
  X_dn <- predict(m_final, newdata = dn, type = "lpmatrix", exclude = "s(Pair)")
  
  # each simulation: eta = X %*% beta
  eta_up <- X_up %*% t(coef_sims)   # rows = obs, cols = sims
  eta_dn <- X_dn %*% t(coef_sims)
  
  # effect per obs per sim on link scale
  eff <- abs(eta_up - eta_dn) / 2
  
  # integrate across Week x Garden for each sim
  as.numeric(colMeans(eff))
}

vars_all <- c(
  log_flower_z   = "Flower abundance",
  log_poll_z     = "Pollinator abundance",
  T_gauss_z      = "Trait matching",
  Overlap_days_z = "Phenology overlap"
)

imp_sims <- lapply(names(vars_all), function(v){
  vals <- effect_pm1_lpmat(v)
  data.frame(Variable = vars_all[[v]], value = vals)
}) |> bind_rows()

importance_ci <- imp_sims %>%
  group_by(Variable) %>%
  summarise(
    Effect = mean(value),
    lwr = quantile(value, 0.025),
    upr = quantile(value, 0.975),
    .groups = "drop"
  ) %>%
  arrange(Effect)

importance_ci

importance_ci <- importance_ci %>%
  mutate(Variable = factor(Variable, levels = Variable))

pA = ggplot(importance_ci, aes(x = Variable, y = Effect)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.15, linewidth = 1) +
  geom_hline(yintercept = 0, linetype = 2) +
  coord_flip() +
  theme_bw(base_size = 12) +
  labs(
    title = "A) Overall relative importance",
    x = NULL,
    y = "Integrated effect (±1 SD; link scale, 95% CI)"
  )

# =========================================================
# Panel B: Seasonal change in importance for Flower & Poll
# (shared across gardens by model structure)
# =========================================================
vars_dyn <- c(
  log_flower_z = "Flower abundance",
  log_poll_z   = "Pollinator abundance"
)

# helper: weekly beta(t) for ±1 SD using lpmatrix
weekly_effect_ci_lpmat <- function(var){
  up <- base_grid; dn <- base_grid
  up[[var]] <-  1
  dn[[var]] <- -1
  
  X_up <- predict(m_final, newdata = up, type = "lpmatrix", exclude = "s(Pair)")
  X_dn <- predict(m_final, newdata = dn, type = "lpmatrix", exclude = "s(Pair)")
  
  # eta matrices: (n_obs x B)
  eta_up <- X_up %*% t(coef_sims)
  eta_dn <- X_dn %*% t(coef_sims)
  
  eff <- (eta_up - eta_dn) / 2  # (n_obs x B), link scale
  
  # collapse over gardens per week for each sim
  idx_by_week <- split(seq_len(nrow(base_grid)), base_grid$Week)
  
  out <- lapply(names(idx_by_week), function(w){
    idx <- idx_by_week[[w]]
    eff_w <- colMeans(eff[idx, , drop=FALSE])  # length B
    data.frame(
      Week = as.numeric(w),
      Effect = mean(eff_w),
      lwr = quantile(eff_w, 0.025),
      upr = quantile(eff_w, 0.975)
    )
  }) |> bind_rows()
  
  out$Variable <- vars_dyn[[var]]
  out
}

weekly_ci <- bind_rows(
  weekly_effect_ci_lpmat("log_flower_z"),
  weekly_effect_ci_lpmat("log_poll_z")
)

# Plot with ribbons (now they will have width)
pB <- ggplot(weekly_ci, aes(x = Week, y = Effect, colour = Variable, fill = Variable)) +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.2, colour = NA) +
  geom_line(linewidth = 1.2) +
  theme_bw(base_size = 12) +
  labs(
    title = "B) Seasonal change in importance",
    x = "Week",
    y = "Effect per +1 SD (link scale, 95% CI)"
  ) +
  theme(legend.position = "top")

pB

#========================================================= 
library(patchwork) 
pA + pB + plot_layout(widths = c(1, 1.4)) 
