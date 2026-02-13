# -----------------------------
# 3) Weekly varying effects (beta(t)) via prediction differences
#    Effect per +1 SD (link scale), by garden and week
#    IMPORTANT: evaluate slopes at garden-week mean context to allow differences across gardens
# -----------------------------

pair0 <- levels(weekly_data$Pair)[1]

# Context (means) per garden x week
means_gw <- weekly_data %>%
  group_by(Botanical_garden, Week) %>%
  summarise(
    mean_flower = mean(log_flower_z, na.rm = TRUE),
    mean_poll   = mean(log_poll_z,   na.rm = TRUE),
    .groups = "drop"
  )

# Base grid uses garden-week means (so interaction matters differently across gardens)
base_grid <- means_gw %>%
  mutate(
    Pair = pair0,
    log_flower_z = mean_flower,
    log_poll_z   = mean_poll
  ) %>%
  select(Week, Botanical_garden, Pair, log_flower_z, log_poll_z)

# --- Flower weekly effect (hold poll at mean_poll for that garden-week) ---
grid_f_up <- base_grid %>% mutate(log_flower_z = log_flower_z + 1)
grid_f_dn <- base_grid %>% mutate(log_flower_z = log_flower_z - 1)

pred_f_up <- predict(m_temporal, newdata = grid_f_up, type = "link", se.fit = TRUE)
pred_f_dn <- predict(m_temporal, newdata = grid_f_dn, type = "link", se.fit = TRUE)

flower_time <- base_grid %>%
  mutate(
    effect_link = (pred_f_up$fit - pred_f_dn$fit) / 2,   # per +1 SD
    se_link     = sqrt(pred_f_up$se.fit^2 + pred_f_dn$se.fit^2) / 2,
    lwr         = effect_link - 1.96 * se_link,
    upr         = effect_link + 1.96 * se_link,
    Variable    = "Flower"
  )

# --- Pollinator weekly effect (hold flower at mean_flower for that garden-week) ---
grid_p_up <- base_grid %>% mutate(log_poll_z = log_poll_z + 1)
grid_p_dn <- base_grid %>% mutate(log_poll_z = log_poll_z - 1)

pred_p_up <- predict(m_temporal, newdata = grid_p_up, type = "link", se.fit = TRUE)
pred_p_dn <- predict(m_temporal, newdata = grid_p_dn, type = "link", se.fit = TRUE)

poll_time <- base_grid %>%
  mutate(
    effect_link = (pred_p_up$fit - pred_p_dn$fit) / 2,   # per +1 SD
    se_link     = sqrt(pred_p_up$se.fit^2 + pred_p_dn$se.fit^2) / 2,
    lwr         = effect_link - 1.96 * se_link,
    upr         = effect_link + 1.96 * se_link,
    Variable    = "Pollinator"
  )

beta_time <- bind_rows(flower_time, poll_time)

# Quick diagnostic plot (should now differ across gardens)
p_weekly_curves <- ggplot(beta_time, aes(x = Week, y = effect_link, colour = Variable, fill = Variable)) +
  geom_line(linewidth = 1.1) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.2, colour = NA) +
  facet_wrap(~ Botanical_garden) +
  theme_bw() +
  labs(
    y = "Effect per +1 SD (link scale)",
    title = "Temporal variation in importance (weekly effects; evaluated at garden-week means)"
  )

print(p_weekly_curves)
