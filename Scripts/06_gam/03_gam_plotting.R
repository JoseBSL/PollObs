library(dplyr)

pair0 <- levels(factor(weekly_data$Pair))[1]

# Contexto ecológico: medias por jardín × semana
means_gw <- weekly_data %>%
  group_by(Botanical_garden, Week) %>%
  summarise(
    mean_flower = mean(log_flower_z, na.rm = TRUE),
    mean_poll   = mean(log_poll_z,   na.rm = TRUE),
    mean_trait  = mean(T_gauss_z,    na.rm = TRUE),
    .groups = "drop"
  )

base_grid_ctx <- means_gw %>%
  mutate(
    Pair = pair0,
    log_flower_z = mean_flower,
    log_poll_z   = mean_poll,
    T_gauss_z    = mean_trait
  ) %>%
  select(Week, Botanical_garden, Pair, log_flower_z, log_poll_z, T_gauss_z)

calc_effect <- function(model, base_grid, var, label){
  up <- base_grid
  dn <- base_grid
  up[[var]] <- up[[var]] + 1
  dn[[var]] <- dn[[var]] - 1
  
  p_up <- predict(model, newdata = up, type = "link", se.fit = TRUE)
  p_dn <- predict(model, newdata = dn, type = "link", se.fit = TRUE)
  
  base_grid %>%
    mutate(
      effect_link = (p_up$fit - p_dn$fit) / 2,
      se_link     = sqrt(p_up$se.fit^2 + p_dn$se.fit^2) / 2,
      lwr         = effect_link - 1.96 * se_link,
      upr         = effect_link + 1.96 * se_link,
      Variable    = label
    )
}

flower_time <- calc_effect(m_temporal, base_grid_ctx, "log_flower_z", "Flower")
poll_time   <- calc_effect(m_temporal, base_grid_ctx, "log_poll_z",   "Pollinator")
trait_time  <- calc_effect(m_temporal, base_grid_ctx, "T_gauss_z",    "Trait_matching")

beta_time <- bind_rows(flower_time, poll_time, trait_time)


library(ggplot2)
library(tidyr)

season_levels <- c("Early","Mid","Late")

weekly_effect <- beta_time %>%
  select(-any_of("Season")) %>%
  group_by(Botanical_garden) %>%
  mutate(
    Season = cut(
      Week,
      breaks = quantile(Week, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE),
      labels = season_levels,
      include.lowest = TRUE
    ),
    Season = factor(Season, levels = season_levels)
  ) %>%
  ungroup() %>%
  filter(is.finite(effect_link))

seasonal_effect <- weekly_effect %>%
  group_by(Botanical_garden, Variable, Season) %>%
  summarise(effect_link = mean(effect_link), .groups = "drop")

full_effect <- weekly_effect %>%
  group_by(Botanical_garden, Variable) %>%
  summarise(effect_link = mean(effect_link), .groups = "drop")

dummy_weekly <- weekly_effect %>%
  distinct(Botanical_garden, Variable, Season) %>%
  mutate(effect_link = 0) %>%
  slice(1)

panel_gam <- ggplot(weekly_effect, aes(x = Season, y = effect_link, fill = Season)) +
  geom_violin(alpha = 0.35, width = 0.55, colour = NA, scale = "width", trim = TRUE, cut = 0) +
  geom_hline(
    data = full_effect,
    aes(yintercept = effect_link, linetype = "Full season"),
    color = "black", linewidth = 0.4, inherit.aes = FALSE
  ) +
  geom_dotplot(
    aes(fill = Season),
    binaxis = "y", stackdir = "center",
    dotsize = 1.1, alpha = 0.85, binwidth = 0.06, stackratio = 1.25
  ) +
  geom_point(
    data = dummy_weekly,
    aes(x = Season, y = effect_link, size = "Weekly"),
    inherit.aes = FALSE, alpha = 0
  ) +
  geom_point(
    data = seasonal_effect,
    aes(x = Season, y = effect_link, size = "Seasonal"),
    inherit.aes = FALSE, shape = 23, stroke = 0.5, alpha = 0.95,
    position = position_nudge(x = +0.5)
  ) +
  facet_grid(Variable ~ Botanical_garden, scales = "free_y") +
  theme_minimal() +
  coord_cartesian(clip = "off") +
  scale_size_manual(
    name   = "Temporal complexity",
    breaks = c("Weekly", "Seasonal"),
    labels = c("Weekly", "Weekly aggregated"),
    values = c("Weekly" = 1.4, "Seasonal" = 2.4)
  ) +
  scale_linetype_manual(
    name   = "Temporal complexity",
    breaks = c("Full season"),
    values = c("Full season" = "dashed")
  ) +
  guides(
    fill  = "none",
    color = "none",
    size = guide_legend(
      order = 1, title = "Temporal complexity",
      override.aes = list(shape = c(21, 23), color = c("black", "black"),
                          fill = c("grey70", "grey70"), alpha = c(1, 1))
    ),
    linetype = guide_legend(
      order = 2, title = NULL,
      override.aes = list(color = "black", linewidth = 0.8)
    )
  ) +
  theme(
    panel.spacing = unit(1.2, "lines"),
    panel.border  = element_rect(color = "black", fill = NA, linewidth = 0.6),
    axis.title    = element_text(face = "bold"),
    axis.text.x   = element_text(angle = 0, hjust = 0.5),
    plot.title    = element_text(face = "bold")
  ) +
  ylab("Effect per +1 SD (link scale)") +
  xlab(NULL) +
  ggtitle("GAM importance – weekly, seasonal, and full season")

print(panel_gam)
