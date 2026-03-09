

shap_df <- shap_values %>%
  as.data.frame() %>%
  mutate(
    week = rf_explore$Week,
    season_block = case_when(
      week <= 22 ~ "early",
      week <= 28 ~ "mid",
      TRUE ~ "late"))

ggplot(shap_df, aes(season_block, Week)) +
  geom_boxplot() +
  theme_classic()


shap_week <- data.frame(
  Week = rf_explore$Week,
  shap_week = shap_values[, "Week"])

shap_week <- shap_week %>%
  mutate(
    season_block = case_when(
      Week <= 22 ~ "early",
      Week <= 28 ~ "mid",
      TRUE ~ "late"))

ggplot(shap_week, aes(season_block, shap_week)) +
  geom_boxplot()



plot_df <- data.frame(
  Week = rf_explore$Week,
  shap_week = shap_values[, "Week"]
)

ggplot(plot_df, aes(Week, shap_week)) +
  geom_point(alpha = 0.3) +
  geom_smooth() +
  theme_classic()


plot_df$garden <- rf_explore$Botanical_garden

ggplot(plot_df, aes(Week, shap_week, colour = garden)) +
  geom_smooth(se = FALSE)





rf_explore$pred_visit <- predict(rf_model_explore, data = rf_explore)$predictions

pred_week <- rf_explore %>%
  group_by(Botanical_garden, Week) %>%
  summarise(
    pred_visit = mean(pred_visit),
    .groups = "drop"
  )

ggplot(pred_week, aes(x = Week, y = pred_visit, colour = Botanical_garden)) +
  geom_line(size = 1.2) +
  theme_classic() +
  labs(
    x = "Week of season",
    y = "Predicted visitation rate",
    colour = "Botanical garden"
  )

ggplot(pred_week, aes(Week, pred_visit, colour = Botanical_garden)) +
  geom_line(size = 1.2) +
  geom_vline(xintercept = c(22, 28), linetype = "dashed") +
  theme_classic() +
  labs(
    x = "Week",
    y = "Predicted visitation rate"
  )

shap_week <- data.frame(
  Week = rf_explore$Week,
  shap_week = shap_values[, "Week"],
  garden = rf_explore$Botanical_garden
)

ggplot(shap_week, aes(Week, shap_week)) +
  geom_point(alpha = 0.2) +
  geom_smooth() +
  theme_classic() +
  labs(
    x = "Week",
    y = "SHAP value (effect of week)"
  )

ggplot(pred_week, aes(Week, pred_visit)) +
  geom_line(size = 1.2) +
  facet_wrap(~ Botanical_garden) +
  theme_classic() +
  labs(
    x = "Week",
    y = "Predicted visitation rate"
  )



rf_explore$pred <- predict(rf_model_explore, data = rf_explore)$predictions

pred_week <- rf_explore %>%
  group_by(Week) %>%
  summarise(pred = mean(pred))

ggplot(pred_week, aes(Week, pred)) +
  geom_line(size = 1.2) +
  theme_classic() +
  labs(
    x = "Week",
    y = "Predicted visitation rate"
  )

rf_explore <- rf_explore %>%
  group_by(Botanical_garden) %>%
  arrange(Week, .by_group = TRUE) %>%
  mutate(
    Season_group = ntile(Week, 3),
    Season = case_when(
      Season_group == 1 ~ "Early",
      Season_group == 2 ~ "Mid",
      Season_group == 3 ~ "Late"
    )
  ) %>%
  select(-Season_group) %>%
  ungroup()

rf_explore$Season <- factor(
  rf_explore$Season,
  levels = c("Early", "Mid", "Late")
)

shap_df <- shap_values %>%
  as.data.frame() %>%
  mutate(
    Season = rf_explore$Season
  )

library(tidyr)

shap_long <- shap_df %>%
  pivot_longer(
    -Season,
    names_to = "variable",
    values_to = "shap_value"
  )

season_importance <- shap_long %>%
  group_by(Season, variable) %>%
  summarise(
    importance = mean(abs(shap_value)),
    .groups = "drop"
  )

ggplot(season_importance,
       aes(x = reorder(variable, importance), y = importance)) +
  geom_col() +
  coord_flip() +
  facet_wrap(~ Season) +
  theme_classic() +
  labs(
    x = NULL,
    y = "Mean |SHAP value|"
  )




shap_df <- shap_values %>%
  as.data.frame() %>%
  mutate(
    season_block = rf_explore$season_block
  )
