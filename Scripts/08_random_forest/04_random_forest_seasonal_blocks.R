
rf_dat = readRDS("Data/Working_files/rf_dat.rds")

rf_dat = rf_dat %>% 
  select(!Mean_Temperature) %>% 
  rename(Flower_width = t_plant) %>%
  mutate(
    log_visit = log1p(VisitRate)
  ) %>%
  select(-VisitRate)

rf_dat <- rf_dat %>%
  group_by(Botanical_garden) %>%
  arrange(Week, .by_group = TRUE) %>%
  mutate(
    Season_group = ntile(Week, 3),
    Season = case_when(
      Season_group == 1 ~ "Early",
      Season_group == 2 ~ "Mid",
      Season_group == 3 ~ "Late"
    ),
    Season = factor(Season, levels = c("Early","Mid","Late"))
  ) %>%
  select(-Season_group) %>%
  ungroup()

library(purrr)

library(purrr)

rf_models_season <- rf_dat %>%
  group_split(Season) %>%
  set_names(levels(rf_dat$Season)) %>%
  map(~ ranger(
    log_visit ~ . - Season,
    data = .x,
    num.trees = 1500,
    importance = "permutation",
    seed = 123
  ))


var_imp_season <- map2_df(
  rf_models_season,
  names(rf_models_season),
  ~ tibble(
    variable = names(.x$variable.importance),
    importance = .x$variable.importance,
    Season = .y
  )
)

var_imp_season <- var_imp_season %>%
  group_by(Season) %>%
  mutate(
    importance_rel = importance / sum(importance)
  ) %>%
  ungroup()

ggplot(var_imp_season,
       aes(variable, importance_rel, fill = Season)) +
  
  geom_col() +
  
  facet_wrap(~Season, scales = "fixed") +
  
  coord_flip() +
  
  theme_classic() +
  
  labs(
    x = NULL,
    y = "Relative variable importance"
  )

