
library(tidyverse)

name_tapnet_full_season_csv <- "Data/Working_files/results_TAPNET_FULL_SEASON_traits.csv"
name_tapnet_partial_season_csv <- "Data/Working_files/results_TAPNET_EARLY_MID_LATE_SEASON_traits.csv"
name_tapnet_ALL_WEEK_csv <- "Data/Working_files/results_TAPNET_ALL_WEEK_WITH_REGULAR_traits.csv"

# Add type labels and fill missing columns
full_season <- readr::read_csv(name_tapnet_full_season_csv) %>%
  mutate(Season = "Full", Week = NA, Type = "Full")

partial_season <- readr::read_csv(name_tapnet_partial_season_csv) %>%
  mutate(Week = NA, Type = "Partial")

all_weeks <- readr::read_csv(name_tapnet_ALL_WEEK_csv) %>%
  mutate(Type = "Weekly")

# Bind all together
tapnet_all_levels <- bind_rows(full_season, partial_season, all_weeks)

tapnet_all_levels$Variable %>% unique()

# 1. Filter for the relevant variables
structural_vars <- c("connectance", "H2", "NODF", "weighted NODF")

tapnet_long <- tapnet_all_levels %>%
  filter(str_detect(Variable, paste(structural_vars, collapse = "|"))) %>%
  filter(str_detect(Variable, "^Mean|^q2.5|^q97.5")) %>%
  separate(Variable, into = c("Stat", "Metric"), sep = " ", extra = "merge") %>%
  pivot_wider(names_from = Stat, values_from = Value) %>%
  filter(!is.na(Mean))  # keep only valid rows


observed_long <- tapnet_all_levels %>%
  filter(str_detect(Variable, "^Observed")) %>%
  separate(Variable, into = c("Stat", "Metric"), sep = " ", extra = "merge") %>%
  rename(Observed = Value) %>%
  filter(Metric %in% structural_vars)

plot_data <- left_join(tapnet_long, observed_long,
                       by = c("Botanical_garden", "Season", "Week", "Metric", "Type"))

plot_data <- plot_data %>%
  mutate(Week_plot = case_when(
    Type == "Full" ~ -2,
    Type == "Partial" & Season == "Early" ~ 1,
    Type == "Partial" & Season == "Mid" ~ 4,
    Type == "Partial" & Season == "Late" ~ 7,
    TRUE ~ as.numeric(Week)
  ))

ggplot(plot_data %>% filter(Botanical_garden == "Jena"), aes(x = Week_plot, color = Season)) +
  geom_errorbar(aes(ymin = `q2.5`, ymax = `q97.5`),
                width = 0.3,
                position = position_dodge(width = 0.5),
                alpha = 0.6) +
  geom_point(aes(y = Mean, shape = Type),
             size = 3,
             position = position_dodge(width = 0.5)) +
  geom_point(aes(y = Observed),
             shape = 1, stroke = 1.2, size = 3,
             position = position_dodge(width = 0.5)) +
  facet_wrap(~ Metric, scales = "free_y") +
  scale_shape_manual(values = c("Weekly" = 16, "Partial" = 17, "Full" = 15)) +
  labs(title = "Network metrics with uncertainty and observed values for Jena",
       y = "Metric value",
       x = "Week (pseudo-x for summaries)") +
  theme_minimal()


ggplot(plot_data %>% filter(Botanical_garden == "Halle"), aes(x = Week_plot, color = Season)) +
  geom_errorbar(aes(ymin = `q2.5`, ymax = `q97.5`),
                width = 0.3,
                position = position_dodge(width = 0.5),
                alpha = 0.6) +
  geom_point(aes(y = Mean, shape = Type),
             size = 3,
             position = position_dodge(width = 0.5)) +
  geom_point(aes(y = Observed),
             shape = 1, stroke = 1.2, size = 3,
             position = position_dodge(width = 0.5)) +
  facet_wrap(~ Metric, scales = "free_y") +
  scale_shape_manual(values = c("Weekly" = 16, "Partial" = 17, "Full" = 15)) +
  labs(title = "Network metrics with uncertainty and observed values for Halle",
       y = "Metric value",
       x = "Week (pseudo-x for summaries)") +
  theme_minimal()

ggplot(plot_data %>% filter(Botanical_garden == "Leipzig"), aes(x = Week_plot, color = Season)) +
  geom_errorbar(aes(ymin = `q2.5`, ymax = `q97.5`),
                width = 0.3,
                position = position_dodge(width = 0.5),
                alpha = 0.6) +
  geom_point(aes(y = Mean, shape = Type),
             size = 3,
             position = position_dodge(width = 0.5)) +
  geom_point(aes(y = Observed),
             shape = 1, stroke = 1.2, size = 3,
             position = position_dodge(width = 0.5)) +
  facet_wrap(~ Metric, scales = "free_y") +
  scale_shape_manual(values = c("Weekly" = 16, "Partial" = 17, "Full" = 15)) +
  labs(title = "Network metrics with uncertainty and observed values for Leipzig",
       y = "Metric value",
       x = "Week (pseudo-x for summaries)") +
  theme_minimal()


# Results for delta--------------------------------------------------------
delta_data <- tapnet_all_levels %>%
  filter(Variable == "delta") %>%
  mutate(Week_plot = case_when(
    Type == "Full" ~ -2,
    Type == "Partial" & Season == "Early" ~ 1,
    Type == "Partial" & Season == "Mid" ~ 4,
    Type == "Partial" & Season == "Late" ~ 7,
    TRUE ~ as.numeric(Week)
  ))

# 2. Plot
ggplot(delta_data,
       aes(x = Week_plot, y = Value, color = Season, shape = Type)) +
  geom_point(size = 3) +
  facet_wrap(~ Botanical_garden) +
  scale_shape_manual(values = c("Weekly" = 16, "Partial" = 17, "Full" = 15)) +
  labs(title = "Delta across weeks and seasons",
       x = "Week (pseudo-x for summaries)",
       y = "Delta value") +
  theme_minimal() +
  theme(legend.position = "bottom")


# Results for tmatch_width_pem-----------------------------------------------
tmatch_width_pem_data <- tapnet_all_levels %>%
  filter(Variable == "tmatch_width_pem") %>%
  mutate(Week_plot = case_when(
    Type == "Full" ~ -2,
    Type == "Partial" & Season == "Early" ~ 1,
    Type == "Partial" & Season == "Mid" ~ 4,
    Type == "Partial" & Season == "Late" ~ 7,
    TRUE ~ as.numeric(Week)
  ))

# 2. Plot
ggplot(tmatch_width_pem_data,
       aes(x = Week_plot, y = Value, color = Season, shape = Type)) +
  geom_point(size = 3) +
  scale_y_log10() +
  facet_wrap(~ Botanical_garden) +
  scale_shape_manual(values = c("Weekly" = 16, "Partial" = 17, "Full" = 15)) +
  labs(title = "tmatch_width_pem across weeks and seasons",
       x = "Week (pseudo-x for summaries)",
       y = "tmatch_width_pem") +
  theme_minimal() +
  theme(legend.position = "bottom")


# Results for tmatch_width_obs------------------------------------------
tmatch_width_obs_data <- tapnet_all_levels %>%
  filter(Variable == "tmatch_width_obs") %>%
  mutate(Week_plot = case_when(
    Type == "Full" ~ -2,
    Type == "Partial" & Season == "Early" ~ 1,
    Type == "Partial" & Season == "Mid" ~ 4,
    Type == "Partial" & Season == "Late" ~ 7,
    TRUE ~ as.numeric(Week)
  ))

# 2. Plot
ggplot(tmatch_width_obs_data,
       aes(x = Week_plot, y = Value, color = Season, shape = Type)) +
  geom_point(size = 3) +
  scale_y_log10() +
  facet_wrap(~ Botanical_garden) +
  scale_shape_manual(values = c("Weekly" = 16, "Partial" = 17, "Full" = 15)) +
  labs(title = "tmatch_width_obs across weeks and seasons",
       x = "Week (pseudo-x for summaries)",
       y = "tmatch_width_obs") +
  theme_minimal() +
  theme(legend.position = "bottom")


# Results for tmatch_bc_sim_web------------------------------------------
# Similarity between fitted and observed network expressed as Bray-Curtis 
bc_sim_web_data <- tapnet_all_levels %>%
  filter(Variable == "bc_sim_web") %>%
  mutate(Week_plot = case_when(
    Type == "Full" ~ -2,
    Type == "Partial" & Season == "Early" ~ 1,
    Type == "Partial" & Season == "Mid" ~ 4,
    Type == "Partial" & Season == "Late" ~ 7,
    TRUE ~ as.numeric(Week)
  ))

# 2. Plot
ggplot(bc_sim_web_data,
       aes(x = Week_plot, y = Value, color = Season, shape = Type)) +
  geom_point(size = 3) +
  facet_wrap(~ Botanical_garden) +
  scale_shape_manual(values = c("Weekly" = 16, "Partial" = 17, "Full" = 15)) +
  labs(title = "bc_sim_web across weeks and seasons",
       x = "Week (pseudo-x for summaries)",
       y = "bc_sim_web") +
  theme_minimal() +
  theme(legend.position = "bottom")


# Results for cor_web------------------------------------------
# Correlation between fitted and observed number of interactions, 
# expressed as Spearman correlation

cor_web_data <- tapnet_all_levels %>%
  filter(Variable == "cor_web") %>%
  mutate(Week_plot = case_when(
    Type == "Full" ~ -2,
    Type == "Partial" & Season == "Early" ~ 1,
    Type == "Partial" & Season == "Mid" ~ 4,
    Type == "Partial" & Season == "Late" ~ 7,
    TRUE ~ as.numeric(Week)
  ))

# 2. Plot
ggplot(cor_web_data,
       aes(x = Week_plot, y = Value, color = Season, shape = Type)) +
  geom_point(size = 3) +
  facet_wrap(~ Botanical_garden) +
  scale_shape_manual(values = c("Weekly" = 16, "Partial" = 17, "Full" = 15)) +
  labs(title = "cor_web across weeks and seasons",
       x = "Week (pseudo-x for summaries)",
       y = "cor_web") +
  theme_minimal() +
  theme(legend.position = "bottom")
