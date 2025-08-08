
library(tidyverse)

name_tapnet_maxnodf_full_season_csv <- "Data/Working_files/results_maxnodf_TAPNET_FULL_SEASON_traits.csv"
name_tapnet_maxnodf_partial_season_csv <- "Data/Working_files/results_maxnodf_TAPNET_EARLY_MID_LATE_SEASON_traits.csv"
name_tapnet_maxnodf_ALL_WEEK_csv <- "Data/Working_files/results_maxnodf_TAPNET_ALL_WEEK_traits.csv"


success_full_season_maxnodf <- readr::read_csv(name_tapnet_maxnodf_full_season_csv) %>%
  dplyr::select(Botanical_garden, Success, Type, Season, Week) %>% unique()
success_partial_season_maxnodf <- readr::read_csv(name_tapnet_maxnodf_partial_season_csv) %>%
  dplyr::select(Botanical_garden, Success, Type, Season, Week) %>% unique()
success_all_weeks_maxnodf <- readr::read_csv(name_tapnet_maxnodf_ALL_WEEK_csv) %>%
  dplyr::select(Botanical_garden, Success, Type, Season, Week) %>% unique()

success_all_weeks_maxnodf %>% filter(Success<200)
