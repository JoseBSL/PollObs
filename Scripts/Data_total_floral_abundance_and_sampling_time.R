
library(tidyverse)
library(lubridate)

#Load data
raw_data = readRDS("Data/Working_files/interaction_data.rds")

data_floral_ab_sampling_time <- raw_data  %>% filter(!is.na(Interactions),
                                                     !is.na(Floral_abundance),
                                                     Pollinator != "None") %>% 
  mutate(Individual = paste0(Sampling,Random_census_stop)) %>% 
  dplyr::select(Botanical_garden, Plant_family, Plant,
         Pollinator_family, Date_time, 
         Floral_abundance, Total_time_species, Individual) %>% 
  mutate(Date = as.Date(Date_time)) %>% 
  mutate(Week = lubridate::week(Date)) %>% 
  dplyr::select(-Date_time, -Date) %>% filter(!is.na(Pollinator_family)) %>% ungroup() %>% 
  dplyr::select(Botanical_garden,Plant_family, Plant, Week, 
         Floral_abundance, Total_time_species, Individual) %>% 
  unique()

data_floral_ab_sampling_time_by_family <- data_floral_ab_sampling_time %>%
  group_by(Botanical_garden, Plant_family, Week) %>%
  summarise(
    Total_floral_abundance_family = sum(Floral_abundance),
    Total_sampling_time_family = sum(Total_time_species)
  ) %>% ungroup()

data_floral_ab_sampling_time_by_sp <- data_floral_ab_sampling_time %>%
  group_by(Botanical_garden, Plant, Week) %>%
  summarise(
    Total_floral_abundance = sum(Floral_abundance),
    Total_sampling_time = sum(Total_time_species)
  ) %>% 
  ungroup()

readr::write_csv(data_floral_ab_sampling_time_by_family,"Data/Working_files/data_floral_ab_sampling_time_by_family.csv")
readr::write_csv(data_floral_ab_sampling_time_by_sp,"Data/Working_files/data_floral_ab_sampling_time_by_sp.csv")
