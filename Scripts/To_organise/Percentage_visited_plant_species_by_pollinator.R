
library(dplyr)
library(tidyr)
library(lubridate)
library(bipartite)

# ONLY FOCALS + RD OBSERVATIONS

#Load data
raw_data = readRDS("Data/Working_files/interaction_data.rds")
interaction_data <- raw_data  %>% filter(!is.na(Interactions),
                                         !is.na(Floral_abundance),
                                         Pollinator != "None") %>% 
  select(Botanical_garden, Plant_family, 
         Pollinator_family, Date_time, 
         Interactions, Floral_abundance) %>% 
  rename(Plant = Plant_family, Pollinator = Pollinator_family) %>% 
  mutate(Date = as.Date(Date_time)) %>% 
  mutate(Week = lubridate::week(Date)) %>% 
  select(-Date_time, -Date) %>% filter(!is.na(Pollinator)) %>% ungroup()

############################################################################
# Weekly estimations
############################################################################

total_plant_Sp_week <- interaction_data %>%
  dplyr::select(Botanical_garden, Plant, Week) %>% unique() %>%
  group_by(Botanical_garden, Week) %>% count() %>%
  rename(Total_plant_richness = n) %>% ungroup()

total_visited_plant_Sp_by_poll_week <- interaction_data %>%
  dplyr::select(Botanical_garden, Plant, Pollinator, Week) %>% unique() %>%
  group_by(Botanical_garden, Pollinator, Week) %>% count() %>%
  rename(Total_visited_plant_richness = n) %>% ungroup()

data_visited_plant_Sp_by_poll_week <- total_visited_plant_Sp_by_poll_week %>%
  left_join(total_plant_Sp_week, 
            by = c("Botanical_garden", "Week")) %>%
  mutate(percentage_vivited_plant_sp = Total_visited_plant_richness/Total_plant_richness)

############################################################################
# Flowering season estimation
############################################################################

total_plant_Sp_season <- interaction_data %>%
  dplyr::select(Botanical_garden, Plant) %>% unique() %>%
  group_by(Botanical_garden) %>% count() %>%
  rename(Total_plant_richness = n) %>% ungroup()

total_visited_plant_Sp_by_poll_season <- interaction_data %>%
  dplyr::select(Botanical_garden, Plant, Pollinator) %>% unique() %>%
  group_by(Botanical_garden, Pollinator) %>% count() %>%
  rename(Total_visited_plant_richness = n) %>% ungroup()

data_visited_plant_Sp_by_poll_season <- total_visited_plant_Sp_by_poll_season %>%
  left_join(total_plant_Sp_season, 
            by = c("Botanical_garden")) %>%
  mutate(percentage_vivited_plant_sp = Total_visited_plant_richness/Total_plant_richness)

############################################################################
# save results
############################################################################

readr::write_csv(data_visited_plant_Sp_by_poll_week,"Data/Working_files/data_visited_plant_FAMILY_by_poll_FAMILY_week.csv")
readr::write_csv(data_visited_plant_Sp_by_poll_season,"Data/Working_files/data_visited_plant_FAMILY_by_poll_FAMILY_season.csv")
