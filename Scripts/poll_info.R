#Script to compute network metrics
#Things that we are interested at the moment
#Total interactions
#Richness per plant 
#Normalised degree
#Average pollinator specialization (selectivity)

#Load libraries
library(dplyr)

#Load raw data
raw_data = readRDS("Data/Working_files/interaction_data.rds")
interaction_data = raw_data  %>% filter(!is.na(Interactions),
                                         !is.na(Floral_abundance),
                                         Pollinator != "None") %>% 
  select(Botanical_garden, Plant_accepted_name, 
         Pollinator_accepted_name, Date_time, 
         Interactions, Floral_abundance) %>% 
  rename(Plant = Plant_accepted_name, Pollinator = Pollinator_accepted_name) %>% filter(!is.na(Pollinator))

#Load sampling time data (to correct if neccesary)
data_sampling_time_by_sp <- 
  readr::read_csv("Data/Working_files/data_floral_ab_sampling_time_by_sp.csv") %>%
  group_by(Botanical_garden, Plant) %>%
  summarise(
    Total_sampling_time = sum(Total_sampling_time)) %>% 
  ungroup()

#Calculate total interactions per plant species
interactions_by_sp = interaction_data %>%
  group_by(Botanical_garden, Plant, Pollinator) %>%
  summarise(
    Total_pair_interactions = sum(Interactions)) %>% 
  ungroup()

#Calculate total links per plant species
degree_by_sp = interaction_data %>%
  select(Botanical_garden,Plant, Pollinator) %>% 
  distinct() %>% 
  group_by(Botanical_garden, Plant) %>%
  summarise(
    Degree = length(Pollinator),
    Degree = length(Pollinator)) %>% 
  ungroup()



