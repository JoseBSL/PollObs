############################################################ #
#Prepare data for modelling FULL SEASON:
# Response variable - int_season
# Key predictors: - env_season
#                 - floral_season
#                 - poll_season
# data:           - season_data
############################################################ #
# Load libraries
library(dplyr)
library(lubridate)
library(ggplot2)
library(glmmTMB)
library(DHARMa)
library(ggeffects)
library(performance)
############################################################ #
# Load data
raw_data = readRDS("Data/Working_files/interaction_data.rds")
############################################################ #
#Prepare pair interactions per season (numerator)
int_season <- raw_data %>%
  filter(!is.na(Interactions),
         !is.na(Floral_abundance),
         Pollinator != "None") %>%
  transmute(
    Botanical_garden,
    Plant = Plant_accepted_name,
    Pollinator = Pollinator_accepted_name,
    Interactions
  ) %>%
  group_by(Botanical_garden, Plant, Pollinator) %>%
  summarise(
    Total_pair_interactions_season = sum(Interactions, na.rm = TRUE),
    .groups = "drop"
  )

#Prepare plant effort per season (denominator)  **IMPORTANT: no Pollinator here**
effort_season <- raw_data %>%
  filter(!is.na(Interactions),
         !is.na(Floral_abundance),
         Pollinator != "None") %>%
  transmute(
    Botanical_garden,
    Plant = Plant_accepted_name,
    Total_time_species
  ) %>%
  distinct() %>%   # avoid repeating within same sampling record
  group_by(Botanical_garden, Plant) %>%
  summarise(
    Total_time_plant_season = sum(Total_time_species, na.rm = TRUE),
    .groups = "drop"
  )

#Floral abundance season
floral_season <- raw_data %>%
  filter(!is.na(Floral_abundance)) %>%
  transmute(
    Botanical_garden,
    Plant = Plant_accepted_name,
    Floral_abundance
  ) %>%
  distinct() %>%
  group_by(Botanical_garden, Plant) %>%
  summarise(
    Floral_abundance_season = sum(Floral_abundance, na.rm = TRUE),
    .groups = "drop"
  )

#Pollinator abundance season
poll_season <- raw_data %>%
  filter(!is.na(Interactions),
         Pollinator != "None") %>%
  transmute(
    Botanical_garden,
    Pollinator = Pollinator_accepted_name
  ) %>%
  group_by(Botanical_garden, Pollinator) %>%
  summarise(
    Pollinator_abundance_season = n(),
    .groups = "drop"
  )

#Environment season
env_season <- raw_data %>%
  transmute(
    Botanical_garden,
    Temperature,
    Humidity,
    Rainfall
  ) %>%
  group_by(Botanical_garden) %>%
  summarise(
    Mean_Temperature_season = mean(Temperature, na.rm = TRUE),
    Mean_Humidity_season    = mean(Humidity, na.rm = TRUE),
    Mean_Rainfall_season    = mean(Rainfall, na.rm = TRUE),
    .groups = "drop"
  )

#FINAL season dataset
season_data = int_season %>%
  left_join(effort_season, by = c("Botanical_garden", "Plant")) %>%
  left_join(floral_season, by = c("Botanical_garden", "Plant")) %>%
  left_join(poll_season,   by = c("Botanical_garden", "Pollinator")) %>%
  left_join(env_season,    by = "Botanical_garden") %>%
  mutate(
    VisitRate_season = Total_pair_interactions_season / Total_time_plant_season,
    Pair = paste(Plant, Pollinator, sep = "_"),
    log_flower = log1p(Floral_abundance_season),
    log_poll   = log1p(Pollinator_abundance_season),
    log_poll_z = as.numeric(scale(log_poll))
  )
