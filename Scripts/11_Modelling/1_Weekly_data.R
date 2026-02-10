############################################################ #
#Prepare data for modelling WEEKLY DATA:
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
raw_data = readRDS("Data/Working_files/interaction_data.rds")  %>% 
  filter(Sampling == "Focal") %>% 
  filter(Plant_rank == "SPECIES") %>% 
  filter(Pollinator_rank == "SPECIES")
############################################################ #

############################################################
# WEEKLY DATA (one row per garden × plant × pollinator × week)
############################################################

# Add Date + Week once
raw_week <- raw_data %>%
  mutate(
    Date = as.Date(Date_time),
    Week = lubridate::week(Date)
  )

# 1) Pair-week totals (counts)
interaction_week <- raw_week %>%
  filter(!is.na(Interactions),
         !is.na(Floral_abundance),
         Pollinator != "None",
         !is.na(Pollinator_accepted_name)) %>%
  transmute(
    Botanical_garden,
    Plant      = Plant_accepted_name,
    Pollinator = Pollinator_accepted_name,
    Week,
    Interactions
  ) %>%
  group_by(Botanical_garden, Plant, Pollinator, Week) %>%
  summarise(
    Total_pair_interactions = sum(Interactions, na.rm = TRUE),
    .groups = "drop"
  )

# 2) Plant-week effort (total observation time per plant per week)
plant_week_effort <- raw_week %>%
  filter(!is.na(Interactions),
         !is.na(Floral_abundance),
         Pollinator != "None") %>%
  transmute(
    Botanical_garden,
    Plant = Plant_accepted_name,
    Week,
    Total_time_species
  ) %>%
  distinct() %>%   # avoids repeating within the same sampling record
  group_by(Botanical_garden, Plant, Week) %>%
  summarise(
    Total_time_plant_week = sum(Total_time_species, na.rm = TRUE),
    .groups = "drop"
  )

# 3) Join + compute visitation rate
interaction_data <- interaction_week %>%
  left_join(plant_week_effort, by = c("Botanical_garden", "Plant", "Week")) %>%
  mutate(
    VisitRate = if_else(Total_time_plant_week > 0,
                        Total_pair_interactions / Total_time_plant_week,
                        NA_real_),
    Pair = paste(Plant, Pollinator, sep = "_")
  )

# 4) Environmental variables per garden-week
environmental_variables <- raw_week %>%
  filter(!is.na(Interactions),
         !is.na(Floral_abundance),
         Pollinator != "None") %>%
  transmute(
    Botanical_garden,
    Week,
    Temperature,
    Humidity,
    Rainfall
  ) %>%
  group_by(Botanical_garden, Week) %>%
  summarise(
    Mean_Temperature = mean(Temperature, na.rm = TRUE),
    Mean_Humidity    = mean(Humidity, na.rm = TRUE),
    Mean_Rainfall    = mean(Rainfall, na.rm = TRUE),
    .groups = "drop"
  )

# 5) Pollinator abundance per garden-pollinator-week
poll_abundance_week <- raw_week %>%
  filter(!is.na(Interactions),
         Pollinator != "None",
         !is.na(Pollinator_accepted_name)) %>%
  group_by(
    Botanical_garden,
    Pollinator = Pollinator_accepted_name,
    Week
  ) %>%
  summarise(
    Total_pollinator_abundance = n(),
    .groups = "drop"
  )

# 6) Floral abundance per garden-plant-week
floral_abundance_week <- raw_week %>%
  filter(!is.na(Floral_abundance)) %>%
  select(
    Botanical_garden,
    Plant = Plant_accepted_name,
    Week,
    Floral_abundance,
    Date_time
  ) %>%
  distinct() %>%   # avoid repeated counts per interaction
  group_by(Botanical_garden, Plant, Week) %>%
  summarise(
    Floral_abundance = sum(Floral_abundance, na.rm = TRUE),
    .groups = "drop"
  )

# 7) FINAL JOIN: one row per (garden × plant × pollinator × week)
weekly_data = interaction_data %>%
  left_join(environmental_variables, by = c("Botanical_garden", "Week")) %>%
  left_join(floral_abundance_week,   by = c("Botanical_garden", "Plant", "Week")) %>%
  left_join(poll_abundance_week,     by = c("Botanical_garden", "Pollinator", "Week"))


# 8) Transforms ready for modelling (WEEKLY)
weekly_data = weekly_data %>%
  mutate(
    log_flower   = log1p(Floral_abundance),
    log_poll     = log1p(Total_pollinator_abundance),
    log_flower_z = as.numeric(scale(log_flower)),
    log_poll_z   = as.numeric(scale(log_poll))
  )

saveRDS(weekly_data, "Data/Working_files/weekly_data.rds")
################################################################################

