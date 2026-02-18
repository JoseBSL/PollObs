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
library(tibble)
############################################################ #
# Load data
raw_data = readRDS("Data/Working_files/interaction_data.rds") 
# Get focal species
raw_focal = raw_data  %>% 
  filter(Sampling == "Focal") %>% 
  filter(Plant_rank == "SPECIES") %>% 
  filter(Pollinator_rank == "SPECIES")
# Get focal species to recover those from random sampling
focal_spp = raw_data %>% 
  filter(Sampling == "Focal") %>% 
  distinct(Plant_accepted_name) %>% 
  pull(Plant_accepted_name)
# get random species that match plant focal species
raw_random = raw_data %>% 
  filter(Sampling == "Random_census") %>% 
  filter(Plant_accepted_name %in% focal_spp) %>% 
  filter(Plant_rank == "SPECIES") %>% 
  filter(Pollinator_rank == "SPECIES")
# Bind back to focal
raw_focal = bind_rows(raw_focal, raw_random)
############################################################
# WEEKLY DATA (one row per garden × plant × pollinator × week)
############################################################
# Add Date + Week once
raw_week = raw_focal %>%
  mutate(
    Date = as.Date(Date_time),
    Week = lubridate::week(Date)
  )

# 1) Pair-week totals (counts)
interaction_week = raw_week %>%
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

# Checks
weekly_data %>% 
  group_by(Botanical_garden) %>% 
  summarise(n_distinct(Week))

weekly_data %>% 
  group_by(Botanical_garden) %>% 
  summarise(n_distinct(Pair))
################################################################################
# add trait data
phyl_pca_forest = readRDS("Data/Working_files/local_pca_phenobs_species_output.rds")

pc1 = phyl_pca_forest$S %>% 
  as.data.frame() %>% 
  rownames_to_column("Plant") %>% 
  select(Plant, PC1,PC2) %>% 
  mutate(Plant = gsub("_", " ", Plant))

weekly_data = left_join(weekly_data,pc1, by="Plant")

# Now add poll traits
poll_traits = readRDS("Data/Trait_data/Processed/PollTraits.rds")

poll_traits = poll_traits %>% 
  group_by(Pollinator) %>%
  mutate(IT_mm_mean = mean(IT_mm, na.rm = TRUE)) %>% 
  mutate(Length_mm_mean = mean(Length_mm, na.rm = TRUE)) %>% 
  mutate(Tongue_mm_mean = mean(Tongue_mm, na.rm = TRUE)) %>% 
  select(Pollinator, IT_mm_mean, Length_mm_mean,Tongue_mm_mean) %>% 
  distinct()


weekly_data = left_join(weekly_data,poll_traits, by="Pollinator")


#Some could be recovered, for now sample size, over 1000 rows
################################################################################
# Add phenology
pheno_overlap = readRDS("Data/Working_files/phenoloical_overlap.rds")

weekly_data = left_join(weekly_data, pheno_overlap)

checks = weekly_data %>% filter(is.na(Overlap_days))
colnames(checks)
weekly_data = weekly_data %>% 
  filter(!is.na(Overlap_days))
#Scale data
weekly_data = weekly_data %>% 
  mutate(Overlap_days_z   = as.numeric(scale(Overlap_days)))


#Fix cols:
weekly_data <- weekly_data %>%
  mutate(
    log_flower_z    = as.numeric(log_flower_z),
    log_poll_z      = as.numeric(log_poll_z),
    Overlap_days_z  = as.numeric(Overlap_days_z),
    Week            = as.numeric(Week),
    Botanical_garden = as.factor(Botanical_garden),
    Pair            = as.factor(Pair),
    Total_time_plant_week = as.numeric(Total_time_plant_week)
  )

#55 missing observations, those could be recovered
# Save data
saveRDS(weekly_data, "Data/Working_files/weekly_data_pca.rds")
################################################################################




