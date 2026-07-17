############################################################ #
#Prepare data for modelling (GAM and Random Forest)
############################################################ #
# Load libraries
library(dplyr)
library(readr)
############################################################ #
# Load data
raw_data = readRDS("Data/Working_files/interaction_data.rds") 
# Load data for traits and phenology
plant_traits = read_csv("Data/Trait_data/Raw/ReproductiveTraits_Morphometrics.csv")
missing_plant_traits = read_csv("Data/Trait_data/Raw/online_data.csv")
poll_traits = readRDS("Data/Working_files/trait_pairs.rds")
pheno_overlap = readRDS("Data/Working_files/phenoloical_overlap.rds")
############################################################ #
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
plant_week_effort = raw_week %>%
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
interaction_data = interaction_week %>%
  left_join(plant_week_effort, by = c("Botanical_garden", "Plant", "Week")) %>%
  mutate(
    VisitRate = if_else(Total_time_plant_week > 0,
                        Total_pair_interactions / Total_time_plant_week,
                        NA_real_),
    Pair = paste(Plant, Pollinator, sep = "_")
  )

# 4) Environmental variables per garden-week
environmental_variables = raw_week %>%
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
poll_abundance_week = raw_week %>%
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
floral_abundance_week = raw_week %>%
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

################################################################################
# Morphometrics to get phenobs species vector
plant_traits = plant_traits %>%
  mutate(Style_length = as.numeric(Style_length))
# Add missing species
online_data = read_csv("Data/Trait_data/Raw/online_data.csv")
online_data = online_data %>%
  mutate(Style_length = as.numeric(Style_length))
plant_traits = bind_rows(plant_traits, online_data)

plant_traits_numerical = plant_traits %>% 
  select(Species, 
         Plant_height_mm, 
         Flower_width, 
         Symmetry, 
         Flower_orientation,
         Flower_shape) %>% 
  group_by(Species) %>% 
  summarise(
    Plant_height_mm = mean(Plant_height_mm, na.rm = TRUE),
    Flower_width    = mean(Flower_width, na.rm = TRUE),
    .groups = "drop")

# Select main categorical traits
mode_fun = function(x) {
  ux <- na.omit(unique(x))
  ux[which.max(tabulate(match(x, ux)))]}

plant_traits_categorical = plant_traits %>% 
  group_by(Species) %>% 
  summarise(
    Symmetry = mode_fun(Symmetry),
    Flower_orientation = mode_fun(Flower_orientation),
    Flower_shape = mode_fun(Flower_shape),
    .groups = "drop")

# Bind back categorical traits
plant_traits_clean = plant_traits_numerical %>% 
  left_join(plant_traits_categorical, by = "Species") %>% 
  rename(Plant = Species)

# Add plant traits to weekly data 
weekly_data = weekly_data %>% 
  left_join(plant_traits_clean, by = "Plant")

################################################################################
#Load nectar data 
nectar = read_csv("Data/Trait_data/Raw/Nectar_volume.csv")
#Select columns of interest
#Note the microcaps were 1ul and 32 mm length
nectar1 = nectar %>% 
  select(Species, Nectar_length_microcap) %>% 
  mutate(Nectar_volume = Nectar_length_microcap * 1 / 32) %>% 
  group_by(Species) %>% 
  summarise(Mean_nectar_volume = mean(Nectar_volume)) %>% 
  rename(Plant = Species)

weekly_data = left_join(weekly_data, nectar1, by="Plant")

################################################################################
# Add poll trait data
weekly_data = left_join(weekly_data, poll_traits, by="Pair")
# Filter out pairs with missing trait data
weekly_data %>% 
  filter(is.na(T_gauss))
weekly_data = weekly_data %>% 
  filter(!is.na(T_gauss))
################################################################################
# Add phenology
weekly_data = left_join(weekly_data, pheno_overlap)
# Checks
weekly_data %>% filter(is.na(Overlap_days))
weekly_data = weekly_data %>% 
  filter(!is.na(Overlap_days))

# Save data
saveRDS(weekly_data, "Data/Working_files/weekly_data_for_modelling.rds")
################################################################################
