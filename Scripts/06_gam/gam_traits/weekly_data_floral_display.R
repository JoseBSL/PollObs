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
# Morphometrics to get phenobs species vector
morphometrics <- read_csv("Data/Trait_data/Raw/ReproductiveTraits_Morphometrics.csv")
morphometrics <- morphometrics %>%
  mutate(Style_length = as.numeric(Style_length))
str(p_vulgaris)
#Add missing species
online_data = read_csv("Data/Trait_data/Raw/online_data.csv")
online_data <- online_data %>%
  mutate(Style_length = as.numeric(Style_length))
str(p_vulgaris)
#Morphometrics
morphometrics = bind_rows(morphometrics, online_data)

morphometrics = morphometrics %>% 
  select(Species, 
         Plant_height_mm, 
         Flower_number, 
         Flower_width, 
         Symmetry, 
         Flower_orientation,
         Flower_shape)

morpho_species <- morphometrics %>% 
  group_by(Species) %>% 
  summarise(
    Plant_height_mm = mean(Plant_height_mm, na.rm = TRUE),
    Flower_number   = mean(Flower_number, na.rm = TRUE),
    Flower_width    = mean(Flower_width, na.rm = TRUE),
    .groups = "drop"
  )

morpho_cat <- morphometrics %>% 
  group_by(Species) %>% 
  summarise(
    Symmetry = first(Symmetry),
    Flower_orientation = first(Flower_orientation),
    Flower_shape = first(Flower_shape),
    .groups = "drop"
  )

mode_fun <- function(x) {
  ux <- na.omit(unique(x))
  ux[which.max(tabulate(match(x, ux)))]
}

morpho_cat <- morphometrics %>% 
  group_by(Species) %>% 
  summarise(
    Symmetry = mode_fun(Symmetry),
    Flower_orientation = mode_fun(Flower_orientation),
    Flower_shape = mode_fun(Flower_shape),
    .groups = "drop"
  )

morpho_species <- morpho_species %>% 
  left_join(morpho_cat, by = "Species")


morpho_species <- morpho_species %>% 
  mutate(
    Plant_height_mm_z = as.numeric(scale(Plant_height_mm)),
    Flower_number_z   = as.numeric(scale(Flower_number)),
    Flower_width_z    = as.numeric(scale(Flower_width)),
    Symmetry = factor(Symmetry),
    Flower_orientation = factor(Flower_orientation),
    Flower_shape = factor(Flower_shape)
  ) %>% 
  rename(Plant = Species)

weekly_data = weekly_data %>% 
  left_join(morpho_species, by = "Plant")

################################################################################
# add trait data
trait_pairs = readRDS("Data/Working_files/trait_pairs.rds")
weekly_data = left_join(weekly_data,trait_pairs, by="Pair")
#Filter out pairs with missing trait data
checks = weekly_data %>% 
  filter(is.na(T_gauss))

weekly_data = weekly_data %>% 
  filter(!is.na(T_gauss))

##Scale data
weekly_data = weekly_data %>% 
  mutate(T_gauss_z   = as.numeric(scale(T_gauss)))
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
saveRDS(weekly_data, "Data/Working_files/weekly_data_floral_display.rds")
################################################################################

