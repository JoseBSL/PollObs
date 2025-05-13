# ======================================================
# Script: Prepare phenology matrix for plant-pollinator interactions
# ======================================================

#### ---- Load libraries ----
library(dplyr)
library(purrr)
library(tidyr)
library(ggplot2)
library(lubridate)
# ======================================================
#### ---- Load data ----

# Plant phenology data
jena_phen    = readRDS("Data/Phenology_data/clean_plant_phenobs_jena.rds")
halle_phen   = readRDS("Data/Phenology_data/clean_plant_phenobs_halle.rds")
leipzig_phen = readRDS("Data/Phenology_data/clean_plant_phenobs_leipzig.rds")

# Pollinator phenology (same for all gardens)
poll_phen = readRDS("Data/Working_files/pollinator_phenology.rds")

# Interaction data for extracting sampling weeks
raw_data = readRDS("Data/Working_files/interaction_data.rds")
# ======================================================
#### ---- Prepare plant phenology ----

# Add missing 'Garden' labels
jena_phen  = jena_phen %>% mutate(Garden = "Jena")
halle_phen = halle_phen %>% mutate(Garden = "Halle")
leipzig_phen = leipzig_phen %>% mutate(Garden = "Leipzig")

# Combine all gardens into one dataset
plant_phen = bind_rows(jena_phen, halle_phen, leipzig_phen)

# Identify missing Dates (converted later)
plant_phen_na = plant_phen %>%
  filter(is.na(Date)) %>%
  mutate(Date = as.Date(Doy - 1, origin = "2023-01-01"))

plant_phen_non_na = plant_phen %>%
  filter(!is.na(Date))

# Merge back, clean values, exclude 2022 data
plant_phen_fixed = bind_rows(plant_phen_na, plant_phen_non_na) %>%
  mutate(
    Flowers_opening = if_else(Flowers_opening == "y", "Yes", "No"),
    Sampling_week   = week(Date),
    Flowering_intensity = if_else(Flowers_opening == "No", 0, Flowering_intensity),
    Year = year(Date)) %>%
  filter(Year != 2022)

#Summarise per week
plant_phen_fixed_week = plant_phen_fixed %>% 
  select(Species, Sampling_week, Garden,  Flowering_intensity) %>% 
  group_by(Species, Garden, Sampling_week) %>% 
  summarise(Flowering_intensity_week = mean(Flowering_intensity) / 100)

#Not all weeks are present for all species
#We can assume that those are 0's
weeks_2023 = tibble(Doy = seq(1:365))
v_weeks_2023 = weeks_2023 %>% 
  mutate(Date = as.Date(Doy - 1, origin = "2023-01-01")) %>% 
  mutate(Sampling_week = week(Date)) %>% 
  distinct(Sampling_week) %>% 
  pull()

# Get unique Species and Gardens
species_garden <- plant_phen_fixed_week %>% 
  distinct(Species, Garden)

# Create all possible combinations
full_combinations <- crossing(
  species_garden,
  Sampling_week = v_weeks_2023
)

# Join your data into it and fill NAs with 0
plant_phen_week_complete = full_combinations %>%
  left_join(plant_phen_fixed_week, 
            by = c("Species", "Garden", "Sampling_week")) %>%
  mutate(Flowering_intensity_week = replace_na(Flowering_intensity_week, 0)) %>% 
  rename(Scaled_abundance = Flowering_intensity_week)
#Looks good to me!

# ======================================================
#### ---- Prepare pollinator phenology ----
poll_phen_week_complete = poll_phen %>%
  mutate(
    Date          = as.Date(Doy - 1, origin = "2023-01-01"),
    Sampling_week = week(Date)) %>% 
  select(Species, Sampling_week, Probability) %>% 
  rename(Scaled_abundance = Probability)  %>% 
  group_by(Species, Sampling_week) %>% 
  summarise(Scaled_abundance = mean(Scaled_abundance))
#I think all weeks are already for each poll
#Check it just in case
  poll_phen_fixed %>% 
  group_by(Species) %>% 
  summarise(n_weeks = n_distinct(Sampling_week)) %>% 
  distinct(n_weeks) %>% 
  pull()
#Yep, all good!

# ======================================================
#### ---- (Prepare sampling weeks) ----
sampling_dates = raw_data %>% 
  select(Botanical_garden, Date) %>% 
  distinct() %>% 
  mutate(Sampling_week = week(Date)) %>% 
  select(Botanical_garden, Sampling_week)

#Extract sampling weeks for 1 garden
#Build example
#Extract sampling dates from 1 garden
sampling_dates = sampling_dates %>% 
  filter(Botanical_garden == "Leipzig") %>% 
  select(Sampling_week) %>% 
  pull()
    
#Create 1st an example with 1 week and bot garden
plant_phen = plant_phen_week_complete %>% 
  filter(Garden == "Leipzig") %>% 
  filter(Sampling_week %in% sampling_dates) %>% 
  rename(Plants = Species) %>% 
  select(!Garden)
poll_phen = poll_phen_week_complete %>% 
  filter(Sampling_week %in% sampling_dates) %>% 
  rename(Pollinators = Species)
#Extract unique species
plants = plant_phen %>% distinct(Plants) %>% pull()
polls = poll_phen %>% distinct(Pollinators) %>% pull()

#Create full grid
full_grid = expand_grid(Plants = plants, Pollinators= polls, Sampling_week = sampling_dates)
#Now join plants
full_grid1 = left_join(full_grid, plant_phen, by = c("Plants", "Sampling_week")) 
#Rename to avoid duplicate cols in the next step
full_grid2 = full_grid1 %>% 
  rename(Scaled_abundance_plants = Scaled_abundance)
#Now join polls
full_grid3 = left_join(full_grid2, poll_phen, by = c("Pollinators", "Sampling_week")) 
#Rename accordingly
full_grid_final = full_grid3 %>% 
  rename(Scaled_abundance_polls = Scaled_abundance)
#Multiply cols for now
full_grid_final = full_grid_final %>% 
  mutate(Scaled_product = Scaled_abundance_plants * Scaled_abundance_polls)
