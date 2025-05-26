# ======================================================
# Script: Prepare phenology matrix for plant-pollinator interactions
# ======================================================

#It uses the phenological records of PLANTS (from EACH garden)
#and the occurrence records of gardens and GBIF for POLLINATORS (common across all)
#The records are converted to probabilities
#by dividing each observation by the total observations
#and then pooled together at the week level (avereage probability per week)
#we assume independence and the multiply plant prob * poll prob

#### ---- Load libraries ----
library(dplyr)
library(purrr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(tibble)
# ======================================================
# Load data
# Plant phenology data
jena_phen = readRDS("Data/Phenology_data/clean_plant_phenobs_jena.rds")
halle_phen = readRDS("Data/Phenology_data/clean_plant_phenobs_halle.rds")
leipzig_phen = readRDS("Data/Phenology_data/clean_plant_phenobs_leipzig.rds")

# Pollinator phenology (same for all gardens)
poll_phen = readRDS("Data/Working_files/pollinator_phenology.rds")

# Interaction data for extracting sampling weeks
raw_data = readRDS("Data/Working_files/interaction_data.rds")

#Load plant-poll networks by garden
net_by_garden = readRDS("Data/Working_files/networks_by_garden_and_week_only_phenobs_pheno.rds")
# Vector to exclude polls without phenology
spp_to_exclude = readRDS("Data/Working_files/spp_to_exclude_pheno.rds")


# ======================================================
# Prepare plant phenology

# Add missing 'Garden' labels
jena_phen  = jena_phen %>% mutate(Garden = "Jena")
halle_phen = halle_phen %>% mutate(Garden = "Halle")
leipzig_phen = leipzig_phen %>% mutate(Garden = "Leipzig")

# Combine all gardens into one dataset
plant_phen = bind_rows(jena_phen, halle_phen, leipzig_phen)

#Fix plants names
plant_phen = plant_phen %>% 
  mutate(Species = if_else(Species == "Anemonoides nemorosa", "Anemone nemorosa", Species)) %>% 
  mutate(Species = if_else(Species == "Anemonoides sylvestris", "Anemone sylvestris", Species)) %>% 
  mutate(Species = if_else(Species == "Anemone hupehensis", "Eriocapitella hupehensis", Species)) %>% 
  mutate(Species = if_else(Species == "Penstemon grandiflorus", "Penstemon bradburyi", Species)) %>% 
  mutate(Species = if_else(Species == "Silene viscaria", "Viscaria vulgaris", Species))

# Identify missing Dates (converted later)
#Records of 2022 that contain flowers are only for few species
#It's ok to have them, for instance, viola is an early flowering spp 
#and started before new years
plant_phen_na = plant_phen %>%
  filter(is.na(Date)) %>%
  mutate(Date = as.Date(Doy - 1, origin = "2023-01-01"))

plant_phen_non_na = plant_phen %>%
  filter(!is.na(Date))

# Merge back, clean values, exclude 2022 data
plant_phen_fixed = bind_rows(plant_phen_na, plant_phen_non_na) %>%
  mutate(
    Flowers_opening = if_else(Flowers_opening == "y", "Yes", "No"),
    Sampling_week = week(Date),
    Flowering_intensity = if_else(Flowers_opening == "No", 0, Flowering_intensity),
    Year = year(Date)) %>%
  filter(Year != 2022)

#Important step!!!
#At the moment the column of flowering intensity is a proportion (value/max value)
#Convert to probability
#Multiply back by max value and then divide by total
plant_phen_fixed = plant_phen_fixed %>% 
  group_by(Species, Garden) %>% 
  mutate(Flowering_intensity1 = (Flowering_intensity * max(Flowering_intensity))/sum(Flowering_intensity)) %>% 
  mutate(Probability = Flowering_intensity1/sum(Flowering_intensity1))
#Safety check
plant_phen_fixed %>% 
  filter(Species == "Silene viscaria" & Garden == "Jena") %>% 
  summarise(Sum_Probability = sum(Probability))
#Sums 1, seems right!

#Summarise per week
plant_phen_fixed_week = plant_phen_fixed %>% 
  select(Species, Sampling_week, Garden, Probability) %>% 
  group_by(Species, Garden, Sampling_week) %>% 
  summarise(Mean_probability = mean(Probability))

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
  mutate(Mean_probability = replace_na(Mean_probability, 0))
#Looks good to me!

# ======================================================
# Prepare pollinator phenology
poll_phen_week_complete = poll_phen %>%
  mutate(
    Date = as.Date(Doy - 1, origin = "2023-01-01"),
    Sampling_week = week(Date)) %>% 
  select(Species, Sampling_week, Probability, Abundances) %>% 
  rename(Proportion = Probability)  %>%
  group_by(Species) %>% 
  mutate(Probability = Abundances/sum(Abundances)) %>% 
  group_by(Species, Sampling_week) %>% 
  summarise(Mean_probability = mean(Probability))

#Exclude polls with insuficent information
poll_phen_week_complete = poll_phen_week_complete %>% 
  filter(!Species %in% spp_to_exclude)

#I think all weeks are already for each poll
#Check it just in case
poll_phen_week_complete %>% 
  group_by(Species) %>% 
  summarise(n_weeks = n_distinct(Sampling_week)) %>% 
  distinct(n_weeks) %>% 
  pull()
#Yep, all good!
#Now check if probabilities worked well
#Do an example for a single spp
poll_phen %>% 
  filter(Species == "Aglais io") %>% 
  mutate(Abundances1 = Abundances/sum(Abundances)) %>% 
  summarise(Mean_probability = sum(Abundances1)) #check if it sums 1


# ======================================================
# Prepare sampling weeks
sampling_dates = raw_data %>% 
  select(Botanical_garden, Date) %>% 
  distinct() %>% 
  mutate(Sampling_week = week(Date)) %>% 
  select(Botanical_garden, Sampling_week)

# ======================================================
# Now prepare phenological matrices for each garden!
# This involves extracting the sampling dates of each garden
# Create a grid of all spp combinations per week
# And add the data with 2-step left join for plants and polls
# Then in a separate function we will convert the long format to a matrix

# Get list of unique gardens
gardens = unique(plant_phen_week_complete$Garden)

# Function to run the process for one garden
compute_probability_matrix = function(garden_name) {
  
  sampling_dates = sampling_dates %>% 
    filter(Botanical_garden == garden_name) %>% 
    select(Sampling_week) %>% 
    pull()
  
  #Create 1st an example with 1 week and bot garden
  plant_phen = plant_phen_week_complete %>% 
    filter(Garden == garden_name) %>% 
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
    rename(Mean_probability_plants = Mean_probability)
  #Now join polls
  full_grid3 = left_join(full_grid2, poll_phen, by = c("Pollinators", "Sampling_week")) 
  #Rename accordingly
  full_grid_final = full_grid3 %>% 
    rename(Mean_probability_pollinators = Mean_probability)
  #Compute probability
  full_grid_final = full_grid_final %>% 
    mutate(Total_probability = Mean_probability_plants * Mean_probability_pollinators)
  #In order to condense in a final matrix
  #we need to group by Plants and pollinators
  full_grid_condensed = full_grid_final %>% 
    group_by(Plants, Pollinators) %>% 
    summarise(Mean_probability = mean(Total_probability))
  
  
  #Those are all combinations
  #For tomorrow convert to a matrix 
  #And do it for each garden
  return(full_grid_condensed)
}


# Create nested tibble (LONG FORMAT) with one row per garden
all_gardens_probabilities_nested = tibble(
  Botanical_garden = gardens,
  Probabilities = map(gardens, compute_probability_matrix))

# View it
all_gardens_probabilities_nested
# Safety check
all_gardens_probabilities_nested %>% 
  filter(Botanical_garden == "Halle") %>% 
  select(Probabilities) %>% 
  pull()


# ======================================================
# CONVERT LONG FORMAT TO MATRIX
net_by_garden 

garden_season_combos = net_by_garden %>% 
  select(Botanical_garden, Sampling_week) %>% 
  distinct()

# Build phenology matrix
build_prob_matrix = function(garden_name, week) {
  
  # Extract right poll and plant order
  network = net_by_garden %>% 
    filter(Botanical_garden == garden_name, Sampling_week == week) %>% 
    select(Interaction_network) %>% 
    pull(Interaction_network) %>% 
    .[[1]]
  # Create vectors with the desire order
  poll_order = colnames(network)
  plant_order = rownames(network)
  
  # Select long format from garden x
  garden_prob = all_gardens_probabilities_nested %>% 
    filter(Botanical_garden == garden_name) %>% 
    select(Probabilities) %>% 
    pull()  %>%
    .[[1]] 
  
  #Safety check conducted for each garden, it works
  #a = unique(garden_prob$Plants)
  #b = unique(plant_order)
  #setdiff(b,a)
  
  garden_prob = garden_prob %>% 
    filter(Plants %in% plant_order) %>% 
    filter(Pollinators %in% poll_order)
  
  # Create phenology matrix
  garden_prob_matrix = garden_prob %>%
    pivot_wider(
      names_from = Pollinators,
      values_from = Mean_probability
    ) %>%
    column_to_rownames("Plants") %>%
    as.matrix()
  
  
  garden_prob_matrix = garden_prob_matrix[plant_order, poll_order]
  
  return(garden_prob_matrix)
  
}

garden_season_combos = net_by_garden %>% 
  select(Botanical_garden, Sampling_week) %>% 
  distinct()

pheno_prob_matrices_by_garden = garden_season_combos %>% 
  mutate(Prob_matrix = map2(Botanical_garden, Sampling_week, build_prob_matrix))

# ======================================================
#Save network matrices
saveRDS(pheno_prob_matrices_by_garden, 
        "Data/Working_files/phenology_networks_only_phenobs_pheno_by_week.rds")


