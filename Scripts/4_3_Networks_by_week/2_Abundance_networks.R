# ======================================================
#Probability probability abundance networks for each garden
#And compute correlation with int and int frequency networks
# ======================================================

#Load libraries
library(dplyr)
library(purrr)
library(tidyr)
library(ggplot2)
library(vegan) #for mantel and procrustes
library(readr)
library(stringr)

# ======================================================
# Load data
# Interaction data
raw_data = readRDS("Data/Working_files/interaction_data.rds")

# Morphometrics to get phenobs species vector
morphometrics = read_csv("Data/Trait_data/Raw/ReproductiveTraits_Morphometrics.csv")
colnames(morphometrics)
phenobs_spp = morphometrics %>% 
  select(Species) %>% 
  mutate(Species = str_replace(Species, "Persicaria bistorta", "Polygonum bistorta")) %>% 
  mutate(Species = str_replace(Species, "Aquilegia chrysantha", "Aquilegia vulgaris")) %>% 
  distinct() %>% 
  pull(Species) 

# Load plant-poll networks by garden
net_by_garden = readRDS("Data/Working_files/networks_by_garden_week_only_phenobs.rds")


# ======================================================
# Create vector with main polls
poll_order = c("Hymenoptera", "Diptera", "Coleoptera", "Lepidoptera")

# Create tibble with unique dates 
dates = raw_data %>% 
  select(Botanical_garden, Date) %>% 
  group_by(Botanical_garden) %>% 
  distinct()

weeks_2023 = tibble(Doy = 1:365) %>%
  mutate(Date = as.Date(Doy - 0, origin = "2023-01-01"),
         Sampling_week = isoweek(Date))

dates = left_join(dates, weeks_2023)

# Prepare interaction data
interaction_data = raw_data %>%
  filter(!is.na(Interactions),
         !is.na(Floral_abundance),
         Pollinator != "None") %>%
  rename(Plants = Plant_accepted_name,
         Pollinators = Pollinator_accepted_name) %>%
  filter(!is.na(Pollinators)) %>% 
  filter(Pollinator_rank == "SPECIES") %>% 
  filter(Sampling == "Focal") %>% 
  # filter(!Pollinators == "Apis mellifera") %>% 
  filter(Pollinator_order %in% poll_order)%>% 
  filter(!Plants == "Iberis sempervirens") 

interaction_data = interaction_data %>% 
  filter(Plant %in% phenobs_spp) 
# Add week number
interaction_data = left_join(interaction_data, dates)

# Pollinator abundance
pollinator_abundance = interaction_data %>% 
  group_by(Botanical_garden, Sampling_week, Pollinators) %>% 
  summarise(Individuals = n()) %>% 
  mutate(Relative_abundance = Individuals/ sum(Individuals))

# Check distribution
pollinator_abundance %>% 
  ggplot(aes(Relative_abundance)) +
  facet_wrap(~ Botanical_garden) +
  geom_histogram()


# Floral abundance
# Two setp process: select distinct values with Date_time
# and sum abundances
floral_abundance = interaction_data %>% 
  select(Botanical_garden, Sampling_week, Plants, Floral_abundance, Date_time) %>% 
  distinct() %>% 
  group_by(Botanical_garden, Sampling_week, Plants) %>% 
  summarise(Total_floral_abundance = sum(Floral_abundance)) %>% 
  mutate(Relative_abundance = Total_floral_abundance/ sum(Total_floral_abundance))
# Check distribution
floral_abundance %>% 
  ggplot(aes(Relative_abundance)) +
  facet_wrap(~ Botanical_garden) +
  geom_histogram()

# ======================================================
# Calculate a probability matrix based on relative abundances
build_prob_matrix = function(garden_name, week) {
  
  network = net_by_garden %>% 
    filter(Botanical_garden == garden_name, Sampling_week == week) %>% 
    select(Interaction_network) %>% 
    pull(Interaction_network) %>% 
    .[[1]]
  
  poll_order = tibble(Pollinators = colnames(network))
  plant_order = tibble(Plants = rownames(network))
  
  pollinator_abundance1 = pollinator_abundance %>% 
    filter(Botanical_garden == garden_name, Sampling_week == week) 
  
  floral_abundance1 = floral_abundance %>% 
    filter(Botanical_garden == garden_name, Sampling_week == week)
  
  poll_abund_ordered = left_join(poll_order, pollinator_abundance1)
  plant_abund_ordered = left_join(plant_order, floral_abundance1)
  #get the ordered vectors
  poll_vector = poll_abund_ordered$Relative_abundance
  plant_vector = plant_abund_ordered$Relative_abundance
  
  prob_matrix = outer(plant_vector, poll_vector, FUN = "*")
  
  # Assign dim names to match network structure
  rownames(prob_matrix) = plant_abund_ordered$Plants
  colnames(prob_matrix) = poll_abund_ordered$Pollinators
  
  return(prob_matrix)
}


# Now run it for each garden-week combination
adund_prob_matrices_by_garden_week = net_by_garden %>%
  mutate(Prob_matrix = pmap(
    list(Botanical_garden, Sampling_week),
    build_prob_matrix
  ))

# ======================================================
#Save network matrices
saveRDS(adund_prob_matrices_by_garden_week, 
        "Data/Working_files/abundance_networks_only_phenobs_by_week.rds")

