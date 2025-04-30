#Probability probability abundance networks for each garden
#And compute correlation with int and int frequency networks

#Load libraries
library(dplyr)
library(purrr)
library(tidyr)
library(ggplot2)
library(vegan) #for mantes and procrustes

#Load data
raw_data = readRDS("Data/Working_files/interaction_data.rds")
#Prepare interaction data
interaction_data = raw_data %>%
  filter(!is.na(Interactions),
         !is.na(Floral_abundance),
         Pollinator != "None") %>%
  rename(Plants = Plant_accepted_name,
         Pollinators = Pollinator_accepted_name) %>%
  filter(!is.na(Pollinators))

#Load morphometrics to get phenobs species vector
morphometrics = read_csv("Data/Trait_data/Raw/ReproductiveTraits_Morphometrics.csv")
colnames(morphometrics)
morphometrics = morphometrics %>% 
  mutate(Species = str_replace(Species, "Persicaria bistorta", "Polygonum bistorta")) %>% 
  mutate(Species = str_replace(Species, "Aquilegia chrysantha", "Aquilegia vulgaris")) 

phenobs_spp = morphometrics %>% 
  distinct() %>% 
  pull(Species) 
#Generate dataset to correct flower unit by flower size
flower_size = morphometrics %>% 
  select(Species, Flower_width) %>% 
  group_by(Species) %>% 
  summarise(Flower_size = mean(Flower_width, na.rm = T)) %>% 
  rename(Plant = Species)

#Filter by phenObs species
interaction_data = interaction_data %>% 
  filter(Plant %in% phenobs_spp) 

#Pollinator abundance
pollinator_abundance = interaction_data %>% 
  group_by(Botanical_garden, Pollinators) %>% 
  summarise(Individuals = length(Pollinators)) %>% 
  mutate(Relative_abundance = Individuals/ max(Individuals))
#Check distribution
pollinator_abundance %>% 
  ggplot(aes(Relative_abundance)) +
  geom_histogram()


#Floral abundance
#Two setp process: select distinct values with Date_time
#and sum abundances
#Create floral display dataset to correct by size
floral_display = left_join(interaction_data, flower_size)

s = floral_display %>% 
  select(Plant, Flower_size) %>% 
  distinct(Plant, Flower_size)

floral_display = floral_display %>% 
  select(Botanical_garden, Plant, Floral_abundance, Flower_size) %>% 
  distinct() %>% 
  mutate(Floral_display = Floral_abundance / Flower_size) %>%   
  group_by(Botanical_garden, Plant) %>% 
  summarise(Total_floral_display = sum(Floral_display, na.rm = TRUE), .groups = "drop") %>% 
  group_by(Botanical_garden) %>% 
  mutate(Relative_floral_display = Total_floral_display / max(Total_floral_display, na.rm = TRUE)) %>% 
  ungroup() %>% 
  rename(Plants = Plant)

#Check distribution
floral_display %>% 
  ggplot(aes(Relative_floral_display)) +
  geom_histogram()

#Load plant-poll networks by garden
net_by_garden = readRDS("Data/Working_files/networks_by_garden_only_phenobs_corrected.rds")

#Calculate a probability matrix based on relative abundances
build_prob_matrix = function(garden_name) {
  
  network = net_by_garden %>% 
    filter(Botanical_garden == garden_name) %>% 
    select(Interaction_network) %>% 
    pull(Interaction_network) %>% 
    .[[1]]
  
  poll_order = tibble(Pollinators = colnames(network))
  plant_order = tibble(Plants = rownames(network))
  
  pollinator_abundance1 = pollinator_abundance %>% 
    filter(Botanical_garden == garden_name) 
  
  floral_display1 = floral_display %>% 
    filter(Botanical_garden == garden_name)
  
  poll_abund_ordered = left_join(poll_order, pollinator_abundance1)
  plant_abund_ordered = left_join(plant_order, floral_display1)
  #get the ordered vectors
  poll_vector = poll_abund_ordered$Relative_abundance
  plant_vector = plant_abund_ordered$Relative_floral_display
  
  prob_matrix = outer(plant_vector, poll_vector, FUN = "*")
  
  # Assign dim names to match network structure
  rownames(prob_matrix) = plant_abund_ordered$Plants
  colnames(prob_matrix) = poll_abund_ordered$Pollinators
  
  return(prob_matrix)
}

#Run it for each garden
prob_matrices_by_garden = net_by_garden %>%
  mutate(Prob_matrix = map(Botanical_garden, build_prob_matrix))

#Save network matrices
saveRDS(prob_matrices_by_garden, 
        "Data/Working_files/abundance_networks_only_phenobs_corrected_by_size.rds")

