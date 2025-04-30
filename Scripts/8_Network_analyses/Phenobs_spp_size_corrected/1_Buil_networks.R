#Create interaction matrices by garden
#1)Total interactions
#AND
#2)Interaction frequency

#Load libraries
library(readr)
library(tidyr)
library(dplyr)
library(purrr)
library(stringr)
library(tibble)

#Load data
raw_data = readRDS("Data/Working_files/interaction_data.rds")

#Load morphometrics to get phenobs species vector
morphometrics = read_csv("Data/Trait_data/Raw/ReproductiveTraits_Morphometrics.csv")
colnames(morphometrics)
phenobs_spp = morphometrics %>% 
  select(Species) %>% 
  mutate(Species = str_replace(Species, "Persicaria bistorta", "Polygonum bistorta")) %>% 
  mutate(Species = str_replace(Species, "Aquilegia chrysantha", "Aquilegia vulgaris")) %>% 
  distinct() %>% 
  pull(Species) 

#Prepare interaction data
interaction_data = raw_data %>%
  filter(!is.na(Interactions),
         !is.na(Floral_abundance),
         Pollinator != "None") %>%
  rename(Plants = Plant_accepted_name,
         Pollinators = Pollinator_accepted_name) %>%
  filter(!is.na(Pollinators))


interaction_data = interaction_data %>% 
  filter(Plant %in% phenobs_spp) 

#1)Total interactions

#Convert to network
to_network = function(data) {
  data %>%
    group_by(Plant, Pollinators) %>%
    summarise(Total_interactions = sum(Interactions),
              .groups = "drop") %>%
    pivot_wider(names_from = Pollinators, 
                values_from = Total_interactions, values_fill = 0) %>%
    column_to_rownames("Plant") %>%
    as.matrix()
}

#Prepare networks per garden
networks_by_garden_interactions = interaction_data %>%
  group_by(Botanical_garden) %>%
  nest() %>%
  mutate(Interaction_network = map(data, to_network)) %>%
  select(!data) %>% 
  ungroup()

#2)Interaction frequency
#Overwrite interactions with interaction frequency 
#to minimise edits in code
interaction_frequency = interaction_data %>% 
  mutate(Interactions = Interactions/Total_time_species) 

#Prepare networks per garden
networks_by_garden_int_frequency = interaction_frequency %>%
  group_by(Botanical_garden) %>%
  nest() %>%
  mutate(Int_frequency_network = map(data, to_network)) %>%
  select(!data) %>% 
  ungroup()

#Now bind both and get a tibble with total int and int freq
net_by_garden = left_join(networks_by_garden_interactions,
                          networks_by_garden_int_frequency)

saveRDS(net_by_garden, "Data/Working_files/networks_by_garden_only_phenobs_corrected.rds")

