#Script to compute some network descriptors at species level
#This intended to be relevant only for the PhenObs plants

#1)Interactions/Interaction frequency
#2)Degree/Normalised degree

#Load libraries
library(dplyr)
library(readr)
library(stringr)
#Load raw data
raw_data = readRDS("Data/Working_files/interaction_data.rds")
interaction_data = raw_data  %>% filter(!is.na(Interactions),
                                         !is.na(Floral_abundance),
                                         Pollinator != "None") %>% 
  select(Botanical_garden, Plant, 
         Pollinator_accepted_name, Date_time, 
         Interactions, Floral_abundance) %>% 
  rename(Plant = Plant, Pollinator = Pollinator_accepted_name) %>% filter(!is.na(Pollinator))

#Load sampling time data (to correct if neccesary)
data_sampling_time_by_sp = 
  read_csv("Data/Working_files/data_floral_ab_sampling_time_by_sp.csv") %>%
  group_by(Botanical_garden, Plant) %>%
  summarise(
    Total_sampling_time = sum(Total_sampling_time)) %>% 
  ungroup() 

#Load some random trait data to extract list of PhenObs plants
phenobs_spp = read_csv("Data/Trait_data/Raw/ReproductiveTraits_Morphometrics.csv") %>% 
  select(Species) %>% 
  distinct() %>% 
  pull()
#Rename these 2 species to match interaction data
#Polygonum bistorta/Aquilegia vulgaris
phenobs_spp = str_replace(phenobs_spp, "Persicaria bistorta", "Polygonum bistorta")
phenobs_spp = str_replace(phenobs_spp, "Aquilegia chrysantha", "Aquilegia vulgaris")

#1)Interactions/Interaction frequency
#Calculate total interactions per plant species
interactions_by_sp = interaction_data %>%
  group_by(Botanical_garden, Plant, Pollinator) %>%
  summarise(
    Total_pair_interactions = sum(Interactions)) %>% 
  ungroup()

#Add sampling time to correct interactions by time
interactions_by_sp_1 = left_join(interactions_by_sp, data_sampling_time_by_sp,
          by = join_by(Botanical_garden, Plant))

#Interactions
interactions = interactions_by_sp_1 %>% 
  rename(Interactions_total = Total_pair_interactions) %>% 
  mutate(Interaction_frequency = Interactions_total/Total_sampling_time)
#Now we have total interactions
#and interaction frequency

#2)Degree/Normalised degree
#Now compute degree and normalised degree
#Calculate total pollinators per garden to normalised the degree
total_pollinators_per_garden = interaction_data %>%
  select(Botanical_garden, Pollinator) %>%
  distinct() %>%
  group_by(Botanical_garden) %>%
  summarise(Total_pollinators = n_distinct(Pollinator)) %>%
  ungroup()

#Calculate degree and normalised degree per plant species
degree = interaction_data %>%
  select(Botanical_garden,Plant, Pollinator) %>% 
  distinct() %>% 
  group_by(Botanical_garden, Plant) %>%
  summarise(
    Degree = length(Pollinator)) %>% 
 left_join(total_pollinators_per_garden, by = "Botanical_garden") %>%  # Join total pollinators per garden
  mutate(Normalised_degree = Degree / Total_pollinators) %>%  # Normalize
  ungroup()

#Bind both datasets
metrics = left_join(interactions, degree, 
          by = join_by(Botanical_garden, Plant))

#Select cols of interest
metrics = metrics %>% 
  select(Plant,
         Interactions_total,
         Interaction_frequency,
         Degree,
         Normalised_degree) %>% 
  group_by(Plant) %>% 
  summarise(Mean_interactions_total = mean(Interactions_total, na.rm = TRUE),
            Mean_interaction_frequency = mean(Interaction_frequency, na.rm = TRUE),
            Mean_degree = mean(Degree, na.rm = TRUE),
            Mean_normalised_degree = mean(Normalised_degree, na.rm = TRUE)) %>% 
  filter(Plant %in% phenobs_spp)

#There are 12 missing species
#Check why!!
spp = metrics %>% 
select(Plant) %>% 
distinct() %>% 
pull()

setdiff(spp,phenobs_spp)
#After selecting the unmodified names
#there are only 4 species and none of them contain interactions
#Cyclamen coum
#Trillium sessile
#Dryas octopetala
#Asarum caudatum
zero_species = tibble(
  Plant = c("Cyclamen coum", "Trillium sessile", "Dryas octopetala", "Asarum caudatum"),
  Mean_interactions_total = 0,
  Mean_interaction_frequency = 0,
  Mean_degree = 0,
  Mean_normalised_degree = 0)

#Add to data
phenobs_metrics = bind_rows(metrics, zero_species)

#Save data
saveRDS(phenobs_metrics, "Data/Working_files/phenobs_network_metrics.rds")
