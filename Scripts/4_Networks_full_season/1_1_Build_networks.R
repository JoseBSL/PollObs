# ======================================================
#Script: Prepare plant-poll networks (visits and visitation rate)
# ======================================================

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
# ======================================================
# Create vector of mail orders
poll_order = c("Hymenoptera", "Diptera", "Coleoptera", "Lepidoptera")

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
  filter(Pollinator_order %in% poll_order) %>% 
  filter(!Plants == "Iberis sempervirens") 
  
interaction_data = interaction_data %>% 
filter(Plant %in% phenobs_spp) 

# ======================================================
#1)Total interactions

#Convert to network
to_network = function(data) {
  data %>%
    group_by(Plants, Pollinators) %>%
    summarise(Total_interactions = sum(Interactions),
              .groups = "drop") %>%
    pivot_wider(names_from = Pollinators, 
                values_from = Total_interactions, values_fill = 0) %>%
    column_to_rownames("Plants") %>%
    as.matrix()
}

#Prepare networks per garden
networks_by_garden_interactions = interaction_data %>%
  group_by(Botanical_garden) %>%
  nest() %>%
  mutate(Interaction_network = map(data, to_network)) %>%
  select(!data) %>% 
  ungroup()

# ======================================================
#2)Interaction frequency
#Overwrite interactions with interaction frequency 
#to minimise edits in code
interaction_frequency = interaction_data %>% 
  group_by(Botanical_garden, Plants, Pollinators) %>%
  summarise(Interaction_total = sum(Interactions))

total_time_species = interaction_data %>%
  select(Botanical_garden, Plants, Date, Total_time_species) %>% 
  group_by(Botanical_garden, Plants) %>%
  distinct() %>% 
  summarise(Total_time_species = sum(Total_time_species, na.rm = TRUE), .groups = "drop")

# Join total time back into interaction data
interaction_data_freq = interaction_frequency %>%
  left_join(total_time_species, by = c("Botanical_garden", "Plants"), suffix = c("", "_summed")) %>%
  mutate(Freq = Interaction_total / Total_time_species)


# Now convert to interaction frequency network
to_freq_network = function(data) {
  data %>%
    group_by(Plants, Pollinators) %>%
    summarise(Total_frequency = sum(Freq), .groups = "drop") %>%
    pivot_wider(names_from = Pollinators, 
                values_from = Total_frequency, values_fill = 0) %>%
    column_to_rownames("Plants") %>%
    as.matrix()
}

# Prepare networks per garden (frequencies)
networks_by_garden_int_frequency = interaction_data_freq %>%
  group_by(Botanical_garden) %>%
  nest() %>%
  mutate(Int_frequency_network = map(data, to_freq_network)) %>%
  select(!data) %>%
  ungroup()


# Now bind both and get a tibble with total int and int freq
net_by_garden = left_join(networks_by_garden_interactions,
                          networks_by_garden_int_frequency)


# ======================================================
# Save data
saveRDS(net_by_garden, "Data/Working_files/networks_by_garden_only_phenobs.rds")


#Safety check
cor(c(dist(net_by_garden$Interaction_network[[1]])),
    c(dist(net_by_garden$Int_frequency_network[[1]])))

