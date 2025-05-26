#Create interaction matrices by garden
#Note that in this script we only consider phenobs species
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
spp_to_exclude = readRDS("Data/Working_files/spp_to_exclude_pheno.rds")

#Load morphometrics to get phenobs species vector
morphometrics = read_csv("Data/Trait_data/Raw/ReproductiveTraits_Morphometrics.csv")
colnames(morphometrics)
phenobs_spp = morphometrics %>% 
  select(Species) %>% 
  mutate(Species = str_replace(Species, "Persicaria bistorta", "Polygonum bistorta")) %>% 
  mutate(Species = str_replace(Species, "Aquilegia chrysantha", "Aquilegia vulgaris")) %>% 
  distinct() %>% 
  pull(Species) 

# Create tibble with unique dates 
dates = raw_data %>% 
  select(Botanical_garden, Date) %>% 
  group_by(Botanical_garden) %>% 
  distinct()

# Convert to tibble
groupped_dates = dates %>%
  group_by(Botanical_garden) %>% 
  arrange(Date, .by_group = TRUE) %>% 
  mutate(Season_group = ntile(Date, 3)) %>%
  mutate(Season = case_when(
    Season_group == 1 ~ "Early",
    Season_group == 2 ~ "Mid",
    Season_group == 3 ~ "Late"
  )) %>%
  select(!Season_group) %>% 
  ungroup()

#Create vector with orders
poll_order = c("Hymenoptera", "Diptera", "Coleoptera", "Lepidoptera")

#Prepare interaction data
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

#Exclude polls with insufficent phenol. records
interaction_data = interaction_data %>% 
  filter(!Pollinators %in% spp_to_exclude)

interaction_data = interaction_data %>% 
  filter(Plant %in% phenobs_spp) 

#Add season category to the data (early-mid and late season)
interaction_data = left_join(interaction_data, groupped_dates)


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
  group_by(Botanical_garden, Season) %>%
  nest() %>%
  mutate(Interaction_network = map(data, to_network)) %>%
  select(!data) %>% 
  ungroup()

#2)Interaction frequency
#Overwrite interactions with interaction frequency 
#to minimise edits in code
interaction_frequency = interaction_data %>% 
  group_by(Botanical_garden, Season, Plants, Pollinators) %>%
  summarise(Interaction_total = sum(Interactions))

total_time_species = interaction_data %>%
  select(Botanical_garden, Season, Plants, Date, Total_time_species) %>% 
  group_by(Botanical_garden, Season, Plants) %>%
  distinct() %>% 
  summarise(Total_time_species = sum(Total_time_species, na.rm = TRUE), .groups = "drop")

# Join total time back into interaction data
interaction_data_freq = interaction_frequency %>%
  left_join(total_time_species, by = c("Botanical_garden", "Season", "Plants"), suffix = c("", "_summed")) %>%
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
  group_by(Botanical_garden, Season) %>%
  nest() %>%
  mutate(Int_frequency_network = map(data, to_freq_network)) %>%
  select(!data) %>%
  ungroup()


#Now bind both and get a tibble with total int and int freq
net_by_garden = left_join(networks_by_garden_interactions,
                          networks_by_garden_int_frequency)

saveRDS(net_by_garden, "Data/Working_files/networks_by_garden_and_season_only_phenobs_pheno.rds")



cor(c(dist(net_by_garden$Interaction_network[[1]])),
    c(dist(net_by_garden$Int_frequency_network[[1]])))

