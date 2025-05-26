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
net_by_garden = readRDS("Data/Working_files/networks_by_garden_season_only_phenobs.rds")


# ======================================================
# Create vector with main polls
poll_order = c("Hymenoptera", "Diptera", "Coleoptera", "Lepidoptera")

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
# Add season category to the data (early-mid and late season)
interaction_data = left_join(interaction_data, groupped_dates)

# Pollinator abundance
pollinator_abundance = interaction_data %>% 
  group_by(Botanical_garden, Season, Pollinators) %>% 
  summarise(Individuals = n()) %>% 
  mutate(Relative_abundance = Individuals/ max(Individuals))
# Check distribution
pollinator_abundance %>% 
  ggplot(aes(Relative_abundance)) +
  facet_wrap(~ Botanical_garden) +
  geom_histogram()


# Floral abundance
# Two setp process: select distinct values with Date_time
# and sum abundances
floral_abundance = interaction_data %>% 
  select(Botanical_garden, Season, Plants, Floral_abundance, Date_time) %>% 
  distinct() %>% 
  group_by(Botanical_garden, Season, Plants) %>% 
  summarise(Total_floral_abundance = sum(Floral_abundance)) %>% 
  mutate(Relative_abundance = Total_floral_abundance/ max(Total_floral_abundance))
# Check distribution
floral_abundance %>% 
  ggplot(aes(Relative_abundance)) +
  facet_wrap(~ Botanical_garden) +
  geom_histogram()

# ======================================================
# Calculate a probability matrix based on relative abundances
build_prob_matrix = function(garden_name, season_name) {
  
  network = net_by_garden %>% 
    filter(Botanical_garden == garden_name, Season == season_name) %>% 
    select(Interaction_network) %>% 
    pull(Interaction_network) %>% 
    .[[1]]
  
  poll_order = tibble(Pollinators = colnames(network))
  plant_order = tibble(Plants = rownames(network))
  
  pollinator_abundance1 = pollinator_abundance %>% 
    filter(Botanical_garden == garden_name, Season == season_name) 
  
  floral_abundance1 = floral_abundance %>% 
    filter(Botanical_garden == garden_name, Season == season_name)
  
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


# Now run it for each garden-season combination
adund_prob_matrices_by_garden_season = net_by_garden %>%
  mutate(Prob_matrix = pmap(
    list(Botanical_garden, Season),
    build_prob_matrix
  ))

# ======================================================
#Save network matrices
saveRDS(adund_prob_matrices_by_garden_season, 
        "Data/Working_files/abundance_networks_only_phenobs_by_season.rds")

