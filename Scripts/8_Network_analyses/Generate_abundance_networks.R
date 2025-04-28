#Probability abundance networks
#This requires multipliying the relative abundance 
#of plants and pollinators

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

#Pollinator abundance
pollinator_abundance = interaction_data %>% 
group_by(Botanical_garden, Pollinator) %>% 
summarise(Individuals = length(Pollinator)) %>% 
mutate(Relative_abundance = Individuals/ max(Individuals))

#Floral abundance
#Two setp process: select distinct values with Date_time
#and sum abundances
floral_abundance = interaction_data %>% 
  select(Botanical_garden, Plant, Floral_abundance, Date_time) %>% 
  distinct() %>% 
  group_by(Plant) %>% 
  summarise(Total_floral_abundance = sum(Floral_abundance)) %>% 
  mutate(Relative_abundance = Total_floral_abundance/ max(Total_floral_abundance))

#Load plant-poll networks by garden
#This will allow to reorder by col and row names
net_by_garden = readRDS("Data/Working_files/networks_by_garden.rds")
polls = colnames(net_by_garden$Interaction_network[[1]])
plants = rownames(net_by_garden$Interaction_network[[1]])

#Pollinator abundance
pollinator_abundance = interaction_data %>% 
  group_by(Botanical_garden, Pollinators) %>% 
  summarise(Individuals = length(Pollinators)) %>% 
  mutate(Relative_abundance = Individuals/ max(Individuals))

#Floral abundance
#Two setp process: select distinct values with Date_time
#and sum abundances
floral_abundance = interaction_data %>% 
  select(Botanical_garden, Plants, Floral_abundance, Date_time) %>% 
  distinct() %>% 
  group_by(Botanical_garden, Plants) %>% 
  summarise(Total_floral_abundance = sum(Floral_abundance)) %>% 
  mutate(Relative_abundance = Total_floral_abundance/ max(Total_floral_abundance))

#Load plant-poll networks by garden
net_by_garden = readRDS("Data/Working_files/networks_by_garden.rds")
poll_order = tibble(Pollinators = colnames(net_by_garden$Interaction_network[[1]]))
plant_order = tibble(Plants = rownames(net_by_garden$Interaction_network[[1]]))

pollinator_abundance= pollinator_abundance %>% 
  filter(Botanical_garden == "Leipzig") 
  
floral_abundance = floral_abundance %>% 
  filter(Botanical_garden == "Leipzig")

poll_abund_ordered = left_join(poll_order, pollinator_abundance)
plant_abund_ordered = left_join(plant_order, floral_abundance)
#get the ordered vectors
poll_vector = poll_abund_ordered$Relative_abundance
plant_vector = plant_abund_ordered$Relative_abundance

prob_matrix = outer(plant_vector, poll_vector, FUN = "*")

# Assign dim names to match network structure
rownames(prob_matrix) = plant_abund_ordered$Plants
colnames(prob_matrix) = poll_abund_ordered$Pollinators






library(vegan)
s = procrustes(net_by_garden$Interaction_network[[1]], prob_matrix)
summary(s)
