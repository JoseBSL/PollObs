# ======================================================
#Probability probability abundance networks for each garden
#And compute correlation with int and int frequency networks
# ======================================================

#Load libraries
library(dplyr)
library(purrr)
library(tidyr)
library(ggplot2)
library(vegan) #for mantes and procrustes

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

#Load plant-poll networks by garden
net_by_garden = readRDS("Data/Working_files/networks_by_garden_only_phenobs.rds")

# ======================================================
# Create vector of main orders
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
  filter(Pollinator_order %in% poll_order)%>% 
  filter(!Plants == "Iberis sempervirens") 

interaction_data = interaction_data %>% 
  filter(Plant %in% phenobs_spp) 

# Pollinator abundance
pollinator_abundance = interaction_data %>% 
  group_by(Botanical_garden, Pollinators) %>% 
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
  select(Botanical_garden, Plants, Floral_abundance, Date_time) %>% 
  distinct() %>% 
  group_by(Botanical_garden, Plants) %>% 
  summarise(Total_floral_abundance = sum(Floral_abundance)) %>% 
  mutate(Relative_abundance = Total_floral_abundance/ max(Total_floral_abundance))
# Check distribution
floral_abundance %>% 
  ggplot(aes(Relative_abundance)) +
  facet_wrap(~ Botanical_garden) +
  geom_histogram()

# ======================================================
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
  
  floral_abundance1 = floral_abundance %>% 
    filter(Botanical_garden == garden_name)
  
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

#Run it for each garden
adund_prob_matrices_by_garden = net_by_garden %>%
  mutate(Prob_matrix = map(Botanical_garden, build_prob_matrix))

# ======================================================
#Save network matrices
saveRDS(adund_prob_matrices_by_garden, 
        "Data/Working_files/abundance_networks_only_phenobs.rds")

#Safety check
#Note that abundances of Iberis sempervivens
#were too high on Halle, at the moment I am excluding it but I am sure
#those were overestimated for several reasons
#flowers were tiny and I counted the whole patch
#Maybe I need to consider inflorescence level and divide the size by 2 or 3
pollinator_abundance_total <- interaction_data %>%
  group_by(Botanical_garden, Date) %>%
  summarise(Individuals = n()) %>%
  group_by(Botanical_garden) %>%
  mutate(Relative_abundance = Individuals / max(Individuals))

floral_abundance_total <- interaction_data %>%
  group_by(Botanical_garden, Date) %>%
  summarise(Floral_abundance = sum(Floral_abundance, na.rm = TRUE)) %>%
  group_by(Botanical_garden) %>%
  mutate(Relative_floral_abundance = Floral_abundance / max(Floral_abundance))

combined_abundance <- left_join(
  pollinator_abundance_total,
  floral_abundance_total,
  by = c("Botanical_garden", "Date")
)

ggplot(combined_abundance, aes(x = Date)) +
  geom_line(aes(y = Relative_abundance, color = "Pollinators"), size = 1) +
  geom_point(aes(y = Relative_abundance, color = "Pollinators"), size = 2) +
  geom_line(aes(y = Relative_floral_abundance, color = "Flowers"), size = 1, linetype = "dashed") +
  geom_point(aes(y = Relative_floral_abundance, color = "Flowers"), size = 2, shape = 21, fill = "white") +
  facet_wrap(~ Botanical_garden, scales = "free_x") +
  scale_color_manual(values = c("Pollinators" = "#0072B2", "Flowers" = "tomato3")) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.title = element_blank()
  ) +
  labs(
    y = "Relative Abundance",
    x = "Date",
    title = "Relative Abundance Over Time"
  )

