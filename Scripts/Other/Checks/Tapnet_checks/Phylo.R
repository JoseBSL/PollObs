# ======================================================
#Script: Script to obtain phylogenetic info
# ======================================================

# Load libraries
library(dplyr)
library(rtrees)
library(ggtree)
library(stringr) 
library(ape) #to build distance matrix
library(ggplot2)

# ======================================================
# Load data
# Interaction data
raw_data = readRDS("Data/Working_files/interaction_data.rds")
# Phenobs spp
phenobs_spp = readRDS("Data/Working_files/phenobs_spp.rds")

# Create vector of main bee families
bee_fam = c("Megachilidae", 
            "Apidae",
            "Colletidae",
            "Andrenidae",
            "Halictidae",
            "Mellittidae")

# ======================================================
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
  filter(Pollinator_family %in% bee_fam) %>% 
  filter(!Plants == "Iberis sempervirens") 

interaction_data = interaction_data %>% 
  filter(Plant %in% phenobs_spp) 

# ======================================================
# First, get phylogenetic information for plants
spp_list = interaction_data %>% 
  select(c(Plants, Plant_genus, Plant_family)) %>% 
  distinct() %>% 
  rename(species = Plants,
         genus = Plant_genus,
         family = Plant_family)

#Note these three species are added at family level
#Betonica_officinalis, Viscaria_vulgaris, Eriocapitella_hupehensis
# By using the synonym of Viscaria we can fix this
spp_list = spp_list %>% 
  mutate(species = if_else(species == "Viscaria vulgaris", 
                           "Silene viscaria",
                           species)) %>% 
  mutate(genus = if_else(genus == "Viscaria", 
                         "Silene",
                         genus))

spp_list = spp_list %>% 
  mutate(species = if_else(species == "Eriocapitella hupehensis", 
                           "Anemone hupehensis",
                           species)) %>% 
  mutate(genus = if_else(genus == "Eriocapitella", 
                         "Anemone",
                         genus))

#Get phylo from megratree
plant_phylo = get_tree(sp_list = spp_list,  
                       taxon = "plant")

#Conduct visual check of number of spp added at genus/family level
checks = plant_phylo$graft_status

# Replace underscore
plant_phylo$tip.label = str_replace(plant_phylo$tip.label, "_", " ")

ggtree(plant_phylo, size=0.1, open.angle=5, alpha=0.5) +
  geom_tippoint(colour='cyan4') +
  geom_tiplab(linetype='dashed', linesize=.05, 
              size=1.75, color= "black", offset = 0.2, fontface=2) +
  theme(plot.margin = margin(5, 50, 5, 5)) +
  coord_cartesian(clip = "off")


# Convert to a matrix
plant_phylo$tip.label = str_replace(plant_phylo$tip.label, "Silene viscaria", "Viscaria vulgaris")
plant_phylo$tip.label = str_replace(plant_phylo$tip.label, "Anemone hupehensis", "Eriocapitella hupehensis")

plant_dist_matrix = cophenetic.phylo(plant_phylo)
plant_dist_matrix = plant_dist_matrix/ max(plant_dist_matrix)
max(plant_dist_matrix)
#Save data
saveRDS(plant_dist_matrix, "Data/Working_files/plant_dist_matrix_bees.rds")
# ======================================================
# Second, get phylogenetic information for pollinators
poll_taxonomic_info = interaction_data %>% 
  select(c(Pollinators, Pollinator_genus, Pollinator_family, Pollinator_order)) %>% 
  distinct() %>% 
  rename(species = Pollinators,
         genus = Pollinator_genus,
         family = Pollinator_family,
         order = Pollinator_order) %>% 
  select(order, family, genus, species)

# Set hierarchy
poll_hierarchy = ~order/family/genus/species
# Convert all columns to factor
poll_taxonomic_info = poll_taxonomic_info %>%
  mutate(across(everything(), as.factor))

poll_phylo = as.phylo(poll_hierarchy, data = poll_taxonomic_info, collapse=FALSE)

poll_dist_matrix = cophenetic.phylo(poll_phylo)
#Scale both to max value 1
poll_dist_matrix = poll_dist_matrix/ max(poll_dist_matrix)
max(poll_dist_matrix)
#Save data
saveRDS(poll_dist_matrix, "Data/Working_files/pollinator_dist_matrix_bees.rds")

ggtree(poll_phylo, size=0.1, open.angle=5, alpha=0.5) +
  geom_tippoint(colour='cyan4') +
  geom_tiplab(linetype='dashed', linesize=.05, 
              size=1.75, color= "black", offset = 0.2, fontface=2) +
  theme(plot.margin = margin(5, 50, 5, 5)) +
  coord_cartesian(clip = "off")


