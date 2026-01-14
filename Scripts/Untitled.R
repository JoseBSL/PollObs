
# Explore if some pollinators 

#Load libraries
library(dplyr)

#Load data
raw_data = readRDS("Data/Working_files/interaction_data.rds")
phenobs_spp = readRDS("Data/Working_files/phenobs_spp.rds")

# Create vector of main orders
poll_order = c("Hymenoptera", "Diptera")

colnames(raw_data)
# Prepare data
int_data = raw_data %>%
  filter(Plant_rank == "SPECIES") %>%
  select(Plant, Plant_accepted_name, Pollinator_accepted_name, Botanical_garden, Sampling, Plant_family,
         Plant_genus) %>%
  rename(Plants = Plant_accepted_name,
         Pollinators = Pollinator_accepted_name) %>%
  filter(Plant %in% phenobs_spp) %>% 
  filter(Pollinators %in% poll_order)




# Check unique number of PhenObs species by garden
phenobs_by_garden = int_data %>% 
filter(Plant %in% phenobs_spp) %>% 
select(Botanical_garden, Plant) %>% 
group_by(Botanical_garden) %>% 
summarise(Phenobs_spp = n_distinct(Plant))

phenos_h = phenobs_by_garden$Phenobs_spp[1]
phenos_j = phenobs_by_garden$Phenobs_spp[2]
phenos_l = phenobs_by_garden$Phenobs_spp[3]

int_data %>% 
  filter(Plant %in% phenobs_spp) %>% 
  distinct(Plant_family) %>% 
  nrow()

int_data %>% 
  filter(Plant %in% phenobs_spp) %>% 
  distinct(Plant_family) %>% 
  nrow()

int_data %>% 
  filter(Plant %in% phenobs_spp) %>% 
  distinct(Plant_genus) %>% 
  nrow()

# Check total sampling time per garden
colnames(raw_data)

time_per_garden = raw_data %>% 
  filter(Plant %in% phenobs_spp) %>% 
  select(Botanical_garden, Date, Total_time_species, Plant) %>% 
  distinct() %>% 
  group_by(Botanical_garden) %>% 
  summarise(Total_time = sum(Total_time_species) / 60)

halle_hours = round(time_per_garden$Total_time[1], 1)
jena_hours = round(time_per_garden$Total_time[2], 1)
leipzig_hours = round(time_per_garden$Total_time[3],1)


time_per_garden = raw_data %>% 
  filter(Plant %in% phenobs_spp) %>% 
  select(Botanical_garden, Date, Total_time_species, Plant) %>% 
  distinct() %>% 
  group_by(Botanical_garden) %>% 
  summarise(Total_time = sum(Total_time_species) / 60)


# First, get phylogenetic information for plants
spp_list = interaction_data %>% 
  select(c(Plants, Plant_genus, Plant_family)) %>% 
  distinct() %>% 
  rename(species = Plants,
         genus = Plant_genus,
         family = Plant_family) %>% 
  mutate(species=family,
         genus = family)

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

  
# Replace underscore
plant_phylo$tip.label = str_replace(plant_phylo$tip.label, "_", " ")

ggtree(plant_phylo, size=0.1, open.angle=5, alpha=0.5) +
  geom_tippoint(colour='cyan4') +
  geom_tiplab(linetype='dashed', linesize=.05, 
              size=1.75, color= "black", offset = 0.2, fontface=2) +
  theme(plot.margin = margin(5, 50, 5, 5)) +
  coord_cartesian(clip = "off")
