
#Variable descriptors
raw_data = readRDS("Data/Working_files/interaction_data.rds")
colnames(raw_data)


int_data = raw_data %>%
  filter(Plant_rank == "SPECIES", 
         Pollinator_rank == "SPECIES") %>%
  select(Plant, Plant_accepted_name, Pollinator_accepted_name, Botanical_garden, Sampling) %>%
  rename(Plants = Plant_accepted_name,
         Pollinators = Pollinator_accepted_name) %>%
  distinct()


phenobs_spp = readRDS("Data/Working_files/phenobs_spp.rds")
phenobs_spp = str_replace(phenobs_spp, "Persicaria bistorta", "Polygonum bistorta") 
phenobs_spp = str_replace(phenobs_spp, "Aquilegia chrysantha", "Aquilegia vulgaris") 

a = int_data %>% 
filter(Plant %in% phenobs_spp) %>% 
distinct(Plant) %>% 
pull()

setdiff(phenobs_spp, a)


interaction_data %>% 
  summarise(length(Plants))


