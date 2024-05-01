#Botanical garden plant-pollinator dataset
#Dataset description----
#Plant-pollinator interactions from 3 different cities Jena-Halle-Leipzig
#Single flowering season (Year 2023)
#Two methods of sampling: focal observations and random census
#We recorded interaction frequency and number of flowers of each species per unit of time
#In addition, focal species have the approximate number of surrounding flowers in a 1 meter radius

#Note: There are few Hymenoptera specimens without id ~15 specimens 

#In this script----
#1) Check for typos in cols
#2) Retrieve taxonomic info

#Load libraries
library(dplyr)
library(readr)
library(rgbif) #To extract taxonomic information
library(stringr)

#Read data
data = read_csv("Data/PollObs_all.csv")

#Read function to re-structure data
#Fix str of data from GBIF
change_str <- function(data) { 
data = data %>% 
select(!c(usageKey, confidence, kingdomKey,
          phylumKey, classKey, orderKey, familyKey,
         genusKey,  speciesKey, acceptedUsageKey,
         verbatim_index)) %>% 
rename(Fixed_name = verbatim_name,
       Scientific_name  = scientificName,
       Canonical_name  = canonicalName,
       Accepted_name = species) %>% 
select(Fixed_name, rank, status, matchType, 
       Scientific_name, Canonical_name,
       Accepted_name, phylum, order, family,
       genus) %>% 
  rename_all(~str_to_title(.))
}



#Check cols
colnames(data)

#Check levels
levels(factor(data$Botanical_garden))
levels(factor(data$Plant))
levels(factor(data$Pollinator))
levels(factor(data$Status))
levels(factor(data$ID_by))
levels(factor(data$Pollinator_id))
levels(factor(data$Image_link))
levels(factor(data$Interactions))
levels(factor(data$Floral_abundance))
levels(factor(data$Capitulum))
levels(factor(data$Flowers_per_capitulum))
levels(factor(data$Flowering_neighbours_intensity))
levels(factor(data$Time_start))
levels(factor(data$Time_finish))
levels(factor(data$Total_time_species))
levels(factor(data$Year))
levels(factor(data$Month))
levels(factor(data$Day))
levels(factor(data$Weather))
levels(factor(data$Observer))
levels(factor(data$Random_census_stop))
levels(factor(data$Sampling))

#2) Retrieve taxonomic info for plants 
#2.1
plant_spp = data %>% 
distinct(Plant) %>% 
pull()
#Fix some synonyms
plant_spp = str_replace(plant_spp, "Erica herbacea", "Erica carnea")
plant_spp = str_replace(plant_spp, "Potentilla sp", "Potentilla")
plant_spp = str_replace(plant_spp, "Trifolium sp", "Trifolium")
plant_spp = str_replace(plant_spp, "Erica herbacea", "Erica carnea")
plant_spp = str_replace(plant_spp, "Penstemon grandiflorus", "Penstemon bradburyi")
plant_spp = str_replace(plant_spp, "Seseli hippomarathrum", "Hippomarathrum vulgare")

#Check for futher taxonomic info
matched_gbif_plants = name_backbone_checklist(name = plant_spp, kingdom='plants')
matched_gbif_plants = change_str(matched_gbif_plants)
#Add canonical name to accepted name when NA
matched_gbif_plants = matched_gbif_plants %>% 
mutate(Accepted_name = if_else(is.na(Accepted_name), Canonical_name, Accepted_name))
#Rename with Plant as prefix
matched_gbif_plants = matched_gbif_plants %>% 
rename_with( ~ str_to_title(paste0("Plant_", .x))) %>% 
select(!c("Plant_scientific_name", "Plant_canonical_name", "Plant_phylum")) %>% 
rename(Plant = Plant_fixed_name)

#Save data
saveRDS(matched_gbif_plants, "Data/Working_files/matched_gbif_plants.rds")

#2.2
poll_spp = data %>% 
mutate(Pollinator = str_replace(Pollinator, " sp", "")) %>% 
distinct(Pollinator) %>% 
pull() 

#Check for futher taxonomic info
matched_gbif_pollinators = name_backbone_checklist(name = poll_spp, kingdom='arthropoda')

matched_gbif_pollinators = matched_gbif_pollinators %>% 
filter(!verbatim_name == "None") %>% 
filter(!verbatim_name == "Unidentified") %>% 
mutate(matchType = case_when(verbatim_name == "Anthribidae" ~ "EXACT", 
                             T ~ matchType))
#Fix structure
matched_gbif_pollinators = change_str(matched_gbif_pollinators)
#Add canonical name to accepted name when NA
matched_gbif_pollinators = matched_gbif_pollinators %>% 
mutate(Accepted_name = if_else(is.na(Accepted_name), Canonical_name, Accepted_name))
#Rename with Pollinator as prefix
matched_gbif_pollinators = matched_gbif_pollinators %>% 
rename_with( ~ str_to_title(paste0("Pollinator_", .x))) %>% 
select(!c("Pollinator_scientific_name", "Pollinator_canonical_name", "Pollinator_phylum")) %>% 
rename(Pollinator = Pollinator_fixed_name)
#Save data
saveRDS(matched_gbif_pollinators, "Data/Working_files/matched_gbif_pollinators.rds")

