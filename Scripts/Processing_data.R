#Botanical garden plant-pollinator dataset
#Dataset description----
#Plant-pollinator interactions from 3 different cities Jena-Halle-Leipzig
#Single flowering season (Year 2023)
#Two methods of sampling: focal observations and random census
#We recorded interaction frequency and number of flowers of each species per unit of time
#In addition, focal species have the approximate number of surrounding flowers in a 1 meter radius

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
plant_spp = str_replace(plant_spp, "Erica herbacea", "Erica carnea")

#Check for futher taxonomic info
matched_gbif_plants = name_backbone_checklist(name = plant_spp, kingdom='plants')
#2.2
poll_spp = data %>% 
distinct(Pollinator) %>% 
pull()
#Check for futher taxonomic info
matched_gbif_pollinators = name_backbone_checklist(name = plant_spp, kingdom='plants')

