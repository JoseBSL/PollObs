#Script to build phenology matrices
#Two type of matrices
#1)Presence/absence in time
#2)Average probability of encounter based on their density distributions


#Load libraries
library(dplyr)
library(purrr)
library(tidyr)
library(ggplot2)

#Load phenology data of both plants and pollinators
#Read botanical garden data
jena_phen = readRDS("Data/Phenology_data/clean_plant_phenobs_jena.rds")
halle_phen = readRDS("Data/Phenology_data/clean_plant_phenobs_halle.rds")
leipzig_phen = readRDS("Data/Phenology_data/clean_plant_phenobs_leipzig.rds")

#Fix missing label on each garden
jena_phen = jena_phen %>% 
  mutate(Garden = "Jena")
halle_phen = halle_phen %>% 
  mutate(Garden = "Halle")
leipzig_phen = leipzig_phen %>% 
  mutate(Garden = "Leipzig")
#Bind all gardens and create the plant ohenology dataset
plant_phen = bind_rows(jena_phen, halle_phen, leipzig_phen)

#Check if there are some flowering plants in other year distinct to 2023
s = plant_phen %>% 
filter(is.na(Date))
#All of those can be back converted from DOY
plant_phen_na = plant_phen %>% 
  filter(is.na(Date)) %>% 
  mutate(Date = as.Date(Doy - 1, origin = paste0(2023, "-01-01")))

plant_phen_non_na = plant_phen %>% 
  filter(!is.na(Date))

#Bind everything together
plant_phen_fixed = bind_rows(plant_phen_na, plant_phen_non_na)
plant_phen_fixed = plant_phen_fixed %>% 
  mutate(Flowers_opening = if_else(Flowers_opening == "y", "Yes", "No"))
  