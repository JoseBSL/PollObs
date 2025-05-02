#Probability phenology networks for each garden
#And compute correlation with int and int frequency networks

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
  
#This may require to build a continuous flowering number for each species
#So we may need to model all of them

#We can skip that by assigning to each sampling date
#the number of flowers of that week

#think about it



