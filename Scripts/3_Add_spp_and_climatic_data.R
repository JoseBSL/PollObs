#Script to organize interaction dataset
#Add further taxonomic info and climatic data (temperature and humidity)

#Load libraries
library(dplyr)
library(readr)
library(rgbif) #To extract taxonomic information
library(stringr)

#Read data
interaction_data = read_csv("Data/PollObs_all.csv")
#Load taxonomy
plants = readRDS("Data/Working_files/matched_gbif_plants.rds")
polls = readRDS("Data/Working_files/matched_gbif_pollinators.rds")
#Load climatic data
leipzig_weather = readRDS("Data/Working_files/leipzig_weather.rds")
halle_weather = readRDS("Data/Working_files/halle_weather.rds")
jena_weather = readRDS("Data/Working_files/jena_weather.rds")

#Select coloumns of interest from interaction data
colnames(interaction_data)

interaction_data = 
interaction_data %>% 
select(Botanical_garden, Plant, Pollinator,
       Interactions, Floral_abundance, Capitulum,
       Flowers_per_capitulum, Flowering_neighbours_intensity,
       Time_start, Time_finish,
       Total_time_species, Year, Month, Day,
       Random_census_stop, Sampling)

#Add taxonomic info
interaction_data


#When accepted name is na add canonical name to accepted name
#Plants and polls
#add taxo
#Add climatic data




