#Script to prepare trait data of plants
#Load libraries
library(readr) #to read files
library(dplyr) #process data
library(ggplot2) #visualize data
library(stringr) #process strings
#Steps:
#Read unprocessed trait data
#1)Traits data from PhenObs and trait data from PollObs (this project)
#PhenoObs is vegetative data and PollObs reproductive data


#1)Read plant traits
#read data
selfing = read_csv("Data/Trait_data/Processed/Selfing.csv")


reproductive_traits = read_csv("Data/Trait_data/Raw/ReproductiveTraits_Morphometrics.csv")


leipzig_phenobs %>% 
distinct(Species) %>% 
pull()

colnames(halle_phenobs)
s = leipzig_phenobs %>% 
filter(Species == "Scabiosa ochroleuca") %>% 
filter(`Flowers opening` == "y") %>% 
dplyr::select(Species, Date, `Flowers opening`, Doy)
s


