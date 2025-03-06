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
#Vegetative data
vegetative_traits = read_csv("Data/Trait_data/Raw/PlantVegetativeTraits.csv")
#Reproductive data
reproductive_traits = read_csv("Data/Trait_data/Raw/ReproductiveTraits_Morphometrics.csv")
