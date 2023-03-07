#Script to explore trait data and number of species

#Load libraries
library(readr)
library(dplyr)

#Load data
data = read_csv("Data/Traits_JeHaLe.csv")

#First, check number of species
s = data %>% select(Species) %>% distinct()

#Calculate mean value of each trait when possible
f = data %>% 
group_by(Species) %>% 
summarise(across(where(is.numeric), mean))
