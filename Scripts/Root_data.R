

#Load libraries
library(tidyr)
library(dplyr)

#Read Groot data
groot = readr::read_csv("Data/GRooTAggregateSpeciesVersion.csv")
spp_names = readr::read_csv("Data/Spp_names.csv")

#Create Species col
groot = groot %>% 
unite(Species, c("genusTNRS", "speciesTNRS"), sep = " ") 

#Rename spp col to merge
spp_names = spp_names %>% 
rename(Species = Accepted_name)  

#Merge
d = left_join(spp_names, groot)

#Check number of available data
1 - sum(is.na(d$traitName)) / length(unique(d$Species))
#Almost 60%, not bad!
75-31
