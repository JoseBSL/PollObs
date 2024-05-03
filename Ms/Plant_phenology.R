
#Script to prepare plant phenology (data from froPhenObs app)
#Load libraries
library(dplyr)
library(readr)

#Load interaction data
int_data = readRDS("Data/Working_files/interaction_data.rds")
#Load phenology data
jena_phenobs = read_csv("Data/Phenology_data/raw_phenology_jena.csv")

#First get focal species recorded in jena in the int dataset
jena_focals = int_data %>% 
filter(Botanical_garden == "Jena") %>% 
filter(Sampling == "Focal") %>% 
distinct(Plant) %>% 
select(Plant) %>% 
rename(Species = Plant)

#Rename cols with underscore
colnames(jena_phenobs) = str_replace(colnames(jena_phenobs), " ", "_")

#Prepare data with right format of dates
flowering_data = jena_phenobs %>% 
select(Date, Species, Flowers_opening, Flowering_intensity) %>% 
mutate(Date = as.Date(str_replace_all(Date, "[.]", "/"), "%d/%m/%Y")) %>% 
group_by(Species) %>% 
filter(Flowers_opening == "y") %>% 
mutate(Species = recode_factor(Species, "Anemone pulsatilla" = "Pulsatilla vulgaris")) %>% 




#Fix some species names first and homogenize with our dataset

d = left_join(jena_focals, flowering_data)

d %>% 
filter(is.na(Date))


