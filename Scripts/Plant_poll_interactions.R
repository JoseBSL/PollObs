#Botanical garden plant-pollinator dataset
#Dataset description----
#Plant-pollinator interactions from 3 different cities Jena-Halle-Leipzig
#Single flowering season (Year 2023)
#Two methods of sampling: focal observations and random census
#We recorded interaction frequency and number of flowers of each species per unit of time
#In addition, focal species have the approximate number of surrounding flowers in a 1 meter radius


#Download plant-pollinator data
library(googlesheets4)

#Read data/takes a while
plant_poll <- read_sheet("https://docs.google.com/spreadsheets/d/1K9zR8M8hpCJEoiSx1oGjaB4PNSD062S5cVdojNjnFpU/edit#gid=2114344765", 
                         sheet = "Plant_poll_interactions")


#Download environmental conditions (e.g., temperature, humidity, wind)
#We need to download it from some data loggers 