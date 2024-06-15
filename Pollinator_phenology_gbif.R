#In this script we download occurrences for the pollinator species
#in order to build a more accurate phenology

#1) Download occurrences from GBIF from the year of sampling (2023) 
#Download records from countries nearby
#Germany, Poland, Austia and Czechia

#Find centroid of the three cities where the botanical gardens are
#and create circunferences of radius that contain occurrence records


#Load libraries
library(dplyr) 
library(rgbif) 
library(openssl)
library(usethis) 
library(tidygeocoder) #Get coordinates from city names
library(REdaS) #to convert degrees to radians


#1) Download GBIF occurrences-----
#Read data and generate list of species
#Load interaction data
int_data = readRDS("Data/Working_files/interaction_data.rds")
#Create vector with species
species = int_data_polls = int_data %>% 
filter(Pollinator_rank=="SPECIES") %>%
filter(!Pollinator == "None") %>% 
select(Pollinator) %>% 
distinct() %>% 
pull()
#First create species key (some long numbers that are needed to download the data) 
#For loop to do it for all species
i <- NULL
gbif_id <- list()

for(i in species){
    
    j <- gsub(" ", "_", i)
    gbif_id[[j]] <- name_backbone(name=i, rank = "species")$usageKey
}

#convert list to dataframe
gbif_id <- data.frame(unlist(gbif_id))
#rename col
colnames(gbif_id) <- "key_number"

#Countries of interest with ISO codes
#Include Germany, Austria, Czechia, Poland
countries <- c("GE", "AT", "CZ", "PL") 

#Download data
occ_download(
pred("hasGeospatialIssue", FALSE),
pred("hasCoordinate", TRUE),
pred("occurrenceStatus","PRESENT"), 
pred_not(pred_in("basisOfRecord",c("LIVING_SPECIMEN"))),
pred_in("taxonKey",c(gbif_id$key_number)),
format = "SIMPLE_CSV",
pred_in("country", c("GE", "AT", "CZ", "PL")))

#Read now the downloaded occurrence data
occurrence = readr::read_delim("Data/oc_data.csv", delim = "\t")



#2 Find centroid of 3 cities-----
#Create tibble with the three cities
cities = tibble(Cities = c("Jena", "Halle", "Leipzig"))
#First find approximate coordinates for Jena, Halle and Leipzig
coords = cities %>%  geocode(Cities, method = 'osm', lat = Latitude , long = Longitude)
#Jena
lat1 = coords %>% filter(Cities == "Jena") %>%  pull(Latitude)
lon1 = coords %>% filter(Cities == "Jena") %>%  pull(Longitude)
#Halle
lat2 = coords %>% filter(Cities == "Halle") %>%  pull(Latitude)
lon2 = coords %>% filter(Cities == "Halle") %>%  pull(Longitude)
#Leipzig
lat3 = coords %>% filter(Cities == "Leipzig") %>%  pull(Latitude)
lon3 = coords %>% filter(Cities == "Leipzig") %>%  pull(Longitude)
#Convert latitude and longitude to radians
lat1_rad = deg2rad(lat1)
lon1_rad = deg2rad(lon1)
lat2_rad = deg2rad(lat2)
lon2_rad = deg2rad(lon2)
lat3_rad = deg2rad(lat3)
lon3_rad = deg2rad(lon3)
#Convert spherical coordinates to Cartesian coordinates
x1 = cos(lat1_rad) * cos(lon1_rad)
y1 = cos(lat1_rad) * sin(lon1_rad)
z1 = sin(lat1_rad)
x2 = cos(lat2_rad) * cos(lon2_rad)
y2 = cos(lat2_rad) * sin(lon2_rad)
z2 = sin(lat2_rad)
x3 = cos(lat3_rad) * cos(lon3_rad)
y3 = cos(lat3_rad) * sin(lon3_rad)
z3 = sin(lat3_rad)
#Calculate the centroid in Cartesian coordinates
x_centroid = (x1 + x2 + x3) / 3
y_centroid = (y1 + y2 + y3) / 3
z_centroid = (z1 + z2 + z3) / 3
#Convert the centroid back to spherical coordinates
hyp = sqrt(x_centroid^2 + y_centroid^2)
lat_centroid = atan2(z_centroid, hyp)
lon_centroid = atan2(y_centroid, x_centroid)
#Convert radians back to degrees
lat_centroid_deg = rad2deg(lat_centroid)
lon_centroid_deg = rad2deg(lon_centroid)
#Store it in a tibble
centroid = tibble(Latitude = lat_centroid_deg, Longitude = lon_centroid_deg)
