#In this script we download occurrences for the pollinator species
#Download records from countries nearby
#Germany, Poland, Austia, Czechia
#Belgium, Netherlands, Slovaquia, France and Hungary

#Skip this part and use directly the downloaded data from the DOI
#https://doi.org/10.15468/dl.5s5kuf

#Load libraries
library(dplyr) 
library(rgbif) 
library(openssl)
library(usethis) 
library(tidygeocoder) #Get coordinates from city names
library(REdaS) #to convert degrees to radians
library(lubridate)

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

#Download data
occ_download(
pred("hasGeospatialIssue", FALSE),
pred("hasCoordinate", TRUE),
pred("occurrenceStatus","PRESENT"), 
pred_not(pred_in("basisOfRecord",c("LIVING_SPECIMEN"))),
pred_in("taxonKey",c(gbif_id$key_number)),
format = "SIMPLE_CSV",
pred_gte("year", 2021), #after year 2020 to reduce number of records 
pred_in("country", c("DE", "AT", "CZ", "PL", "NL", "BE", "SK", "FR", "HU")))



