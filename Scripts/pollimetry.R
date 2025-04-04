
#Install packages
#devtools::install_github("liamkendall/pollimetry")
#devtools::install_github("liamkendall/pollimetrydata")
#Load library
library(pollimetry)
library(pollimetrydata)
library(dplyr)

#Load pollinator trait data
polltraits = readRDS("Data/Trait_data/Processed/PollTraits.rds")

#Read data
matched_gbif_pollinators = readRDS("Data/Working_files/matched_gbif_pollinators.rds")

#Join data
d = left_join(polltraits, matched_gbif_pollinators)

#Select cols of interest and rename to extrapolate tongue length
colnames(d)
#This is only implemented for these families
bee_fam = c("Andrenidae", "Apidae", "Colletidae", "Halictidae", "Megachilidae")

d1 = d %>% 
select(c(IT_mm, Pollinator_family)) %>% 
rename(Family = Pollinator_family) %>% 
rename(IT = IT_mm) %>% 
filter(Family %in% bee_fam)


tongue_length = tonguelength(d1, mouthpart="all")


