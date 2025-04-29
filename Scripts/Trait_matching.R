#Prepare corolla tube length and proboscis length for trait matching analyses
#Note that proboscis length is an approximation of the real ones
#This was done with the pollimetry package, 
#our images and genus level/family level information

#Load libraries
library(dplyr)
library(readr)
library(stringr)

conflicted::conflicts_prefer(dplyr::filter)
#Prepare floral traits for trait matching analyses
morphometrics = read_csv("Data/Trait_data/Raw/ReproductiveTraits_Morphometrics.csv")
colnames(morphometrics)
#1)Prepare traits used in Lanuza 2023
lanuza_2023_traits = morphometrics %>% 
  select(
    Species,
    Flower_number, 
    Flower_width,
    Style_length, 
    `Ovule_number/flower`,
    Plant_height_mm,
    Floral_tube_length) %>% 
  rename(Flowers_per_plant = Flower_number) %>% 
  rename(Corolla_diameter_mean = Flower_width) %>% 
  rename(Ovule_number = `Ovule_number/flower`) %>% 
  rename(Plant_height_mean_m = Plant_height_mm) %>% 
  mutate(Plant_height_mean_m = Plant_height_mean_m/1000) %>% 
  mutate(Style_length = as.numeric(Style_length)) %>% 
  group_by(Species) %>%
  summarise(
    Flowers_per_plant = mean(Flowers_per_plant, na.rm = TRUE),
    Corolla_diameter_mean = mean(Corolla_diameter_mean, na.rm = TRUE),
    Style_length = mean(Style_length, na.rm = TRUE),
    Ovule_number = mean(Ovule_number, na.rm = TRUE),
    Plant_height_mean_m = mean(Plant_height_mean_m, na.rm = TRUE),
    Floral_tube_mean_length = mean(Floral_tube_length, na.rm = TRUE))

#Load pollinator trait data
polltraits_mean = readRDS("Data/Trait_data/Processed/PollTraits_all.rds")

p1 = polltraits_mean %>% 
  ggplot(aes(IT_mean)) +
  geom_histogram(colour="black", fill="plum3") +
  theme_bw()+
  coord_cartesian(expand = FALSE) + 
  xlab("IT distance (mm)") +
  ylab("Counts")


p2 = polltraits_mean %>% 
  ggplot(aes(Body_length_mean)) +
  geom_histogram(colour="black", fill="steelblue3") +
  theme_bw()+
  coord_cartesian(expand = FALSE) + 
  xlab("Body length (mm)") +
  ylab("Counts")

p3 = polltraits_mean %>% 
  ggplot(aes(Body_length_mean)) +
  geom_histogram(colour="black", fill="tomato3") +
  theme_bw()+
  coord_cartesian(expand = FALSE) + 
  xlab("Proboscis length (mm)") +
  ylab("Counts")


#All ready!
polltraits = polltraits_mean %>% 
  rename(Pollinators = Pollinator_accepted_name) %>% 
  rename(Proboscis_length = Proboscis_length_mean) %>% 
  select(Pollinators, Proboscis_length)

planttraits = lanuza_2023_traits %>% 
  rename(Plants = Species) %>% 
  rename(Floral_tube_length = Floral_tube_mean_length) %>% 
  select(Plants, Floral_tube_length)  

#Create tibble with all combinations
species_combinations = expand.grid(Pollinators = polltraits$Pollinators, Plants = planttraits$Plants)

species_combinations1 = left_join(species_combinations, polltraits,
                                  by = "Pollinators")
#This would be the final, make it short for simplicity
d = left_join(species_combinations1, planttraits,
                                  by = "Plants")

#Ratio 
d$Trait_ratio = d$Proboscis_length / d$Floral_tube_length
#Calculate an absolute value of distance
d$Trait_difference = d$Proboscis_length - d$Floral_tube_length

#Visualize distributions
library(ggplot2)
ggplot(d, aes(x = Trait_ratio)) +
  geom_histogram() +
  theme_minimal() 

ggplot(d, aes(x = Trait_difference)) +
  geom_histogram() +
  theme_minimal() 


#Load interaction data
#Load data
raw_data = readRDS("Data/Working_files/interaction_data.rds")
interaction_data = raw_data  %>% 
  filter(!is.na(Interactions),
         !is.na(Floral_abundance),
         Pollinator != "None")  %>% 
  rename(Plants = Plant_accepted_name,
         Pollinators = Pollinator_accepted_name) %>% 
  filter(!is.na(Pollinators))


