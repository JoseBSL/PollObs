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
polltraits = readRDS("Data/Trait_data/Processed/PollTraits_with_proboscis.rds")
#Check cols
colnames(polltraits)
#Obtain average value per species
polltraits_mean = polltraits %>% 
  group_by(Pollinator_accepted_name) %>% 
  summarise(IT_mean = mean(IT, na.rm = TRUE),
          Body_length_mean = mean(Body_length, na.rm = TRUE),
          Proboscis_length_mean = mean(Proboscis_length, na.rm = TRUE)) %>% 
filter(!Pollinator_accepted_name == "Gasteruption") %>% 
mutate(Pollinator_genus = word(Pollinator_accepted_name, 1))
#Some species with missing values can be recovered
na_only_data = polltraits_mean %>%
  filter(if_any(c(Pollinator_accepted_name, IT_mean, Body_length_mean, Proboscis_length_mean), is.na))
#Add those at genus level to avoid missing species
#Hylaeus
Hylaeus = polltraits_mean %>% 
filter(Pollinator_genus == "Hylaeus") %>% 
summarise(IT_mean = mean(IT_mean, na.rm =T),
          Body_length_mean = mean(Body_length_mean, na.rm =T))
Hylaeus_IT = Hylaeus %>% pull(IT_mean)
Hylaeus_body_length = Hylaeus %>% pull(Body_length_mean)

#Now add values back to dataset
polltraits_mean = polltraits_mean %>% 
mutate(IT_mean = 
         if_else(Pollinator_genus == "Hylaeus" & is.na(IT_mean), 
                 Hylaeus_IT, IT_mean)) %>% 
mutate(Body_length_mean = 
          if_else(Pollinator_genus == "Hylaeus" & is.na(Body_length_mean), 
                  Hylaeus_body_length, Body_length_mean))

#Lasioglossum
Lasioglossum = polltraits_mean %>% 
  filter(Pollinator_genus == "Lasioglossum") %>% 
  summarise(IT_mean = mean(IT_mean, na.rm =T),
            Body_length_mean = mean(Body_length_mean, na.rm =T))
Lasioglossum_IT = Lasioglossum %>% pull(IT_mean)
Lasioglossum_body_length = Lasioglossum %>% pull(Body_length_mean)
#Now add values back to dataset
polltraits_mean = polltraits_mean %>% 
  mutate(IT_mean = 
           if_else(Pollinator_genus == "Lasioglossum" & is.na(IT_mean), 
                   Hylaeus_IT, IT_mean)) %>% 
  mutate(Body_length_mean = 
           if_else(Pollinator_genus == "Lasioglossum" & is.na(Body_length_mean), 
                   Hylaeus_body_length, Body_length_mean))
#Pollenia
Pollenia = polltraits_mean %>% 
  filter(Pollinator_genus == "Pollenia") %>% 
  summarise(IT_mean = mean(IT_mean, na.rm =T),
            Body_length_mean = mean(Body_length_mean, na.rm =T))
Pollenia_IT = Pollenia %>% pull(IT_mean)
Pollenia_body_length = Pollenia %>% pull(Body_length_mean)
#Now add values back to dataset
polltraits_mean = polltraits_mean %>% 
  mutate(IT_mean = 
           if_else(Pollinator_genus == "Pollenia" & is.na(IT_mean), 
                   Pollenia_IT, IT_mean)) %>% 
  mutate(Body_length_mean = 
           if_else(Pollinator_genus == "Pollenia" & is.na(Body_length_mean), 
                   Pollenia_body_length, Body_length_mean))

#Lapposyrphus lapponicus add Eupeodes values as it is the closest relative
Eupeodes = polltraits_mean %>% 
  filter(Pollinator_genus == "Eupeodes") %>% 
  summarise(IT_mean = mean(IT_mean, na.rm =T),
            Body_length_mean = mean(Body_length_mean, na.rm =T))
Eupeodes_IT = Eupeodes %>% pull(IT_mean)
Eupeodes_body_length = Eupeodes %>% pull(Body_length_mean)
#Now add values back to dataset
polltraits_mean = polltraits_mean %>% 
  mutate(IT_mean = 
           if_else(Pollinator_genus == "Lapposyrphus" & is.na(IT_mean), 
                   Eupeodes_IT, IT_mean)) %>% 
  mutate(Body_length_mean = 
           if_else(Pollinator_genus == "Lapposyrphus" & is.na(Body_length_mean), 
                   Eupeodes_body_length, Body_length_mean))
#Same with Paragus constrictus
polltraits_mean = polltraits_mean %>% 
  mutate(IT_mean = 
           if_else(Pollinator_genus == "Paragus" & is.na(IT_mean), 
                   Eupeodes_IT, IT_mean)) %>% 
  mutate(Body_length_mean = 
           if_else(Pollinator_genus == "Paragus" & is.na(Body_length_mean), 
                   Eupeodes_body_length, Body_length_mean))

#Meliscaeva auricollis  add Episyrphus values as it is the closest relative
Episyrphus = polltraits_mean %>% 
  filter(Pollinator_genus == "Episyrphus") %>% 
  summarise(IT_mean = mean(IT_mean, na.rm =T),
            Body_length_mean = mean(Body_length_mean, na.rm =T))
Episyrphus_IT = Episyrphus %>% pull(IT_mean)
Episyrphus_body_length = Episyrphus %>% pull(Body_length_mean)

polltraits_mean = polltraits_mean %>% 
  mutate(IT_mean = 
           if_else(Pollinator_genus == "Meliscaeva" & is.na(IT_mean), 
                   Episyrphus_IT, IT_mean)) %>% 
  mutate(Body_length_mean = 
           if_else(Pollinator_genus == "Meliscaeva" & is.na(Body_length_mean), 
                   Episyrphus_body_length, Body_length_mean))

#Dolichovespula saxonica  add vespula value
Vespula = polltraits_mean %>% 
  filter(Pollinator_genus == "Vespula") %>% 
  summarise(IT_mean = mean(IT_mean, na.rm =T),
            Body_length_mean = mean(Body_length_mean, na.rm =T))
Vespula_IT = Vespula %>% pull(IT_mean)
Vespula_body_length = Vespula %>% pull(Body_length_mean)

polltraits_mean = polltraits_mean %>% 
  mutate(IT_mean = 
           if_else(Pollinator_genus == "Dolichovespula" & is.na(IT_mean), 
                   Vespula_IT, IT_mean)) %>% 
  mutate(Body_length_mean = 
           if_else(Pollinator_genus == "Dolichovespula" & is.na(Body_length_mean), 
                   Vespula_body_length, Body_length_mean))

#Minettia longipennis add Botanophila depressa value, similar size
Botanophila = polltraits_mean %>% 
  filter(Pollinator_genus == "Botanophila") %>% 
  summarise(IT_mean = mean(IT_mean, na.rm =T),
            Body_length_mean = mean(Body_length_mean, na.rm =T))
Botanophila_IT = Botanophila %>% pull(IT_mean)
Botanophila_body_length = Botanophila %>% pull(Body_length_mean)

polltraits_mean = polltraits_mean %>% 
  mutate(IT_mean = 
           if_else(Pollinator_genus == "Minettia" & is.na(IT_mean), 
                   Botanophila_IT, IT_mean)) %>% 
  mutate(Body_length_mean = 
           if_else(Pollinator_genus == "Minettia" & is.na(Body_length_mean), 
                   Botanophila_body_length, Body_length_mean))
##Some species with missing values can be recovered
polltraits_mean %>%
  filter(if_any(c(Pollinator_accepted_name, IT_mean, Body_length_mean, Proboscis_length_mean), is.na))
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

#Ratio suggested by chatgpt, I like the idea
d$Trait_ratio = pmin(d$Proboscis_length, d$Floral_tube_length) /
  pmax(d$Proboscis_length, d$Floral_tube_length)
#Calculate an absolute value of distance
d$Trait_difference = abs(d$Proboscis_length - d$Floral_tube_length)

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


