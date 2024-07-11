
library(dplyr)
library(tidyr)
library(readr)

poll_species_specialization_Garden_Week <- 
  readr::read_csv("Data/Working_files/species_d_prime_by_week_corrected_by_ab.csv") %>%
  filter(Type != "Plant") %>%
  rename(Pollinator = sp_name)

plant_species_visited_by_poll_week <- 
  readr::read_csv("Data/Working_files/data_visited_plant_Sp_by_poll_week.csv") 


poll_specialization_data <- poll_species_specialization_Garden_Week %>%
  left_join(plant_species_visited_by_poll_week, by = c("Botanical_garden",
                                                       "Pollinator",
                                                       "Week"))
