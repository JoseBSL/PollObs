

#Library
library(dplyr)
#Load data
raw_data = readRDS("Data/Working_files/interaction_data.rds")

bee_fam = c("Megachilidae", 
            "Apidae",
            "Colletidae",
            "Andrenidae",
            "Halictidae",
            "Mellittidae")

bee_data = raw_data %>% 
filter(Botanical_garden == "Halle") %>% 
filter(Pollinator_family %in% bee_fam)

#Filter out columns
bee_data1 = bee_data %>% 
select(!c(Floral_abundance,
          Capitulum,
          Flowers_per_capitulum,
          Flowering_neighbours_intensity,
          Time_start,
          Time_finish,
          Total_time_species,
          Plant_rank,
          Plant_status,
          Plant_matchtype,
          Plant_order,
          Pollinator_order,
          Date,
          Plant_family,
          Plant_genus,
          Plant,
          Pollinator))

#Reorganise columns
bee_data1 = bee_data1 %>%
select(Botanical_garden,
       Plant_accepted_name,
       Pollinator_accepted_name,
       Pollinator_family,
       Pollinator_genus,
       Pollinator_rank,
       Interactions,
       Year,
       Month,
       Day,
       Random_census_stop,
       Sampling,
       )

#Count number of individuals
bee_individuals = bee_data1 %>% 
filter(Pollinator_rank=="SPECIES") %>% 
group_by(Pollinator_accepted_name) %>% 
summarise(Individuals = n())

library(readr)
write_csv(bee_data1, "Data/Bee_RawData_Halle.csv")
write_csv(bee_individuals, "Data/Bee_Individuals_Halle.csv")

