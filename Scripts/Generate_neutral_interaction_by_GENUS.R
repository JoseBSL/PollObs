
library(dplyr)
library(tidyr)
library(lubridate)
library(readr)

#Load data
raw_data = readRDS("Data/Working_files/interaction_data.rds")


#Prepare plant and pollinator data by genus
data_floral_ab_genus <- raw_data  %>% filter(!is.na(Interactions),
                                                     !is.na(Floral_abundance),
                                                     Pollinator != "None") %>% 
  mutate(Individual = paste0(Sampling,Random_census_stop)) %>% 
  dplyr::select(Botanical_garden, Plant_genus, Plant_accepted_name, 
                Date_time, Floral_abundance, Individual) %>% 
  mutate(Plant = Plant_accepted_name, Date = as.Date(Date_time)) %>% 
  mutate(Week = lubridate::week(Date)) %>% 
  dplyr::select(-Date_time, -Date) %>% ungroup() %>% 
  dplyr::select(Botanical_garden,Plant_genus, Plant, Week,
                Floral_abundance, Individual) %>% unique() %>%
  group_by(Botanical_garden, Plant_genus, Week) %>%
  summarise(
    Total_floral_abundance_genus = sum(Floral_abundance),
  ) %>% ungroup() %>% arrange(Week)

data_poll_int_genus <- raw_data  %>% filter(!is.na(Interactions),
                                             !is.na(Floral_abundance),
                                             Pollinator != "None") %>% 
  dplyr::select(Botanical_garden, Pollinator_genus, Pollinator_accepted_name, 
                Date_time, Interactions) %>% 
  mutate(Pollinator = Pollinator_accepted_name, Date = as.Date(Date_time)) %>% 
  mutate(Week = lubridate::week(Date)) %>% 
  dplyr::select(-Date_time, -Date) %>% ungroup() %>% 
  dplyr::select(Botanical_garden,Pollinator_genus, Pollinator, Week,
                Interactions) %>%
  group_by(Botanical_garden, Pollinator_genus, Week) %>%
  summarise(
    Total_interactions = sum(Interactions),
  ) %>% ungroup() %>% arrange(Week) %>%
  filter(!is.na(Pollinator_genus))


#############################################################################
#  GENERATE RANDOM INTERACTIONS
#############################################################################

Total_number_samples_week <- 1000
Botanical_garden_list <- unique(data_floral_ab_genus$Botanical_garden)
results_rand_int <- NULL

for (Botanical_garden_i in Botanical_garden_list) {
  
  vector_weeks4Botanical_garden_i <- data_floral_ab_genus %>%
    filter(Botanical_garden==Botanical_garden_i) %>%
    dplyr::select(Week) %>% unique() %>% pull()
  
  for (Week_j in vector_weeks4Botanical_garden_i) {
    
    set.seed(123)
    
    for (sample_k in c(1:Total_number_samples_week)) {
      
      
      data_floral_ab_genus_Botanical_garden_i_Week_j <- data_floral_ab_genus %>%
        filter(Botanical_garden==Botanical_garden_i, Week == Week_j)
      
      min_number_flowers <- min(data_floral_ab_genus_Botanical_garden_i_Week_j$Total_floral_abundance_genus)
      genus_min_flowers_index <- 
        which(data_floral_ab_genus_Botanical_garden_i_Week_j$Total_floral_abundance_genus == min_number_flowers)
      
      genus_min_flowers <- data_floral_ab_genus_Botanical_garden_i_Week_j$Plant_genus[genus_min_flowers_index]
      
      if(length(genus_min_flowers)>1){
        genus_min_flowers <- genus_min_flowers[1]
      }
      
      total_floral_ab_Botanical_garden_i_Week_j <- 
        sum(data_floral_ab_genus_Botanical_garden_i_Week_j$Total_floral_abundance_genus)
      
      data_floral_ab_genus_Botanical_garden_i_Week_j$genus_prob <- 
        data_floral_ab_genus_Botanical_garden_i_Week_j$Total_floral_abundance_genus / total_floral_ab_Botanical_garden_i_Week_j
      
      plant_genus <- data_floral_ab_genus_Botanical_garden_i_Week_j$Plant_genus
      number_plant_genus <- length(plant_genus)
      
      data_poll_int_genus_Botanical_garden_i_Week_j <- data_poll_int_genus %>%
        filter(Botanical_garden==Botanical_garden_i, Week == Week_j)
      
      total_poll_int_Botanical_garden_i_Week_j <- 
        sum(data_poll_int_genus_Botanical_garden_i_Week_j$Total_interactions)
      
      # Sample plant genus partners
      
      min_number_flowers_rd <- min_number_flowers + 1
      
      while(min_number_flowers_rd > min_number_flowers){
        
        random_int_plant_genus_ijk_index <- sample(c(1:number_plant_genus), 
                                                   size = total_poll_int_Botanical_garden_i_Week_j, 
                                                   replace = TRUE, 
                                                   prob = data_floral_ab_genus_Botanical_garden_i_Week_j$genus_prob)
        
        random_int_plant_genus_ijk <- data_floral_ab_genus_Botanical_garden_i_Week_j$Plant_genus[random_int_plant_genus_ijk_index]
        
        min_number_flowers_rd <- sum(random_int_plant_genus_ijk == genus_min_flowers)
        
      }
      
      # Assign pollinator genus partners
      
      random_int_poll_genus_ijk <- NULL
      for (poll_genus_ijk in 1:nrow(data_poll_int_genus_Botanical_garden_i_Week_j)) {
        random_int_poll_genus_ijk <- c(random_int_poll_genus_ijk,
                                       rep(data_poll_int_genus_Botanical_garden_i_Week_j$Pollinator_genus[poll_genus_ijk],
                                           data_poll_int_genus_Botanical_garden_i_Week_j$Total_interactions[poll_genus_ijk]))
      }
      
      
      
      rand_int_ijk <- tibble(Plant_genus = random_int_plant_genus_ijk,
                             Pollinator_genus = random_int_poll_genus_ijk) %>% 
        group_by(Plant_genus, Pollinator_genus) %>% count() %>%
        rename(Total_interactions = n) %>%
        mutate(Botanical_garden = Botanical_garden_i,
               Week = Week_j,
               Sample = sample_k)
      
      
      results_rand_int <- bind_rows(results_rand_int, rand_int_ijk)
      
    }
  }
}

readr::write_csv(results_rand_int,
                 paste0("Data/Working_files/neutral_weekly_interactions_by_GENUS_",Total_number_samples_week,"_samples.csv"))
