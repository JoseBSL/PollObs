
library(dplyr)
library(tidyr)
library(lubridate)
library(bipartite)

# ONLY FOCALS + RD OBSERVATIONS

#Load data
raw_data = readRDS("Data/Working_files/interaction_data.rds")
interaction_data <- raw_data  %>% filter(!is.na(Interactions),
                                         !is.na(Floral_abundance),
                                         Pollinator != "None") %>% 
  select(Botanical_garden, Plant_accepted_name, 
         Pollinator_accepted_name, Date_time, 
         Interactions, Floral_abundance) %>% 
  rename(Plant = Plant_accepted_name, Pollinator = Pollinator_accepted_name) %>% filter(!is.na(Pollinator))

# Extract interaction and abundance data per sp, garden and week

interaction_data_season_aux <- interaction_data %>%
  group_by(Botanical_garden, Plant, Pollinator) %>%
  summarise(
    Total_pair_interactions = sum(Interactions),
  ) %>% ungroup()

poll_abundance_season <- interaction_data %>%
  group_by(Botanical_garden, Pollinator) %>%
  count() %>% 
  rename(Total_pollinator_abundance = n) %>% ungroup()

data_floral_ab_sampling_time_by_sp <- 
  readr::read_csv("Data/Working_files/data_floral_ab_sampling_time_by_sp.csv") %>%
  group_by(Botanical_garden, Plant) %>%
  summarise(
    Total_floral_abundance = sum(Total_floral_abundance),
  ) %>% ungroup()

interaction_data_season <- interaction_data_season_aux %>%
  left_join(poll_abundance_season, 
            by = c("Botanical_garden", "Pollinator")) %>%
  left_join(data_floral_ab_sampling_time_by_sp, 
            by = c("Botanical_garden", "Plant"))


# Prepare dfs to store the specialization information
average_specialization <- interaction_data %>% 
  dplyr::select(Botanical_garden) %>% unique()

average_specialization$av_dprime_plant <- NA
average_specialization$av_dprime_plant_upper <- NA
average_specialization$av_dprime_plant_lower <- NA
average_specialization$av_dprime_poll <- NA
average_specialization$av_dprime_poll_upper <- NA
average_specialization$av_dprime_poll_lower <- NA
average_specialization$H2prime <- NA

species_specialization_Garden <- NULL


###############################
# GET THE INFO
###############################

Botanical_garden_list <- unique(interaction_data$Botanical_garden)

for (Botanical_garden_i in Botanical_garden_list) {
  
  interaction_df_Garden_i <- interaction_data_season %>% ungroup() %>%
    filter(Botanical_garden == Botanical_garden_i) %>%
    dplyr::select(Plant, Pollinator,Total_pair_interactions) %>% ungroup()
  
  Total_number_interactions <- sum(interaction_df_Garden_i$Total_pair_interactions)
  
  total_number_interactions_plant <- interaction_df_Garden_i %>%
    group_by(Plant) %>% count(wt = Total_pair_interactions) %>%
    rename(Total_interactions_plant = n) %>%
    mutate(Weight_qi = Total_interactions_plant/Total_number_interactions) %>% 
    ungroup()
  
  total_number_interactions_poll <- interaction_df_Garden_i %>%
    group_by(Pollinator) %>% count(wt = Total_pair_interactions) %>%
    rename(Total_interactions_poll = n) %>%
    mutate(weight_qj = Total_interactions_poll/Total_number_interactions) %>% 
    ungroup()
  
  ############################################
  # create a bipartite interaction matrix
  ############################################
  
  interaction_matrix_Garden_i_aux <- interaction_df_Garden_i %>%
    tidyr::pivot_wider(names_from = Pollinator, 
                       values_from = Total_pair_interactions, 
                       values_fill = list(Total_pair_interactions = 0))
  
  interaction_matrix_Garden_i <- interaction_matrix_Garden_i_aux %>%
    dplyr::select(-Plant) %>% as.matrix()
  
  rownames(interaction_matrix_Garden_i) <- 
    interaction_matrix_Garden_i_aux$Plant
  
  ############################################
  # d prime estimation
  ############################################
  
  # Results for plant species
  
  all_results_specialization_plant_Garden_i <- 
    dfun(interaction_matrix_Garden_i)# plant_abundance_Garden_i$Plant_total_floral_abundance)
  
  specialization_plant_Garden_i <- 
    all_results_specialization_plant_Garden_i$dprime %>% as.data.frame() %>%
    mutate(Botanical_garden = Botanical_garden_i)
  colnames(specialization_plant_Garden_i) <- c("dprime_plant","Botanical_garden")
  
  specialization_plant_Garden_i$dprime[is.nan(specialization_plant_Garden_i$dprime)] <- 0
  specialization_plant_Garden_i$dprime_plant[is.nan(specialization_plant_Garden_i$dprime_plant)] <- 0
  
  specialization_plant_Garden_i$Plant <- 
    rownames(specialization_plant_Garden_i)
  specialization_plant_Garden_i$Type <- "Plant"
  
  sd_specialization_plant_Garden_i <- specialization_plant_Garden_i %>% 
    left_join(total_number_interactions_plant, by = "Plant") %>%
    mutate(weighted_dprime = Weight_qi * dprime_plant)
  
  # Results for pollinator species
  
  all_results_specialization_poll_Garden_i <- 
    dfun(t(interaction_matrix_Garden_i))
  specialization_poll_Garden_i <- 
    all_results_specialization_poll_Garden_i$dprime %>% as.data.frame() %>%
    mutate(Botanical_garden = Botanical_garden_i)
  colnames(specialization_poll_Garden_i) <- c("dprime_poll",
                                                     "Botanical_garden")
  specialization_poll_Garden_i$dprime[is.nan(specialization_poll_Garden_i$dprime)] <- 0
  specialization_poll_Garden_i$dprime_poll[is.nan(specialization_poll_Garden_i$dprime_poll)] <- 0
  
  specialization_poll_Garden_i$Pollinator <- 
    rownames(specialization_poll_Garden_i)
  specialization_poll_Garden_i$Type <- "Pollinator"
  
  sd_specialization_poll_Garden_i <- specialization_poll_Garden_i %>% 
    left_join(total_number_interactions_poll, by = "Pollinator") %>%
    mutate(weighted_dprime = weight_qj * dprime_poll)
  
  
  ############################################
  # H2 prime estimation
  ############################################
  
  network_specialization_plant_Garden_i <- 
    bipartite::H2fun(interaction_matrix_Garden_i, H2_integer=TRUE)
  
  ############################################
  # Save average results
  ############################################
  
  garden_week_row <- which(average_specialization$Botanical_garden==Botanical_garden_i)
  
  average_specialization$av_dprime_plant[garden_week_row] <- 
    sum(sd_specialization_plant_Garden_i$weighted_dprime)
  average_specialization$av_dprime_poll[garden_week_row] <- 
    sum(sd_specialization_poll_Garden_i$weighted_dprime)
  average_specialization$H2prime[garden_week_row] <- 
    network_specialization_plant_Garden_i[1]
  
  average_specialization$av_dprime_plant_upper[garden_week_row] <- 
    quantile(sd_specialization_plant_Garden_i$dprime_plant, 0.975)
  average_specialization$av_dprime_plant_lower[garden_week_row] <- 
    quantile(sd_specialization_plant_Garden_i$dprime_plant, 0.025)
  average_specialization$av_dprime_poll_upper[garden_week_row] <- 
    quantile(sd_specialization_poll_Garden_i$dprime_poll, 0.975)
  average_specialization$av_dprime_poll_lower[garden_week_row] <- 
    quantile(sd_specialization_poll_Garden_i$dprime_poll, 0.025)
  
  ############################################
  # Save species results
  ############################################
  
  rownames(specialization_plant_Garden_i) <- 1:nrow(specialization_plant_Garden_i)
  rownames(specialization_poll_Garden_i) <- 1:nrow(specialization_poll_Garden_i)
  species_specialization_Garden <- 
    bind_rows(species_specialization_Garden,
              specialization_plant_Garden_i %>% 
                dplyr::select(-dprime_plant) %>% rename(sp_name = Plant),
              specialization_poll_Garden_i %>% 
                dplyr::select(-dprime_poll) %>% rename(sp_name = Pollinator))
  
}

readr::write_csv(average_specialization,"Data/Working_files/average_d_prime_by_SEASON_corrected_by_int.csv")
readr::write_csv(species_specialization_Garden,"Data/Working_files/species_d_prime_by_SEASON_corrected_by_int.csv")
