
library(dplyr)
library(tidyr)
library(lubridate)
library(readr)

#Load data
raw_data = readRDS("Data/Working_files/interaction_data.rds")

# Prepare observed interaction data by genus
data_interactions_genus <- raw_data  %>% filter(!is.na(Interactions),
                                             !is.na(Floral_abundance),
                                             Pollinator != "None",
                                             !is.na(Pollinator_genus)) %>% 
  dplyr::select(Botanical_garden, Plant_genus, Pollinator_genus,
                Date_time, Interactions) %>% 
  mutate(Date = as.Date(Date_time)) %>% 
  mutate(Week = lubridate::week(Date)) %>% 
  dplyr::select(-Date_time, -Date) %>% ungroup() %>% 
  dplyr::select(Botanical_garden,Plant_genus, Pollinator_genus, Week,
                Interactions) %>%
  group_by(Botanical_garden, Week, Plant_genus, Pollinator_genus) %>%
  summarise(
    Total_interactions = sum(Interactions),
  ) %>% ungroup() %>% arrange(Week)

#Load neutral interactions data
Total_number_samples_week <- 1000
data_neutral_interactions_genus <-  
  readr::read_csv(paste0("Data/Working_files/neutral_weekly_interactions_by_GENUS_",
                 Total_number_samples_week,
                 "_samples.csv"))%>%
  group_by(Botanical_garden, Week, Plant_genus, Pollinator_genus) %>%
  summarise(
    Mean_total_interactions = mean(Total_interactions),
    SD_total_interactions = sd(Total_interactions),
  ) %>% ungroup() %>% arrange(Week)


# Estimate z_scores

z_sc_data_interactions_genus_aux <- data_interactions_genus %>%
  left_join(data_neutral_interactions_genus,
            by = c("Botanical_garden", "Week", 
            "Plant_genus", "Pollinator_genus"))

z_sc_data_interactions_genus_aux[is.na(z_sc_data_interactions_genus_aux)] <- 0

z_sc_data_interactions_genus_final <- z_sc_data_interactions_genus_aux %>%
  mutate(z_score = (Total_interactions-Mean_total_interactions)/SD_total_interactions)

z_sc_data_interactions_genus_final_filtered <- z_sc_data_interactions_genus_final %>%
  filter(z_score >= 1.96 | z_score <= -1.96)
  