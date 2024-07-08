
library(dplyr)
library(tidyr)
library(lubridate)
library(bipartite)

#Load data
raw_data = readRDS("Data/Working_files/interaction_data.rds")
interaction_data <- raw_data %>% 
  select(Botanical_garden, Plant, Pollinator, Date_time, Interactions) %>% 
  mutate(Date = as.Date(Date_time)) %>% 
  mutate(Week = lubridate::week(Date)) %>% 
  select(!Date)

#Select interaction data
interaction_data_week <- interaction_data %>% 
  group_by(Botanical_garden, Plant, Pollinator, Week) %>%
  count(wt = Interactions)%>% 
  rename(Total_pair_interactions = n) %>%
  filter(Total_pair_interactions>0) %>% ungroup()

# Prepare dfs to store the specialization information
average_specialization <- interaction_data %>% 
  dplyr::select(Botanical_garden, Week) %>% unique()

average_specialization$av_dprime_plant <- NA
average_specialization$av_dprime_poll <- NA
average_specialization$H2prime <- NA

species_specialization_Garden_Week <- NULL


###############################
# GET THE INFO
###############################

Botanical_garden_list <- unique(interaction_data$Botanical_garden)

for (Botanical_garden_i in Botanical_garden_list) {
  
  Botanical_garden_week_list <- interaction_data_week %>%
    filter(Botanical_garden == Botanical_garden_i) %>%
    dplyr::select(Week) %>% unique() %>% pull()
  
  for (Week_j in Botanical_garden_week_list) {
    
    ############################################
    # weights estimation for d prime estimation
    ############################################
    
    interaction_df_Garden_i_Week_j <- interaction_data_week %>% ungroup() %>%
      filter(Botanical_garden == Botanical_garden_i, Week == Week_j) %>%
      dplyr::select(Plant, Pollinator,Total_pair_interactions) %>% ungroup()
    
    Total_number_interactions <- sum(interaction_df_Garden_i_Week_j$Total_pair_interactions)
    
    total_number_interactions_plant <- interaction_df_Garden_i_Week_j %>%
      group_by(Plant) %>% count(wt = Total_pair_interactions) %>%
      rename(Total_interactions_plant = n) %>%
      mutate(Weight_qi = Total_interactions_plant/Total_number_interactions) %>% 
      ungroup()
    
    total_number_interactions_poll <- interaction_df_Garden_i_Week_j %>%
      group_by(Pollinator) %>% count(wt = Total_pair_interactions) %>%
      rename(Total_interactions_poll = n) %>%
      mutate(weight_qj = Total_interactions_poll/total_number_interactions) %>% 
      ungroup()
    
    
    ############################################
    # create a bipartite interaction matrix
    ############################################
    
    interaction_matrix_Garden_i_Week_j_aux <- interaction_df_Garden_i_Week_j %>%
      tidyr::pivot_wider(names_from = Pollinator, 
                         values_from = Total_pair_interactions, 
                         values_fill = list(Total_pair_interactions = 0))
    
    interaction_matrix_Garden_i_Week_j <- interaction_matrix_Garden_i_Week_j_aux %>%
      dplyr::select(-Plant) %>% as.matrix()
    
    rownames(interaction_matrix_Garden_i_Week_j) <- 
      interaction_matrix_Garden_i_Week_j_aux$Plant
    
    ############################################
    # d prime estimation
    ############################################
    
    all_results_specialization_plant_Garden_i_Week_j <- 
      dfun(interaction_matrix_Garden_i_Week_j)
    
    specialization_plant_Garden_i_Week_j <- 
      all_results_specialization_plant_Garden_i_Week_j$dprime %>% as.data.frame() %>%
      mutate(Botanical_garden = Botanical_garden_i, Week = Week_j)
    colnames(specialization_plant_Garden_i_Week_j) <- c("dprime_plant",
                                                        "Botanical_garden", "Week")
    specialization_plant_Garden_i_Week_j$Plant <- 
      rownames(specialization_plant_Garden_i_Week_j)
    specialization_plant_Garden_i_Week_j$Type <- "Plant"
    
    sd_specialization_plant_Garden_i_Week_j <- specialization_plant_Garden_i_Week_j %>% 
      left_join(total_number_interactions_plant, by = "Plant") %>%
      mutate(weighted_dprime = Weight_qi * dprime_plant)
    
    all_results_specialization_poll_Garden_i_Week_j <- 
      dfun(t(interaction_matrix_Garden_i_Week_j)) 
    specialization_poll_Garden_i_Week_j <- 
      all_results_specialization_poll_Garden_i_Week_j$dprime %>% as.data.frame() %>%
      mutate(Botanical_garden = Botanical_garden_i, Week = Week_j)
    colnames(specialization_poll_Garden_i_Week_j) <- c("dprime_poll",
                                                       "Botanical_garden", "Week")
    specialization_poll_Garden_i_Week_j$Pollinator <- 
      rownames(specialization_poll_Garden_i_Week_j)
    specialization_poll_Garden_i_Week_j$Type <- "Pollinator"
    
    sd_specialization_poll_Garden_i_Week_j <- specialization_poll_Garden_i_Week_j %>% 
      left_join(total_number_interactions_poll, by = "Pollinator") %>%
      mutate(weighted_dprime = weight_qj * dprime_poll)
    
    garden_week_row <- which(average_specialization$Botanical_garden==Botanical_garden_i &
                               average_specialization$Week==Week_j)
    
    ############################################
    # H2 prime estimation
    ############################################
    
    network_specialization_plant_Garden_i_Week_j <- 
      bipartite::H2fun(interaction_matrix_Garden_i_Week_j, H2_integer=TRUE)
    
    ############################################
    # Save average results
    ############################################
    
    average_specialization$av_dprime_plant[garden_week_row] <- 
      sum(sd_specialization_plant_Garden_i_Week_j$weighted_dprime)
    average_specialization$av_dprime_poll[garden_week_row] <- 
      sum(sd_specialization_poll_Garden_i_Week_j$weighted_dprime)
    average_specialization$H2prime[garden_week_row] <- 
      network_specialization_plant_Garden_i_Week_j[1]
    
    ############################################
    # Save species results
    ############################################
    
    species_specialization_Garden_Week <- 
      bind_rows(species_specialization_Garden_Week,
                specialization_plant_Garden_i_Week_j,
                specialization_poll_Garden_i_Week_j)
    
  }
  
}


library(ggplot2)

av_dprime_plant_plot <- ggplot(average_specialization, aes(x=Week,y=av_dprime_plant, color = Botanical_garden))+
  geom_point(size=3)+
  geom_line(linewidth=1.5)+
  ylim(0,1)+
  labs(x="Week number", y= "Average species level\nspecialization index for plants", color = NULL)+
  theme_bw()+
  theme(legend.position = "bottom", legend.title = element_blank())+
  theme(legend.text = element_text(size = 18),
        axis.text=element_text(size=16),
        axis.title=element_text(size=18,face="bold"),
        plot.title=element_text(size=19,face="bold"),
        strip.text = element_text(size = 18))

av_dprime_poll_plot <- ggplot(average_specialization, aes(x=Week,y=av_dprime_poll, color = Botanical_garden))+
  geom_point(size=3)+
  geom_line(linewidth=1.5)+
  ylim(0,1)+
  labs(x="Week number", y= "Average species level\nspecialization index for pollinators", color = NULL)+
  theme_bw()+
  theme(legend.position = "bottom", legend.title = element_blank())+
  theme(legend.text = element_text(size = 18),
        axis.text=element_text(size=16),
        axis.title=element_text(size=18,face="bold"),
        plot.title=element_text(size=19,face="bold"),
        strip.text = element_text(size = 18))

av_dprime_plant_poll_plot <- ggplot(average_specialization, aes(x=Week,y=av_dprime_plant-av_dprime_poll, color = Botanical_garden))+
  geom_point(size=3)+
  geom_line(linewidth=1.5)+
  ylim(0,1)+
  labs(x="Week number", y= "Difference between the species level\nspecialization index of plants and pollinators", color = NULL)+
  theme_bw()+
  theme(legend.position = "bottom", legend.title = element_blank())+
  theme(legend.text = element_text(size = 18),
        axis.text=element_text(size=16),
        axis.title=element_text(size=18,face="bold"),
        plot.title=element_text(size=19,face="bold"),
        strip.text = element_text(size = 18))


av_H2prime_poll_plot <- ggplot(average_specialization, aes(x=Week,y=H2prime, color = Botanical_garden))+
  geom_point(size=3)+
  geom_line(linewidth=1.5)+
  ylim(0,1)+
  labs(x="Week number", y= "Network-level specialization index", color = NULL)+
  theme_bw()+
  theme(legend.position = "bottom", legend.title = element_blank())+
  theme(legend.text = element_text(size = 18),
        axis.text=element_text(size=16),
        axis.title=element_text(size=18,face="bold"),
        plot.title=element_text(size=19,face="bold"),
        strip.text = element_text(size = 18))

library(patchwork)
(av_dprime_plant_plot|av_dprime_poll_plot|av_H2prime_poll_plot) + plot_layout(guides = "collect") & theme(legend.position = 'bottom')
