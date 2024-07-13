
library(dplyr)
library(tidyr)
library(lubridate)
library(glmmTMB)

# ONLY FOCALS + RD OBSERVATIONS

#Load data
raw_data = readRDS("Data/Working_files/interaction_data.rds")
interaction_data <- raw_data  %>% filter(!is.na(Interactions),
                                         !is.na(Floral_abundance),
                                         Pollinator != "None") %>% 
  select(Botanical_garden, Plant_accepted_name, 
         Pollinator_accepted_name, Date_time, 
         Interactions, Floral_abundance, Total_time_species, Temperature, Humidity, Rainfall) %>% 
  rename(Plant = Plant_accepted_name, Pollinator = Pollinator_accepted_name) %>% 
  mutate(Date = as.Date(Date_time)) %>% 
  mutate(Week = lubridate::week(Date)) %>% 
  select(-Date_time, -Date) %>% filter(!is.na(Pollinator)) %>% ungroup() %>%
  group_by( Botanical_garden,Plant,Pollinator,Week) %>%
  summarise(
    Total_pair_interactions = sum(Interactions),
    Total_floral_abundance = sum(Floral_abundance),
    Total_sampling_time = sum(Total_time_species),
    Mean_Temperature = mean(Temperature),
    Mean_Humidity  = mean(Humidity), 
    Mean_Rainfall  = mean(Rainfall)
  ) %>% ungroup() %>%
  mutate(Pair = paste0(Plant,"_",Pollinator))

poll_abundance_week <- interaction_data %>%
  group_by(Botanical_garden, Pollinator, Week) %>%
  count() %>% 
  rename(Total_pollinator_abundance = n) %>% ungroup()

data_model <- interaction_data %>%
  left_join(poll_abundance_week, 
            by = c("Botanical_garden", "Pollinator", "Week")) %>%
  mutate(Visitation_rate = Total_pair_interactions/Total_sampling_time)


model1 <- glmmTMB::glmmTMB(Total_pair_interactions~scale(Total_floral_abundance)*
                             scale(Total_pollinator_abundance)+
                             scale(Mean_Temperature)+
                             scale(Mean_Humidity)+
                             scale(Mean_Rainfall)+
                             scale(Week) + Botanical_garden + (1|Pair),
                           family=nbinom2,
                           data = data_model)

summary(model1)

performance::check_collinearity(model1)

library(DHARMa)
simulationOutput1 <- DHARMa::simulateResiduals(fittedModel = model1, plot = F)
plot(simulationOutput1)

model2 <- glmmTMB::glmmTMB(Visitation_rate ~scale(Total_floral_abundance)+scale(Total_pollinator_abundance)+
                             scale(Mean_Temperature)+
                             scale(Mean_Humidity)+
                             scale(Mean_Rainfall)+Botanical_garden+
                             scale(Week) + (1|Pair)+ (1|Pair),
                           family=gaussian,
                           data = data_model)

summary(model2)

performance::check_collinearity(model2)

library(DHARMa)
simulationOutput2 <- DHARMa::simulateResiduals(fittedModel = model2, plot = F)
plot(simulationOutput2)
