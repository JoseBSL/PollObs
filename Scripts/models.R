
library(dplyr)
library(tidyr)
library(lubridate)
library(glmmTMB)
library(DHARMa)
library(ggeffects)
library(ggplot2)

# ONLY FOCALS + RD OBSERVATIONS

#Load data
raw_data = readRDS("Data/Working_files/interaction_data.rds")

# Only one day per week
raw_data %>% filter(!is.na(Interactions),
                    !is.na(Floral_abundance),
                    Pollinator != "None") %>% 
  select(Botanical_garden, Plant_accepted_name, 
         Pollinator_accepted_name, Date_time, 
         Interactions, Floral_abundance, Total_time_species, Temperature, Humidity, Rainfall) %>% 
  rename(Plant = Plant_accepted_name, Pollinator = Pollinator_accepted_name) %>% 
  mutate(Date = as.Date(Date_time)) %>% 
  mutate(Week = lubridate::week(Date)) %>% 
  select(Botanical_garden, Week, Date) %>% unique() %>%
  group_by(Botanical_garden, Week) %>%
  count() %>% filter(n>1)

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
    Mean_Temperature = mean(Temperature),
    Mean_Humidity  = mean(Humidity), 
    Mean_Rainfall  = mean(Rainfall)
  ) %>% ungroup() %>%
  mutate(Pair = paste0(Plant,"_",Pollinator))

poll_abundance_week <- interaction_data %>%
  group_by(Botanical_garden, Pollinator, Week) %>%
  count() %>% 
  rename(Total_pollinator_abundance = n) %>% ungroup()

data_floral_ab_sampling_time_by_sp <- readr::read_csv("Data/Working_files/data_floral_ab_sampling_time_by_sp.csv")

data_model <- interaction_data %>%
  left_join(poll_abundance_week, 
            by = c("Botanical_garden", "Pollinator", "Week")) %>%
  left_join(data_floral_ab_sampling_time_by_sp, 
            by = c("Botanical_garden", "Plant", "Week"))  %>%
  mutate(Visitation_rate = Total_pair_interactions/Total_sampling_time,
         log_Total_floral_abundance = log(Total_floral_abundance))


ggplot(data_model, aes(y = (Total_pair_interactions), x = Week))+
  geom_point()

# Apparent outlayers
# Jose: eran números desproporcionados de Bombus, los datos son reales o cercanos a la realidad
# Jose: calculé las visitas por bombus en 30 segundos y extrapolé por unidad de tiempo

ggplot(data_model, aes(y = log(Total_pair_interactions), x = Week))+
  geom_point()

data_model %>% filter(Total_floral_abundance>1e5) %>% arrange(desc(Total_floral_abundance)) %>%
  select(Botanical_garden,Plant,Pollinator,Week,Total_pair_interactions,Total_floral_abundance)


# data_model_fil <- data_model %>% filter(Total_pair_interactions<2e4)
# 
# model1 <- glmmTMB::glmmTMB(Total_pair_interactions ~ scale(Total_floral_abundance)*
#                              scale(Total_pollinator_abundance)+
#                              offset(scale(Total_sampling_time))+
#                              #scale(Total_sampling_time):scale(Total_pollinator_abundance)+
#                              scale(Mean_Temperature)+
#                              #scale(Mean_Humidity)+
#                              #scale(Mean_Rainfall)+
#                              Botanical_garden + 
#                              (1|Week/Pair),
#                            disp=~scale(Total_floral_abundance)*
#                              scale(Total_pollinator_abundance),
#                            zi=~1,
#                            family=nbinom2,
#                            data = data_model)
# 
# summary(model1)
# 
# performance::check_collinearity(model1)
# 
# simulationOutput1 <- DHARMa::simulateResiduals(fittedModel = model1, plot = F)
# plot(simulationOutput1)
# testDispersion(simulationOutput1)
# testZeroInflation(simulationOutput1)
# testQuantiles(simulationOutput1)
# #testCategorical(simulationOutput1, catPred = data_model_fil$Pair)
# plotResiduals(simulationOutput1, data_model$Total_floral_abundance)
# plotResiduals(simulationOutput1, data_model$Total_pollinator_abundance)
# plotResiduals(simulationOutput1, data_model$Mean_Temperature)
# 
# 
# model11 <- glmmTMB::glmmTMB(log(Total_pair_interactions) ~ scale(Total_floral_abundance)*
#                              scale(Total_pollinator_abundance)+
#                              offset(scale(Total_sampling_time))+
#                              #scale(Total_sampling_time):scale(Total_pollinator_abundance)+
#                              scale(Mean_Temperature)+
#                              #scale(Mean_Humidity)+
#                              #scale(Mean_Rainfall)+
#                              Botanical_garden + 
#                              (1|Week/Pair),
#                            family=Gamma(link = "log"),
#                            data = data_model %>% filter(Total_pair_interactions>1))
# 
# summary(model11)
# 
# performance::check_collinearity(model11)
# 
# simulationOutput11 <- DHARMa::simulateResiduals(fittedModel = model11, plot = F)
# plot(simulationOutput11)
# testDispersion(simulationOutput11)
# testZeroInflation(simulationOutput11)
# testQuantiles(simulationOutput11)
# #testCategorical(simulationOutput1, catPred = data_model_fil$Pair)
# plotResiduals(simulationOutput11, data_model$Total_floral_abundance)
# plotResiduals(simulationOutput11, data_model$Total_pollinator_abundance)
# plotResiduals(simulationOutput11, data_model$Mean_Temperature)



model2 <- glmmTMB::glmmTMB(Visitation_rate ~ scale(log_Total_floral_abundance)*scale(Total_pollinator_abundance)+
                             scale(Mean_Temperature)+
                             #scale(Mean_Humidity)+
                             #scale(Mean_Rainfall)+
                             Botanical_garden+
                             #scale(Week) + 
                             (1|Week/Pair),
                           family=Gamma(link = "log"),
                           data = data_model)

summary(model2)

performance::check_collinearity(model2)

simulationOutput2 <- DHARMa::simulateResiduals(fittedModel = model2)
plot(simulationOutput2)
testDispersion(simulationOutput2)
testZeroInflation(simulationOutput2)
plotResiduals(simulationOutput2, data_model$Total_floral_abundance)
plotResiduals(simulationOutput2, data_model$Total_pollinator_abundance)
plotResiduals(simulationOutput2, data_model$Mean_Temperature)


scale( data_model$Total_floral_abundance)

# Obtener los efectos marginales
effects_model2 <- ggpredict(model2, terms = c("Total_pollinator_abundance", "log_Total_floral_abundance", "Mean_Temperature", "Botanical_garden"))
plot(effects_model2)

mean_fl_ab <- mean(data_model$log_Total_floral_abundance)
sd_fl_ab <- sd(data_model$log_Total_floral_abundance)

effects_model2 <- effects_model2 %>% mutate(BothLabels = paste0(panel,": ",facet, " ºC" ),
                                            group2 = round(as.numeric(group)*sd_fl_ab+mean_fl_ab,0))


ggplot(effects_model2, aes(x = x, y = predicted, color = as.factor(group2))) +
  geom_line(size = 1.3) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = as.factor(group2)), alpha = 0.2, color =NA) +
  labs(x = "Total pollinator abundance by species (counts)",
       y = "Predicted visitation rate (counts per minute)",
       color =  "Log. of the total number of flowers by species (counts)",
       fill = "Log. of the total number of flowers by species (counts)") +
  facet_wrap(~BothLabels)+
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position = "bottom")+
  theme(legend.text = element_text(size = 18),
        legend.title=element_text(size=18,face="bold"),
        axis.text=element_text(size=16),
        axis.title=element_text(size=18,face="bold"),
        plot.title=element_text(size=19,face="bold"),
        strip.text = element_text(size = 18))

