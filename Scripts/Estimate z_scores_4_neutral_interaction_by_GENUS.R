
library(dplyr)
library(tidyr)
library(lubridate)
library(readr)
library(ggplot2)

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

#  1,486 interacciones observadas


data_interactions_genus %>% dplyr::select(Plant_genus, Pollinator_genus) %>%
  unique()

#Load neutral interactions data
Total_number_samples_week <- 1000
data_neutral_interactions_genus <-  
  readr::read_csv(paste0("Data/Working_files/neutral_weekly_interactions_by_GENUS_",
                 Total_number_samples_week,
                 "_samples.csv")) %>%
  group_by(Botanical_garden, Week, Plant_genus, Pollinator_genus) %>%
  summarise(
    Mean_total_interactions = mean(Total_interactions),
    SD_total_interactions = sd(Total_interactions),
  ) %>% ungroup() %>% arrange(Week)

data_neutral_interactions_genus %>% dplyr::select(Plant_genus, Pollinator_genus) %>%
  unique() #7,419 interactions

# Estimate z_scores

z_sc_data_interactions_genus_aux <- data_interactions_genus %>%
  left_join(data_neutral_interactions_genus,
            by = c("Botanical_garden", "Week", 
            "Plant_genus", "Pollinator_genus"))

z_sc_data_interactions_genus_aux[is.na(z_sc_data_interactions_genus_aux)] <- 0

z_sc_data_interactions_genus_final <- z_sc_data_interactions_genus_aux %>%
  mutate(z_score = (Total_interactions-Mean_total_interactions)/SD_total_interactions)

z_sc_data_interactions_genus_final$Type <- "Non-significant"
z_sc_data_interactions_genus_final$Type[z_sc_data_interactions_genus_final$z_score >= 1.96] <- "Over-represented"
z_sc_data_interactions_genus_final$Type[z_sc_data_interactions_genus_final$z_score <= -1.96] <- "Under-represented"


z_sc_data_interactions_genus_final %>% select(Plant_genus, Pollinator_genus, Type) %>%
  unique() %>% group_by(Plant_genus, Pollinator_genus) %>% count() %>%
  filter(n>1)

ggplot(z_sc_data_interactions_genus_final, aes(x=z_score, fill = as.factor(Week)))+
  geom_histogram(position = "stack")+
  scale_fill_viridis_d(option = "viridis", name = "Week") +
  facet_wrap(~Botanical_garden)+
  theme_bw()

z_sc_data_interactions_genus_final_filtered <- z_sc_data_interactions_genus_final %>%
  filter(z_score >= 1.96 | z_score <= -1.96)
  

significant_pairs_week <- z_sc_data_interactions_genus_final_filtered %>% 
  mutate(Pair = paste0(Plant_genus,"_",Pollinator_genus)) %>% 
  group_by(Botanical_garden,Pair,Type) %>% count() %>% 
  rename(Weeks_significant = n) 


ggplot(significant_pairs_week,
       aes(x=Weeks_significant, fill = Type))+
  geom_histogram(position = "stack")+
  facet_wrap(~Botanical_garden)+
  labs(x="Total number of weeks in the study where a pairwise\ninteraction was found to be significant", 
       y = "Number of significant pairwise interactions\nduring the sampling period", fill=NULL)+
  theme_bw()+
  theme(legend.position = "bottom")+
  theme(legend.text = element_text(size = 16),
        axis.text=element_text(size=17),
        axis.title=element_text(size=16,face="bold"),
        plot.title=element_text(size=16,face="bold"),
        strip.text = element_text(size = 18))


significant_pairs_week %>% filter(Weeks_significant>= 4) %>% arrange(desc(Weeks_significant))

all_pairs_week <- z_sc_data_interactions_genus_final %>% 
  mutate(Pair = paste0(Plant_genus,"_",Pollinator_genus)) %>% 
  group_by(Botanical_garden,Pair,Type) %>% count() %>% 
  rename(Total_weeks = n) 
  
ggplot(all_pairs_week,
       aes(x=Total_weeks, fill = Type))+
  geom_bar(position = "fill")+
  facet_wrap(~Botanical_garden)+
  labs(x="Total number of weeks in the study where a pairwise\ninteraction was found", 
       y = "Number of pairwise interactions\nduring the sampling period", fill=NULL)+
  theme_bw()+
  theme(legend.position = "bottom")+
  theme(legend.text = element_text(size = 16),
        axis.text=element_text(size=17),
        axis.title=element_text(size=16,face="bold"),
        plot.title=element_text(size=16,face="bold"),
        strip.text = element_text(size = 18))


library(scales)
library(viridis)
levels(z_sc_data_interactions_genus_final$Type)
z_sc_data_interactions_genus_final$Type <- factor(z_sc_data_interactions_genus_final$Type, 
                                                  levels = c("Under-represented", 
                                                             "Non-significant", 
                                                             "Over-represented"))

ggplot(z_sc_data_interactions_genus_final %>% filter(!is.infinite(z_score)), 
       aes(x=z_score, y = Total_interactions, fill= Type))+
         geom_point(size=3.2, alpha= 0.35, shape=21, color="gray", stroke=0.5)+
  # scale_x_continuous(trans=scales::pseudo_log_trans(base = 10), breaks=c(-100,-10,-1.96,1.96, 10, 100))+
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  labs(y="Total number of observed interactions by genus", 
       x = "Estimated z-score", fill=NULL)+
  scale_fill_viridis_d() +
theme_bw()+
  theme(legend.position = "bottom")+
  theme(legend.text = element_text(size = 17),
        axis.text=element_text(size=17),
        axis.title=element_text(size=16,face="bold"))

z_sc_data_interactions_genus_final %>% filter(Type=="Non-significant") %>%
  arrange(desc(Total_interactions))



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

z_sc_data_interactions_genus_final_fl_ab <- z_sc_data_interactions_genus_final %>%
  left_join(data_floral_ab_genus,
            by = c("Botanical_garden", "Week", 
                   "Plant_genus"))

ggplot(z_sc_data_interactions_genus_final_fl_ab %>% filter(!is.infinite(z_score)), 
       aes(x=z_score, y = Total_interactions, fill= log10(Total_floral_abundance_genus)))+
  geom_point(size=3.2, alpha= 0.35, shape=21, color="gray", stroke=0.5)+
  scale_x_continuous(trans=scales::pseudo_log_trans(base = 10), breaks=c(-100,-10,-1.96,1.96, 10, 100))+
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  labs(y="Total number of observed interactions by genus", 
       x = "Estimated z-score", fill="log10(floral ab.)")+
  scale_fill_viridis() +
  theme_bw()+
  theme(legend.text = element_text(size = 17),
        axis.text=element_text(size=17),
        axis.title=element_text(size=16,face="bold"))


ggplot(z_sc_data_interactions_genus_final_fl_ab %>% filter(!is.infinite(z_score)), 
       aes(x=z_score, y = Total_floral_abundance_genus, fill = log10(Total_interactions)))+
  geom_point(size=3.2, alpha= 0.35, shape=21, color="gray", stroke=0.5)+
  scale_x_continuous(trans=scales::pseudo_log_trans(base = 10), breaks=c(-100,-10,-1.96,1.96, 10, 100))+
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  labs(y="Foral abundance by genus", 
       x = "Estimated z-score", fill="log10(Total interactions)")+
  scale_fill_viridis() +
  theme_bw()+
  theme(legend.text = element_text(size = 17),
        axis.text=element_text(size=17),
        axis.title=element_text(size=16,face="bold"))

ggplot(z_sc_data_interactions_genus_final_fl_ab %>% filter(!is.infinite(z_score)), 
       aes(x=Type, y = Total_interactions))+
  geom_boxplot()+
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  labs(y="Total number of observed interactions by genus", 
       x = NULL, fill=NULL)+
  scale_fill_viridis() +
  theme_bw()+
  theme(legend.position = "bottom")+
  theme(legend.text = element_text(size = 17),
        axis.text=element_text(size=17),
        axis.title=element_text(size=16,face="bold"))


ggplot(z_sc_data_interactions_genus_final_fl_ab %>% filter(!is.infinite(z_score)), 
       aes(x=Type, y = Total_floral_abundance_genus))+
  geom_boxplot()+
  # scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
  #               labels = trans_format("log10", math_format(10^.x))) +
  labs(y="Floral abundance by genus", 
       x = NULL, fill=NULL)+
  scale_fill_viridis() +
  theme_bw()+
  theme(legend.position = "bottom")+
  theme(legend.text = element_text(size = 17),
        axis.text=element_text(size=17),
        axis.title=element_text(size=16,face="bold"))

####################################################################
# MODELING Z-SCORE
####################################################################

library(glmmTMB)

data_model <- z_sc_data_interactions_genus_final_fl_ab %>% filter(!is.infinite(z_score), !is.nan(z_score)) %>% 
  mutate(Pair=paste0(Plant_genus,"_",Pollinator_genus),
         Log_floral_abundance =log(Total_floral_abundance_genus),
         Log_total_interactions =log(Total_interactions))

min_zscore <- min(data_model$z_score)

data_model$log_new_z_score <- log(data_model$z_score+abs(min_zscore)+1)
data_model$new_z_score <- data_model$z_score+abs(min_zscore)+1

model <- glmmTMB(abs(z_score) ~  Botanical_garden + scale(Log_floral_abundance)  +
                            (1|Week/Pair),
                 family = Gamma(link = "log"),
                          data = data_model)
summary(model)
performance::check_collinearity(model)

library(DHARMa)
simulationOutput <- DHARMa::simulateResiduals(fittedModel = model)
plot(simulationOutput)
testDispersion(simulationOutput)
testZeroInflation(simulationOutput)

library(ggeffects)
effects_model <- ggpredict(model, terms = c("Botanical_garden", "Log_floral_abundance"))

plot(effects_model)

mean_Log_floral_abundance <- mean(data_model$Log_floral_abundance)
sd_Log_floral_abundance <- sd(data_model$Log_floral_abundance)


effects_model$group2 = signif(exp(as.numeric(effects_model$group)*sd_Log_floral_abundance+mean_Log_floral_abundance), 1)


ggplot(effects_model, aes(x = x, y = predicted, color = as.factor(group2))) +
  geom_point(size = 2, position=position_dodge(width=0.5)) +
  geom_hline(yintercept = 1.96, linewidth= 1.3, linetype = "dashed")+
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high, color = as.factor(group2),width=.1),
                position=position_dodge(width=0.5)) +
  labs(x = NULL,
       y = "abs(z score)",
       color =  "Flower abundance by genus")+
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position = "bottom")+
  theme(legend.text = element_text(size = 18),
        legend.title=element_text(size=18,face="bold"),
        axis.text=element_text(size=16),
        axis.title=element_text(size=18,face="bold"),
        plot.title=element_text(size=19,face="bold"),
        strip.text = element_text(size = 18))
