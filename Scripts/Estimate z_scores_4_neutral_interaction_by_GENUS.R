
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
                 "_samples.csv"))%>%
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
ggplot(z_sc_data_interactions_genus_final %>% filter(!is.infinite(z_score)), 
       aes(x=z_score, y = Total_interactions, color= Type))+
         geom_point(size=3, alpha= 0.35)+
  scale_x_continuous(trans=scales::pseudo_log_trans(base = 10), breaks=c(-100,-10,-1.96,1.96, 10, 100))+
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  labs(y="Total number of observed interactions by genus", 
       x = "Z-score estimated", color=NULL)+
theme_bw()+
  theme(legend.position = "bottom")+
  theme(legend.text = element_text(size = 17),
        axis.text=element_text(size=17),
        axis.title=element_text(size=16,face="bold"))
       