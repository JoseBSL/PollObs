


library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)

poll_lecticity <- 
  readr::read_csv("Data/Working_files/Lecticity.csv") %>% 
select(Pollinator, Strict_lecticity) %>% 
rename(Lecticity = Strict_lecticity)

plant_species_visited_by_poll_week <- 
  readr::read_csv("Data/Working_files/data_visited_plant_Sp_by_poll_week.csv") 


data_plot_lecticity <- plant_species_visited_by_poll_week %>% left_join(poll_lecticity, by = "Pollinator")

ggplot(data_plot_lecticity %>% filter(!is.na(Lecticity)),aes(x=Lecticity, 
                                                             y = 100*percentage_vivited_plant_sp,
                                                             fill = Lecticity))+
  geom_boxplot()+
  labs(x= "Lecticity", y = "Percentage vivited plant sp per week (%)", fill = NULL)+
  facet_wrap(~Botanical_garden)+
  theme_bw()+
  theme(legend.text = element_text(size = 18),
          axis.text=element_text(size=16),
          axis.title=element_text(size=16,face="bold"),
          plot.title=element_text(size=16,face="bold"),
          strip.text = element_text(size = 18))+ guides(fill="none")

poll_species_specialization_Garden_Week <- 
  readr::read_csv("Data/Working_files/species_d_prime_by_week_corrected_by_ab.csv") %>%
  filter(Type != "Plant") %>%
  rename(Pollinator = sp_name)


data_plot_dprime_lecticity <- poll_species_specialization_Garden_Week %>% left_join(poll_lecticity, by = "Pollinator")

ggplot(data_plot_dprime_lecticity %>% filter(!is.na(Lecticity)),aes(x=Lecticity, 
                                                             y = dprime,
                                                             fill = Lecticity))+
  geom_boxplot()+
  labs(x= "Lecticity", y = "d prime per week", fill = NULL)+
  facet_wrap(~Botanical_garden)+
  theme_bw()+
  theme(legend.text = element_text(size = 18),
        axis.text=element_text(size=16),
        axis.title=element_text(size=16,face="bold"),
        plot.title=element_text(size=16,face="bold"),
        strip.text = element_text(size = 18))+ guides(fill="none")



