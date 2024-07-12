
library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)

poll_species_specialization_Garden_Week <- 
  readr::read_csv("Data/Working_files/FAMILY_d_prime_by_week_corrected_by_ab.csv") %>%
  filter(Type != "Plant") %>%
  rename(Pollinator = sp_name)

plant_species_visited_by_poll_week <- 
  readr::read_csv("Data/Working_files/data_visited_plant_FAMILY_by_poll_FAMILY_week.csv") 


poll_specialization_data <- poll_species_specialization_Garden_Week %>%
  left_join(plant_species_visited_by_poll_week, by = c("Botanical_garden",
                                                       "Pollinator",
                                                       "Week"))

ggplot(poll_specialization_data, aes(x=dprime,y=100*percentage_vivited_plant_sp, color = Week))+
  geom_point(size=3, alpha=0.25)+
  xlim(0,1)+ylim(0,100)+
  facet_wrap(~Botanical_garden)+
  labs(x="d prime for pollinators per week", y= "Percentage of vivited plant families\nper week (%)", color = NULL)+
  theme_bw()+
  theme(legend.position = "bottom")+
  theme(legend.text = element_text(size = 18),
        axis.text=element_text(size=16),
        axis.title=element_text(size=16,face="bold"),
        plot.title=element_text(size=16,face="bold"),
        strip.text = element_text(size = 18))
