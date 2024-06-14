#Script to generate pollinator phenology

#Load libraries
library(dplyr) #To manipulate data
#Load interaction data
int_data = readRDS("Data/Working_files/interaction_data.rds")
#Select columns of interest
int_data_polls = int_data %>% 
select(Botanical_garden, 
       Pollinator,
       Interactions,
       Date)
#Filter out "none" in pollinator column
int_data_polls = int_data_polls %>% 
filter(!Pollinator == "None")
#Convert Date to day of the year (Doy)
int_data_polls = int_data_polls %>% 
mutate(Doy = lubridate::yday(Date))

#Things to do:
#Count number of individuals per date
#

