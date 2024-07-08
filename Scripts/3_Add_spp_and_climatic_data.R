#Script to organize interaction dataset
#Add further taxonomic info and climatic data (temperature and humidity)

#Load libraries
library(dplyr) #To manipulate data
library(readr) #To read files
library(rgbif) #To extract taxonomic information
library(stringr) #To manipulate strings
library(lubridate) #To manipulate dates and times

#Read data
interaction_data = read_csv("Data/PollObs_all.csv")
#Load taxonomy
plants = readRDS("Data/Working_files/matched_gbif_plants.rds") 
polls = readRDS("Data/Working_files/matched_gbif_pollinators.rds") 
#Load climatic data
leipzig_weather = readRDS("Data/Working_files/leipzig_weather.rds") %>% 
rename(Botanical_garden = Weather_station)
halle_weather = readRDS("Data/Working_files/halle_weather.rds")%>% 
rename(Botanical_garden = Weather_station)
jena_weather = readRDS("Data/Working_files/jena_weather.rds")%>% 
rename(Botanical_garden = Weather_station)

#Select coloumns of interest from interaction data
colnames(interaction_data)

interaction_data = 
interaction_data %>% 
select(Botanical_garden, Plant, Pollinator,
       Interactions, Floral_abundance, Capitulum,
       Flowers_per_capitulum, Flowering_neighbours_intensity,
       Time_start, Time_finish,
       Total_time_species, Year, Month, Day,
       Random_census_stop, Sampling)
#Add taxonomic info
interaction_data = left_join(interaction_data, plants)
interaction_data = left_join(interaction_data, polls)

#Add climatic data
#First organise data in POSIXct styles (date and time)
date_time = interaction_data %>% 
select(Year, Month, Day, Time_start) %>% 
mutate(Date = make_date(Year, Month, Day)) %>% 
mutate(Date1 = as.POSIXct(paste(Date, Time_start), format="%Y-%m-%d %H:%M")) %>% 
mutate(Date_time = round_date(Date1, "hour")) %>% 
select(Date_time)
interaction_data$Date_time = date_time$Date_time

#Because I have 3 sets of weather conditions (Jena-Halle-Leipzig)
#The easiest is to bind everything together and then merge
weather_conditions = bind_rows(leipzig_weather, halle_weather, jena_weather)

#Add weather data to the dataset
interaction_data = left_join(interaction_data, 
    weather_conditions, by = join_by(Botanical_garden, Date_time))

#Convert date time to date
interaction_data = interaction_data %>% 
mutate(Date = as.Date(Date_time)) 


#Fix one record that is out of phenology
#Anthophora plumipes 2023-07-04 is likey Anthophora quadrimaculata
interaction_data = interaction_data %>% 
mutate(Pollinator = case_when(
  Pollinator == "Anthophora plumipes" & Date == "2023-07-04" ~ "Anthophora quadrimaculata",
  TRUE ~ Pollinator))
interaction_data = interaction_data %>% 
mutate(Pollinator_accepted_name = case_when(
  Pollinator_accepted_name == "Anthophora plumipes" & Date == "2023-07-04" ~ "Anthophora quadrimaculata",
  TRUE ~ Pollinator_accepted_name))


saveRDS(interaction_data, "Data/Working_files/interaction_data.rds")
