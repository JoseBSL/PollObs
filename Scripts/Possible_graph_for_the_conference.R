#Read now the downloaded occurrence data
#https://doi.org/10.15468/dl.saf8be
library(data.table)
library(dplyr)
library(lubridate)
library(ggplot2)
library(stringr)
library(tidygeocoder)
library(pracma)
library(giscoR)
library(sf)
library(googlesheets4)
library(readr)

#Create tibble with the three cities
cities = tibble(Cities = c("Jena", "Halle", "Leipzig"))
#First find approximate coordinates for Jena, Halle and Leipzig
coords = cities %>%  geocode(Cities, method = 'osm', lat = Latitude , long = Longitude)

# Get all countries and transform to the same CRS
cntries <- gisco_get_countries(year = 2020,
                               resolution = 20) %>%
           st_transform(4326)

p1 = ggplot() +
  # First overlay with the whole world
  geom_sf(data = cntries, fill = "grey80", color = "black") +
  geom_point(data = coords, aes(Longitude, Latitude, color= Cities), size = 2) +
  scale_colour_manual(values=c("#FEDC56", "#74C476", "#4682B4")) +
  # Set limits
  xlim(c(-6, 32)) +
  ylim(c(38, 62)) +
xlab(NULL) +
ylab(NULL) +
theme(panel.grid.major = element_line(color = gray(0.5), linetype = "dashed", 
size = 0.5), panel.background = element_rect(fill = "aliceblue"),
panel.border = element_rect(colour = "black", fill=NA, size=1.5),
axis.ticks = element_blank(), legend.position = "bottom",
axis.text = element_blank())

#Try to prepare complementary informative graphs
#Load data
data = readRDS("Data/Working_files/interaction_data.rds")
plant_list = read_csv("Data/PollObs_species.csv")
#Generacte vector with species names
spp_list = plant_list %>%  
select(Accepted_name) %>% 
pull()


#Check what is not matching as focal
data %>% 
filter(Sampling == "Focal") %>% 
summarise(n_distinct(Plant))

#Extract focal species that are not matching
spp_to_exclude = data %>% 
filter(Sampling == "Focal") %>% 
filter(!Plant %in% spp_list) %>% 
select(Plant) %>% 
pull()

#Exclude non-matching focals now
data = data %>% 
filter(!Plant %in% spp_to_exclude)
#Filter out species with 0 pollinators
data = data %>% 
filter(!Pollinator == "None")

#Check number of interactions from focal and random
data %>% 
group_by(Botanical_garden, Sampling) %>% 
count()

#Explore interactions across time
#1st create date col
data = data %>%
mutate(Date = make_date(Year, Month, Day))
#Now summarise interactions by dates and bot garden
data_dates_1 = data %>% 
select(Interactions, Botanical_garden, Date) %>% 
group_by(Botanical_garden, Date) %>% 
summarise(Interactions = n())

#Plot interactions
ggplot(data_dates_1) + 
geom_density(aes(Date, Interactions), stat = "identity") +
facet_wrap(vars(Botanical_garden))

#Prepare dataset with sampling method
data_dates_2 = data %>% 
select(Interactions, Botanical_garden, Date, Sampling) %>% 
group_by(Botanical_garden, Date, Sampling) %>% 
summarise(Interactions = n())

library(ggdark)
p2 = ggplot(data_dates_2) + 
geom_line(aes(Date, Interactions, 
          group = Sampling, 
          linetype=Sampling,
          colour = Botanical_garden), 
             stat = "identity") +
scale_colour_manual(
  values=c("#FEDC56", "#74C476", "#4682B4"),
  guide = "none") +
facet_grid(Botanical_garden ~ .) +
dark_theme_gray() +theme(
  strip.background = element_blank(),
  strip.text = element_blank(),
  legend.position = "bottom") 

library(patchwork)
p1 + p2

