#Read now the downloaded occurrence data
#https://doi.org/10.15468/dl.saf8be
library(data.table)
library(dplyr)
library(lubridate)
library(ggplot2)
library(stringr)
library(tidygeocoder)
library(pracma)
#Find centroid of the three cities where the botanical gardens are
#and create circunferences of radius that contain occurrence records
#2 Find centroid of 3 cities-----
#Create tibble with the three cities
cities = tibble(Cities = c("Jena", "Halle", "Leipzig"))
#First find approximate coordinates for Jena, Halle and Leipzig
coords = cities %>%  geocode(Cities, method = 'osm', lat = Latitude , long = Longitude)
#Jena
lat1 = coords %>% filter(Cities == "Jena") %>%  pull(Latitude)
lon1 = coords %>% filter(Cities == "Jena") %>%  pull(Longitude)
#Halle
lat2 = coords %>% filter(Cities == "Halle") %>%  pull(Latitude)
lon2 = coords %>% filter(Cities == "Halle") %>%  pull(Longitude)
#Leipzig
lat3 = coords %>% filter(Cities == "Leipzig") %>%  pull(Latitude)
lon3 = coords %>% filter(Cities == "Leipzig") %>%  pull(Longitude)
#Convert latitude and longitude to radians
lat1_rad = deg2rad(lat1)
lon1_rad = deg2rad(lon1)
lat2_rad = deg2rad(lat2)
lon2_rad = deg2rad(lon2)
lat3_rad = deg2rad(lat3)
lon3_rad = deg2rad(lon3)
#Convert spherical coordinates to Cartesian coordinates
x1 = cos(lat1_rad) * cos(lon1_rad)
y1 = cos(lat1_rad) * sin(lon1_rad)
z1 = sin(lat1_rad)
x2 = cos(lat2_rad) * cos(lon2_rad)
y2 = cos(lat2_rad) * sin(lon2_rad)
z2 = sin(lat2_rad)
x3 = cos(lat3_rad) * cos(lon3_rad)
y3 = cos(lat3_rad) * sin(lon3_rad)
z3 = sin(lat3_rad)
#Calculate the centroid in Cartesian coordinates
x_centroid = (x1 + x2 + x3) / 3
y_centroid = (y1 + y2 + y3) / 3
z_centroid = (z1 + z2 + z3) / 3
#Convert the centroid back to spherical coordinates
hyp = sqrt(x_centroid^2 + y_centroid^2)
lat_centroid = atan2(z_centroid, hyp)
lon_centroid = atan2(y_centroid, x_centroid)
#Convert radians back to degrees
lat_centroid_deg = rad2deg(lat_centroid)
lon_centroid_deg = rad2deg(lon_centroid)
#Store it in a tibble
centroid = tibble(Latitude = lat_centroid_deg, Longitude = lon_centroid_deg)
#Create a rectangle with +/- 2 lat and +/- 6 lon
rectangle = c(
    "xmin" = lon_centroid_deg-6.8,
    "xmax" = lon_centroid_deg+6.8,
    "ymin" = lat_centroid_deg-2.5,
    "ymax" = lat_centroid_deg+2.5
  ) %>%
  sf::st_bbox() %>%
  sf::st_as_sfc() %>%
  sf::st_as_sf(crs = 4326) %>%
  sf::st_transform(crs = 4326)

world <- map_data("world")

ggplot() +
geom_map(
  data = world,
  map = world,
  aes(long, lat, map_id = region),
  color = "white",
  fill = "lightgray",
  size = 0.01) +
#geom_point(data = centroid, aes(lon_centroid_deg, lat_centroid_deg), color= "red") +
geom_point(data = coords, aes(Longitude, Latitude, color= Cities)) +
#geom_sf(data = rectangle, colour = "red", fill = NA) +
coord_sf(xlim = c(5, 20),
         ylim = c(45, 60)) 



library(giscoR)
# Get all countries and transform to the same CRS
cntries <- gisco_get_countries(year = 2020,
                               resolution = 20) %>%
           st_transform(4326)

# Plot
ggplot() +
  # First overlay with the whole world
  geom_sf(data = cntries, fill = "grey80", color = "black") +
  geom_point(data = coords, aes(Longitude, Latitude, color= Cities), size = 2) +
  scale_colour_manual(values=c("#FEDC56", "#74C476", "#4682B4")) +
  # Set limits
  xlim(c(10.5, 13.5)) +
  ylim(c(50.5, 52)) +
xlab(NULL) +
ylab(NULL) +
theme(panel.grid.major = element_line(color = gray(0.5), linetype = "dashed", 
size = 0.5), panel.background = element_rect(fill = "aliceblue"),
panel.border = element_rect(colour = "black", fill=NA, size=1.5))


ggplot() +
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
axis.ticks = element_blank())



#Try to prepare complementary informative graphs
#Download plant-pollinator data
library(googlesheets4)
library(dplyr) #To process data
library(rgbif) #To extract taxonomic information
library(stringr) #To process data
library(ggplot2) #For plotting
library(lubridate) #To manipulate dates
library(readr)

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

ggplot(data_dates_2) + 
geom_density(aes(Date, Interactions, group = Sampling), stat = "identity") +
geom_density(data = data_dates_1, aes(Date, Interactions), stat = "identity") +
facet_grid(Botanical_garden ~ .) +
theme(
  strip.background = element_blank(),
  strip.text = element_blank())



