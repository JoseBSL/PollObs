#Read now the downloaded occurrence data
#https://doi.org/10.15468/dl.saf8be
library(data.table)
library(dplyr)
library(lubridate)
library(ggplot2)
library(stringr)

occurrence = fread("Data/Occurrence_data_gbif.csv", select = c("order", "family", 
                                         "genus", "species",
                                         "taxonRank", "verbatimScientificName",
                                         "decimalLatitude", "decimalLongitude",
                                         "day", "month", "year"), 
                    showProgress = FALSE)
#Check colnames
colnames(occurrence)
#Let's work only with 2023 records (for now)
oc_2023 = occurrence %>% 
filter(year == "2023")
#Create a date and a day of the year column
#and check number of date records per species
oc_2023 = oc_2023 %>% 
mutate(date = make_date(year, month, day)) %>% 
mutate(Doy = yday(date))
#get vector to order dataframe by this order
#Species with higher number of records are the easiest
#Let's start with those
spp_order = oc_2023 %>% 
group_by(species) %>% 
summarise(n_dates = length(date)) %>% 
arrange(-n_dates) %>% 
pull(species)
#Order
oc_2023 = oc_2023 %>%
arrange(match(species, spp_order))
#Final filter by latitude and longitude
#For that let's get a centroid of the three cities that form a triangle and select
#logical latitude and longitude ranges that contain that point in the center
#a rectangle that goes over long latitude range seems the most logical to me
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
    "xmin" = lon_centroid_deg-6.1,
    "xmax" = lon_centroid_deg+6.1,
    "ymin" = lat_centroid_deg-2,
    "ymax" = lat_centroid_deg+2
  ) %>%
  sf::st_bbox() %>%
  sf::st_as_sfc() %>%
  sf::st_as_sf(crs = 4326) %>%
  sf::st_transform(crs = 4326)

world <- map_data("world")
#ggplot() +
#geom_map(
#  data = world,
#  map = world,
#  aes(long, lat, map_id = region),
#  color = "white",
#  fill = "lightgray",
#  size = 0.01) +
#ylim(0, 70) +
#geom_point(data = centroid, aes(lon_centroid_deg, lat_centroid_deg), color= "red") +
#geom_point(data = oc_2023,
#             aes(decimalLongitude, decimalLatitude),
#             alpha = 0.7,
#             size = 0.05) +
#geom_sf(data = rectangle, colour = "red", fill = NA) +
#coord_sf(xlim = c(-15, 44),
#         ylim = c(33, 73),
#         expand = FALSE) 
#
  
#Now extract point within the rectangle
library(sf)
oc_2023 = oc_2023 %>% 
st_as_sf(coords = c("decimalLongitude", "decimalLatitude"), dim = "XY") %>% 
st_set_crs(4326)
#This is the cool way but takes too long
#oc_extraction = sf::st_intersection(rectangle, oc_2023)
colnames(oc_2023)
#Let's apply the filters manually...
#Ok for now :)
oc_2023_1 = oc_2023 %>% 
 dplyr::mutate(longitude = sf::st_coordinates(.)[,1],
                latitude = sf::st_coordinates(.)[,2])
#Drop geometry
oc_2023_1 = sf::st_drop_geometry(oc_2023_1)
#Make upper case all columns
oc_2023_1 = oc_2023_1 %>%
rename_with(str_to_title)
#Check colnames
colnames(oc_2023_1)
#apply manual filter
oc_2023_2 = oc_2023_1 %>% 
filter(Latitude > lat_centroid_deg-2) %>% 
filter(Latitude < lat_centroid_deg+2) %>% 
filter(Longitude > lon_centroid_deg-6.1) %>% 
filter(Longitude < lon_centroid_deg+6.1) 
#Save
saveRDS(oc_2023_2, "Data/Working_files/oc_extraction.rds")

