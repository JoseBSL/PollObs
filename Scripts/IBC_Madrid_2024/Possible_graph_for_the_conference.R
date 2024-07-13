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


#Europe----
#Load libraries
library(giscoR) #map of europe
library(ggplot2)
library(dplyr)
library(readr)
library(sf)
library(patchwork)
library(rnaturalearth)

#Load europe map
Europe <- gisco_get_countries(region = "Europe")
europe_map = Europe %>% 
filter(CNTR_ID == "DE")

#Plot Europe map with counties of interest highlighted
p1 = ggplot(Europe) +
geom_sf(fill="gray25",color = "black", alpha = 0.5,size=0.2)  +
geom_sf(data= europe_map,
aes(fill = CNTR_ID, group=CNTR_ID),fill = "#f2ea69", color = "black",alpha=0.8, size = 0.25) +
coord_sf(xlim = c(-10, 30), ylim = c(35, 65))  + 
microplot::theme_collapse(panel.border = element_rect(colour = "black", fill=NA, size=2),
panel.background = element_rect(fill = "white")) +
geom_point(data = coords, aes(Longitude, Latitude, color= Cities), size = 0.15) +
scale_colour_manual(values=c("#440154FF", "#7AD151FF", "#2A788EFF")) +
theme(legend.position = "none")


#Modify coords for plotting
coords1 = tibble(Cities = c("Jena", "Halle", "Leipzig"), 
       Latitude = c(50.9,51.5,51.35),
       Longitude =c(11.4,11.8,12.1))

p2 = ggplot(europe_map) +
geom_sf(fill="gray25",color = "black", alpha = 0.5,size=0.2)  + 
microplot::theme_collapse(panel.border = element_rect(colour = "black", fill=NA, size=2),
panel.background = element_rect(fill = "white")) +
geom_point(data = coords, aes(Longitude, Latitude, color= Cities), size = 2) +
scale_colour_manual(values=c("#440154FF", "#7AD151FF", "#2A788EFF")) +
geom_text(data = coords1, aes(Longitude, Latitude, label = Cities), color= "black", hjust = -0.5) +
theme(legend.position = "none")

p2

#Insert general plot within specific plot
panel1 = p2 + inset_element(p1, left = 0, bottom = -0.89 , right = 0.35, top = 1.23, ignore_tag = TRUE) 
panel1


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

#Order levels
data_dates_2$Botanical_garden = factor(data_dates_2$Botanical_garden, 
                                       levels = c("Halle", "Leipzig", "Jena"))

levels(data_dates_2_a$Botanical_garden)

data_dates_2_a = data_dates_2 %>%  filter(Sampling == "Focal")
data_dates_2_b = data_dates_2 %>%  filter(Sampling == "Random_census")

p2 = ggplot(data_dates_2_a) + 
  geom_line(aes(Date, Interactions, 
                colour = Botanical_garden, 
                linetype = "Dashed"), 
            stat = "identity") +
geom_line(data = data_dates_2_b, aes(Date, Interactions, 
      colour = Botanical_garden, 
      linetype = "Solid"),  stat = "identity") +
scale_colour_manual(
  values=c("#440154FF", "#2A788EFF", "#7AD151FF"),
  guide = "none") +
  scale_linetype_manual(
    values = c("Dashed" = "solid", "Solid" = "dashed"),
    labels = c("Focal obs.", "Random census"),
    name = "Sampling",
    guide = guide_legend(override.aes = list(colour = "black"))) +
facet_grid(Botanical_garden ~ .) +
theme_classic() +
theme(
  strip.background = element_blank(),
  strip.text = element_blank()) +
ylab(NULL) +
scale_y_continuous(name = NULL, sec.axis = sec_axis(~., name = "Interactions")) +
  guides(y = "none") +
theme(legend.position = "bottom", plot.title  = element_text(hjust=0.55, face = "bold")) +
ggtitle("Flowering season")

p2

library(patchwork)
#

panel2 = plot_spacer() / p2 / plot_spacer()+
plot_layout(widths = c(0.1, 0.2, 0.1), heights = c(0.1, 0.2, 0.)) 

panel1 + panel2

