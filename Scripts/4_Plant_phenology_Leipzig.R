#Script to prepare plant phenology (data from froPhenObs app)
#Leipzig only
#Load libraries
library(dplyr)
library(readr)
library(stringr)
library(ggplot2)
#To predict some missing phenologies
library(mgcv)
library(tidygam)
#Load interaction data
int_data = readRDS("Data/Working_files/interaction_data.rds")
#Load phenology data
leipzig_phenobs = read_csv("Data/Phenology_data/raw_plant_phenobs_leipzig.csv")
#Set ggplot theme
theme_set(theme_light())

#Get focal species recorded in Leipzig
leipzig_focals = int_data %>% 
filter(Botanical_garden == "Leipzig") %>% 
filter(Sampling == "Focal") %>% 
distinct(Plant) %>% 
select(Plant) %>% 
rename(Species = Plant) 

#Rename cols with underscore
colnames(leipzig_phenobs) = str_replace(colnames(leipzig_phenobs), " ", "_")
#Prepare data with right format of dates
flowering_data = leipzig_phenobs %>% 
select(Date, Doy, Species, Flowers_opening, Flowering_intensity) %>% 
mutate(Date = as.Date(str_replace_all(Date, "[.]", "/"), "%d/%m/%Y")) %>% 
mutate(Species = recode_factor(Species, "Scopolia carniolia" = "Scopolia carniolica")) %>% 
mutate(Species = recode_factor(Species, "Anemone pulsatilla" = "Pulsatilla vulgaris")) %>% 
mutate(Species = recode_factor(Species, "Anemone nemorosa" = "Anemonoides nemorosa")) %>% 
mutate(Species = recode_factor(Species, "Aquilegia chrysantha" = "Aquilegia vulgaris")) %>% 
mutate(Species = recode_factor(Species, "Asarum canadense" = "Asarum caudatum")) %>% 
mutate(Species = recode_factor(Species, "Anemone sylvestris" = "Anemonoides sylvestris")) %>% 
mutate(Species = recode_factor(Species, "Menyanthes trifolata" = "Menyanthes trifoliata")) %>% 
mutate(Species = recode_factor(Species, "Psephellus dealbata" = "Psephellus dealbatus")) %>% 
mutate(Species = recode_factor(Species, "Vincetoxicum hinrundinaria" = "Vincetoxicum hirundinaria")) %>% 
mutate(Species = recode_factor(Species, "Hemerocallis dumotieri" = "Hemerocallis dumortieri"))

#Fix some species names first and homogenize with our dataset
d = left_join(leipzig_focals, flowering_data)
d %>% 
filter(is.na(Date))

#Missing phenologies 
# 1. Helleborus foetidus  
# 2. Primula veris 
# 3. Lamium album      
# 4. Silene viscaria
# 5. Centranthus ruber
# 6. Platycodon grandiflorus

#1. Helleborus foetidus ----
#Plot available data of flower number
int_data %>% 
filter(Botanical_garden == "Leipzig") %>% 
filter(Plant == "Helleborus foetidus") %>%
select(Date, Floral_abundance) %>% 
mutate(Doy = lubridate::yday(Date)) %>% 
ggplot(aes(Date, Floral_abundance)) +
geom_point()+
ggalt::geom_xspline(color = "black")
#I remember it was flowering peak when I observed it
#Let's assume it has a symmetric flowering

hf = int_data %>% 
filter(Botanical_garden == "Leipzig") %>% 
filter(Plant == "Helleborus foetidus") %>%
select(Date, Floral_abundance) %>% 
mutate(Doy = lubridate::yday(Date)) %>% 
mutate(Doy_difference = abs(sort(Doy)[2] - Doy))




#Prepare data
s_viscaria_jena = int_data %>% 
filter(Botanical_garden == "Jena") %>% 
filter(Plant == "Silene viscaria") %>%
distinct(Floral_abundance, Date) %>% 
mutate(Floral_abundance1 = Floral_abundance/max(Floral_abundance)) %>% 
mutate(Doy = lubridate::yday(Date))
#Run a gam model in order to predict missing phenologies
#Fit a regression model
s_viscaria_gam = mgcv::gam(Floral_abundance ~ s(Doy, k=3),
                   #bs="fs",
                   gamma = 3,
                   poisson,
                   data = s_viscaria_jena)
#Plot predicted values
predict_gam(s_viscaria_gam, tran_fun = exp) %>%
plot("Doy")
#Check if it works. Round values so very small values are considered as zero
s_viscaria_new = tibble(Doy = c(unique(jena_phenobs$Doy)))
s_viscaria_new$Flowering_intensity = round(predict(s_viscaria_gam, s_viscaria_new, type = "response"))
#Set values under 1 as 0, convert to percentage and add needed columns
s_viscaria_new = s_viscaria_new %>% 
mutate(Flowering_intensity = Flowering_intensity/ max(Flowering_intensity)*100) %>% 
mutate(Flowering_intensity = if_else(Flowering_intensity<1, 0, Flowering_intensity)) %>% 
mutate(Flowers_opening = if_else(Flowering_intensity==0, "no", "y")) %>% 
mutate(Garden = "Jena") %>% 
mutate(Species = "Silene viscaria") %>% 
filter(Flowers_opening == "y")

#To add 0' with dates without flowering 
#let's recover the sampled dates and do left join with them
values_doy = tibble(Doy = unique(flowering_data$Doy))
s_viscaria_new = left_join(values_doy, s_viscaria_new) 
s_viscaria_new = s_viscaria_new %>% 
mutate(Flowering_intensity = 
      if_else(is.na(Flowering_intensity), 0,Flowering_intensity)) %>% 
mutate(Flowers_opening = 
      if_else(is.na(Flowers_opening), "no", Flowers_opening)) %>% 
mutate(Garden = "Jena") %>% 
mutate(Species = "Silene viscaria")

#Add S. viscaria phenology
flowering_data = bind_rows(flowering_data, s_viscaria_new)



