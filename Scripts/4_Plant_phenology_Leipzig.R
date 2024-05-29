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
jena_phenobs = read_csv("Data/Phenology_data/raw_plant_phenobs_jena.csv")

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
# 1. Helleborus foetidus  (simulate half of phenology based on my observations)
# 2. Primula veris (get phenology from Jena)
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
hf_a = int_data %>% 
filter(Botanical_garden == "Leipzig") %>% 
filter(Plant == "Helleborus foetidus") %>%
select(Date, Floral_abundance) %>% 
mutate(Doy = lubridate::yday(Date)) %>% 
distinct()

hf_b = int_data %>% 
filter(Botanical_garden == "Leipzig") %>% 
filter(Plant == "Helleborus foetidus") %>%
select(Date, Floral_abundance) %>% 
mutate(Doy = lubridate::yday(Date)) %>% 
distinct()  %>% 
mutate(Doy_difference = abs(sort(Doy)[1] - Doy)) %>% 
mutate(Doy = 82 - Doy_difference) %>% 
mutate(Date = Date[1] - Doy_difference) %>% 
filter(!Doy_difference == 0)

h_foetidus_leipzig = bind_rows(hf_a, hf_b) %>% 
select(!Doy_difference)
#Check how the distribution looks
ggplot(hf,aes(x = Date, y = Floral_abundance)) + 
stat_smooth(method = "gam",
method.args=list(family=poisson),
geom = "area",
alpha=0.75,
span = 0.1)
#Extract values for the dates of interest
#Run a gam model in order to predict missing phenologies
#Fit a regression model
h_foetidus_gam = mgcv::gam(Floral_abundance ~ s(Doy, k=3),
                   #bs="fs",
                   gamma = 3,
                   poisson,
                   data = h_foetidus_leipzig)

#Plot predicted values
predict_gam(h_foetidus_gam, tran_fun = exp) %>%
plot("Doy")
#Check if it works. Round values so very small values are considered as zero
h_foetidus_new = tibble(Doy = c(unique(jena_phenobs$Doy)))
h_foetidus_new$Flowering_intensity = round(predict(h_foetidus_gam, h_foetidus_new, type = "response"))
#Set values under 1 as 0, convert to percentage and add needed columns
h_foetidus_new = h_foetidus_new %>% 
mutate(Flowering_intensity = Flowering_intensity/ max(Flowering_intensity)*100) %>% 
mutate(Flowering_intensity = if_else(Flowering_intensity<1, 0, Flowering_intensity)) %>% 
mutate(Flowers_opening = if_else(Flowering_intensity==0, "no", "y")) %>% 
mutate(Garden = "Leipzig") %>% 
mutate(Species = "Helleborus foetidus") %>% 
filter(Flowers_opening == "y")

#To add 0' with dates without flowering 
#let's recover the sampled dates and do left join with them
values_doy = tibble(Doy = unique(flowering_data$Doy))
h_foetidus_new = left_join(values_doy, h_foetidus_new) 
h_foetidus_new = h_foetidus_new %>% 
mutate(Flowering_intensity = 
      if_else(is.na(Flowering_intensity), 0,Flowering_intensity)) %>% 
mutate(Flowers_opening = 
      if_else(is.na(Flowers_opening), "no", Flowers_opening)) %>% 
mutate(Garden = "Leipzig") %>% 
mutate(Species = "Helleborus foetidus")

#Add H. foetidus phenology
flowering_data = bind_rows(flowering_data, h_foetidus_new)

# 2. Primula veris ----
colnames(jena_phenobs) = str_replace(colnames(jena_phenobs), " ", "_")
p_veris_jena = jena_phenobs %>% 
select(Date, Doy, Species, Flowers_opening, Flowering_intensity) %>% 
mutate(Date = as.Date(str_replace_all(Date, "[.]", "/"), "%d/%m/%Y")) %>% 
filter(Species == "Primula veris")
#Extract values for the dates of interest
#Run a gam model in order to predict missing phenologies
#Fit a regression model
p_veris_gam = mgcv::gam(Flowering_intensity ~ s(Doy, k=3),
                   #bs="fs",
                   gamma = 3,
                   poisson,
                   data = p_veris_jena)
#Plot predicted values
predict_gam(p_veris_gam, tran_fun = exp) %>%
plot("Doy")

#Check if it works. Round values so very small values are considered as zero
p_veris_new = tibble(Doy = c(unique(jena_phenobs$Doy)))
p_veris_new$Flowering_intensity = round(predict(p_veris_gam, p_veris_new, type = "response"))
#Set values under 1 as 0, convert to percentage and add needed columns
p_veris_new = p_veris_new %>% 
mutate(Flowering_intensity = Flowering_intensity/ max(Flowering_intensity)*100) %>% 
mutate(Flowering_intensity = if_else(Flowering_intensity < 1, 0, Flowering_intensity)) %>% 
mutate(Flowers_opening = if_else(Flowering_intensity==0, "no", "y")) %>% 
mutate(Garden = "Leipzig") %>% 
mutate(Species = "Primula veris") %>% 
filter(Flowers_opening == "y")

#To add 0' with dates without flowering 
#let's recover the sampled dates and do left join with them
values_doy = tibble(Doy = unique(flowering_data$Doy))
p_veris_new = left_join(values_doy, p_veris_new) 
p_veris_new = p_veris_new %>% 
mutate(Flowering_intensity = 
      if_else(is.na(Flowering_intensity), 0,Flowering_intensity)) %>% 
mutate(Flowers_opening = 
      if_else(is.na(Flowers_opening), "no", Flowers_opening)) %>% 
mutate(Garden = "Leipzig") %>% 
mutate(Species = "Primula veris")

#Add P. veris phenology
flowering_data = bind_rows(flowering_data, p_veris_new)

# 3. Lamium album ----   
l_album_jena = jena_phenobs %>% 
select(Date, Doy, Species, Flowers_opening, Flowering_intensity) %>% 
mutate(Date = as.Date(str_replace_all(Date, "[.]", "/"), "%d/%m/%Y")) %>% 
filter(Species == "Lamium album")
#Extract values for the dates of interest
#Run a gam model in order to predict missing phenologies
#Fit a regression model
l_album_gam = mgcv::gam(Flowering_intensity ~ s(Doy, k=3),
                   #bs="fs",
                   gamma = 3,
                   poisson,
                   data = l_album_jena)
#Plot predicted values
predict_gam(l_album_gam, tran_fun = exp) %>%
plot("Doy")







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








#Merge all----
leipzig_phen = left_join(leipzig_focals, flowering_data)
leipzig_phen %>% 
filter(is.na(Doy))
#Save phenology for Jena species
saveRDS(leipzig_phen, "Data/Phenology_data/clean_plant_phenobs_leipzig.rds")
#Plot all----
#Because it may not work at 1st, generate a vector and do it for a subset of spp
v = unique(leipzig_phen$Species)
d1 = halle_phen %>% 
filter(Species %in% v) %>% 
mutate(Flowering_intensity = if_else(Flowers_opening=="no", 0, Flowering_intensity)) 
#Find first numeric value of flowering intensity to order species
lev_species = d1 %>% 
group_by(Species) %>% 
filter(Flowers_opening == "y") %>% 
slice_min(Doy) %>% 
arrange(Doy) %>% 
pull(Species)
#This should be the order to be printed (from 1st flowering to last)
d1$Species = factor(d1$Species, levels = lev_species)
#Generate as many colors as species
colfunc = colorRampPalette(c("cyan4", "brown3"))
cols = colfunc(nlevels(d1$Species))
#Plot
d1 %>% 
ggplot(aes(x = Doy, y = Flowering_intensity, fill = Species)) + 
stat_smooth(method = "gam",
method.args=list(family=poisson),
geom = "area",
alpha=0.75,
span = 0.1) +
theme_minimal()+
theme(legend.position = "none", 
      strip.background = element_blank(),
      strip.text.x = element_blank(),
      panel.spacing = unit(-1.5,'lines')) +
coord_cartesian(expand = FALSE) +
scale_y_continuous(expand = c(0,0), breaks = c(NULL), labels = c(NULL)) +
facet_wrap(~Species, ncol=1) +
ylab("Flowering intensity") +
xlab("Day of the year") +
scale_fill_manual(values = cols) +
ggtitle("Halle Botanical Garden")




