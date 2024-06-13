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
halle_phenobs = read_csv("Data/Phenology_data/raw_plant_phenobs_halle.csv")

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

#Filter out cyclamen coum as it has no interactions
leipzig_focals = leipzig_focals %>% filter(!Species == "Cyclamen coum") 

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
ggplot(h_foetidus_leipzig,aes(x = Date, y = Floral_abundance)) + 
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
mutate(Flowering_intensity = round(Flowering_intensity/ max(Flowering_intensity)*100)) %>% 
mutate(Flowering_intensity = if_else(Flowering_intensity<1, NA, Flowering_intensity)) %>% 
mutate(Flowers_opening = if_else(is.na(Flowering_intensity), "no", "y")) %>% 
mutate(Garden = "Leipzig") %>% 
mutate(Species = "Helleborus foetidus") %>% 
filter(Flowers_opening == "y")

#To add 0' with dates without flowering 
#let's recover the sampled dates and do left join with them
values_doy = tibble(Doy = unique(flowering_data$Doy))
h_foetidus_new = left_join(values_doy, h_foetidus_new) 
h_foetidus_new = h_foetidus_new %>% 
mutate(Flowers_opening = 
      if_else(is.na(Flowers_opening), "no", Flowers_opening)) %>% 
mutate(Garden = "Leipzig") %>% 
mutate(Species = "Helleborus foetidus")

ggplot(h_foetidus_new,aes(x = Doy, y = Flowering_intensity)) + 
stat_smooth(method = "gam",
method.args=list(family=poisson),
geom = "area",
alpha=0.75,
span = 0.1)


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
mutate(Flowering_intensity = round(Flowering_intensity/ max(Flowering_intensity)*100)) %>% 
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
#We use halle flower counts, similar weather to leipzig
#Plot available data of flower number
int_data %>% 
filter(Botanical_garden == "Halle") %>% 
filter(Plant == "Lamium album") %>%
filter(Sampling == "Focal") %>% 
distinct(Botanical_garden, Floral_abundance, Date) %>% 
ggplot(aes(Date, Floral_abundance, group = Botanical_garden)) +
geom_point()+
ggalt::geom_xspline(color = "black") 
#looks good
#store data and run model
l_album_jena = int_data %>% 
filter(Botanical_garden == "Jena") %>% 
filter(Plant == "Lamium album") %>%
filter(Sampling == "Focal") %>% 
mutate(Doy = lubridate::yday(Date)) %>% 
distinct(Botanical_garden, Floral_abundance, Doy)
#Extract values for the dates of interest
#Run a gam model in order to predict missing phenologies
#Fit a regression model
l_album_gam = mgcv::gam(Floral_abundance ~ s(Doy, k=3),
                   #bs="fs",
                   gamma = 3,
                   poisson,
                   data = l_album_jena)
#Plot predicted values
predict_gam(l_album_gam, tran_fun = exp) %>%
plot("Doy")

#Check if it works. Round values so very small values are considered as zero
l_album_new = tibble(Doy = c(unique(leipzig_phenobs$Doy)))
l_album_new$Flowering_intensity = round(predict(l_album_gam, l_album_new, type = "response"))
#Set values under 1 as 0, convert to percentage and add needed columns
l_album_new = l_album_new %>% 
mutate(Flowering_intensity = round(Flowering_intensity/ max(Flowering_intensity)*100)) %>% 
mutate(Flowering_intensity = if_else(Flowering_intensity < 1, 0, Flowering_intensity)) %>% 
mutate(Flowers_opening = if_else(Flowering_intensity==0, "no", "y")) %>% 
mutate(Garden = "Leipzig") %>% 
mutate(Species = "Lamium album") %>% 
filter(Flowers_opening == "y")

#To add 0' with dates without flowering 
#let's recover the sampled dates and do left join with them
values_doy = tibble(Doy = unique(flowering_data$Doy))
l_album_new = left_join(values_doy, l_album_new) 
l_album_new = l_album_new %>% 
mutate(Flowering_intensity = 
      if_else(is.na(Flowering_intensity), 0,Flowering_intensity)) %>% 
mutate(Flowers_opening = 
      if_else(is.na(Flowers_opening), "no", Flowers_opening)) %>% 
mutate(Garden = "Leipzig") %>% 
mutate(Species = "Lamium album")

#Add L. album phenology
flowering_data = bind_rows(flowering_data, l_album_new)

# 4. Silene viscaria ----   
#Prepare data
s_viscaria_jena = int_data %>% 
filter(Botanical_garden == "Halle") %>% 
filter(Plant == "Silene viscaria") %>%
distinct(Floral_abundance, Date) %>% 
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
s_viscaria_new = tibble(Doy = c(unique(leipzig_phenobs$Doy)))
s_viscaria_new$Flowering_intensity = round(predict(s_viscaria_gam, s_viscaria_new, type = "response"))
#Set values under 1 as 0, convert to percentage and add needed columns
s_viscaria_new = s_viscaria_new %>% 
mutate(Flowering_intensity = round(Flowering_intensity/ max(Flowering_intensity)*100)) %>% 
mutate(Flowering_intensity = if_else(Flowering_intensity<1, 0, Flowering_intensity)) %>% 
mutate(Flowers_opening = if_else(Flowering_intensity==0, "no", "y")) %>% 
mutate(Garden = "Leipzig") %>% 
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
mutate(Garden = "Leipzig") %>% 
mutate(Species = "Silene viscaria")

#Add S. viscaria phenology
flowering_data = bind_rows(flowering_data, s_viscaria_new)

# 5. Centranthus ruber----
int_data %>% 
filter(Botanical_garden == "Leipzig") %>% 
filter(Plant == "Centranthus ruber") %>%
filter(Sampling == "Focal") %>% 
distinct(Botanical_garden, Floral_abundance, Date) %>% 
ggplot(aes(Date, Floral_abundance, group = Botanical_garden)) +
geom_point()+
ggalt::geom_xspline(color = "black") 

#Prepare data
c_ruber_leipzig = int_data %>% 
filter(Botanical_garden == "Leipzig") %>% 
filter(Plant == "Centranthus ruber") %>%
filter(Sampling == "Focal") %>% 
distinct(Botanical_garden, Floral_abundance, Date) %>% 
mutate(Doy = lubridate::yday(Date))

#Add one row to avoid over predicting values in early stage
c_ruber_to_add = tibble(Botanical_garden = "Leipzig",
                      Floral_abundance = 5,
                      Doy = 150)

c_ruber_leipzig = bind_rows(c_ruber_leipzig, c_ruber_to_add)

#Add one date with low values at the beigining so the model can predict well
#Create one row to be added that will make the flowering simulation work
to_be_added = tibble(Botanical_garden = "Leipzig", Floral_abundance = 150, Date = as.Date("2023-06-01"))
to_be_added = to_be_added %>% 
mutate(Doy = lubridate::yday(Date))
c_ruber_leipzig = bind_rows(c_ruber_leipzig, to_be_added)
#Run a gam model in order to predict missing phenologies
#Fit a regression model
c_ruber_gam =mgcv::gam(Floral_abundance ~ s(Doy, k=3),
                   #bs="fs",
                   gamma = 3,
                   poisson,
                   data = c_ruber_leipzig)
#Plot predicted values
predict_gam(c_ruber_gam, tran_fun = exp) %>%
plot("Doy")

#Check if it works. Round values so very small values are considered as zero
c_ruber_new = tibble(Doy = c(unique(jena_phenobs$Doy)))
c_ruber_new$Flowering_intensity = round(predict(c_ruber_gam, c_ruber_new, type = "response"))
#Set values under 1 as 0, convert to percentage and add needed columns
c_ruber_new = c_ruber_new %>% 
mutate(Flowering_intensity = round(Flowering_intensity/ max(Flowering_intensity)*100)) %>% 
mutate(Flowering_intensity = if_else(Flowering_intensity<15, 0, Flowering_intensity)) %>% 
mutate(Flowers_opening = if_else(Flowering_intensity==0, "no", "y")) %>% 
mutate(Garden = "Leipzig") %>% 
mutate(Species = "Centranthus ruber") %>% 
filter(Flowers_opening == "y")

#To add 0' with dates without flowering 
#let's recover the sampled dates and do left join with them
values_doy = tibble(Doy = unique(flowering_data$Doy))
c_ruber_new = left_join(values_doy, c_ruber_new) 
c_ruber_new = c_ruber_new %>% 
mutate(Flowering_intensity = 
      if_else(is.na(Flowering_intensity), 0,Flowering_intensity)) %>% 
mutate(Flowers_opening = 
      if_else(is.na(Flowers_opening), "no", Flowers_opening)) %>% 
mutate(Garden = "Leipzig") %>% 
mutate(Species = "Centranthus ruber")
#Add C. ruber phenology
flowering_data = bind_rows(flowering_data, c_ruber_new)

# 6. Platycodon grandiflorus -----
#Rename cols with underscore
colnames(halle_phenobs) = str_replace(colnames(halle_phenobs), " ", "_")
p_grandiflorus_jena = halle_phenobs %>% 
select(Date, Doy, Species, Flowers_opening, Flowering_intensity) %>% 
mutate(Date = as.Date(str_replace_all(Date, "[.]", "/"), "%d/%m/%Y")) %>% 
filter(Species == "Platycodon grandiflorum") %>% 
mutate(Species = recode(Species, "Platycodon grandiflorum" = "Platycodon grandiflorus"))

#Add one row to avoid over predicting values in early stage
p_grandiflorus_to_add = tibble(Date= NA, Doy = 160, Species = "Platycodon grandiflorus",
                       Flowers_opening = "no", Flowering_intensity= 1)

p_grandiflorus_jena = bind_rows(p_grandiflorus_jena, p_grandiflorus_to_add)

#Extract values for the dates of interest
#Run a gam model in order to predict missing phenologies
#Fit a regression model
p_grandiflorus_gam = mgcv::gam(Flowering_intensity ~ s(Doy, k=3),
                   #bs="fs",
                   gamma = 3,
                   poisson,
                   data = p_grandiflorus_jena)
#Plot predicted values
predict_gam(p_grandiflorus_gam, tran_fun = exp) %>%
plot("Doy")
#Looks ok!

#Check if it works. Round values so very small values are considered as zero
p_grandiflorus_new = tibble(Doy = c(unique(jena_phenobs$Doy)))
p_grandiflorus_new$Flowering_intensity = round(predict(p_grandiflorus_gam, p_grandiflorus_new, type = "response"))
#Set values under 1 as 0, convert to percentage and add needed columns
p_grandiflorus_new = p_grandiflorus_new %>% 
mutate(Flowering_intensity = round(Flowering_intensity/ max(Flowering_intensity)*100)) %>% 
mutate(Flowering_intensity = if_else(Flowering_intensity<15, 0, Flowering_intensity)) %>% 
mutate(Flowers_opening = if_else(Flowering_intensity==0, "no", "y")) %>% 
mutate(Garden = "Leipzig") %>% 
mutate(Species = "Platycodon grandiflorus") %>% 
filter(Flowers_opening == "y")

#To add 0' with dates without flowering 
#let's recover the sampled dates and do left join with them
values_doy = tibble(Doy = unique(flowering_data$Doy))
p_grandiflorus_new = left_join(values_doy, p_grandiflorus_new) 
p_grandiflorus_new = p_grandiflorus_new %>% 
mutate(Flowering_intensity = 
      if_else(is.na(Flowering_intensity), 0,Flowering_intensity)) %>% 
mutate(Flowers_opening = 
      if_else(is.na(Flowers_opening), "no", Flowers_opening)) %>% 
mutate(Garden = "Leipzig") %>% 
mutate(Species = "Platycodon grandiflorus")
#Add P. grandiflorus phenology
flowering_data = bind_rows(flowering_data, p_grandiflorus_new)

#Fix Silene vulgaris (late value)
flowering_data = flowering_data %>% 
mutate(Flowering_intensity = case_when(Doy == 228 ~ 0, 
       T ~ Flowering_intensity))

str(flowering_data)


#Merge all----
leipzig_phen = left_join(leipzig_focals, flowering_data)
leipzig_phen %>% 
filter(is.na(Doy))
#Save phenology for Jena species
saveRDS(leipzig_phen, "Data/Phenology_data/clean_plant_phenobs_leipzig.rds")
#Plot all----
#Because it may not work at 1st, generate a vector and do it for a subset of spp
v = unique(leipzig_phen$Species)
d1 = leipzig_phen %>% 
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
spp = unique(d1$Species)

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
ggtitle("Leipzig Botanical Garden")



