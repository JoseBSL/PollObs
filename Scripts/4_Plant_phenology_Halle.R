#Script to prepare plant phenology (data from froPhenObs app)
#Halle only
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
halle_phenobs = read_csv("Data/Phenology_data/raw_plant_phenobs_halle.csv")
#Set ggplot theme
theme_set(theme_light())

#Get focal species recorded in Halle
halle_focals = int_data %>% 
filter(Botanical_garden == "Halle") %>% 
filter(Sampling == "Focal") %>% 
distinct(Plant) %>% 
select(Plant) %>% 
rename(Species = Plant) 

#Rename cols with underscore
colnames(halle_phenobs) = str_replace(colnames(halle_phenobs), " ", "_")
#Prepare data with right format of dates
flowering_data = halle_phenobs %>% 
select(Date, Doy, Species, Flowers_opening, Flowering_intensity) %>% 
mutate(Date = as.Date(str_replace_all(Date, "[.]", "/"), "%d/%m/%Y")) %>% 
mutate(Species = recode_factor(Species, "Platycodon grandiflorum" = "Platycodon grandiflorus")) %>% 
mutate(Species = recode_factor(Species, "Anemone nemorosa" = "Anemonoides nemorosa")) %>% 
mutate(Species = recode_factor(Species, "Anemone sylvestris" = "Anemonoides sylvestris"))
#Filter out one duplicate value from Tanacetum|Saponaria|Fuchsia
#And add the mean value because we don't know which one is the correct one
flowering_data = flowering_data %>% 
filter(!(Species== "Tanacetum vulgare" & Doy == "188" & Flowering_intensity == "10")) %>% 
filter(!(Species== "Saponaria officinalis" & Doy == "188" & Flowering_intensity == "15")) %>% 
filter(!(Species== "Fuchsia magellanica" & Doy == "188" & Flowering_intensity == "45")) %>% 
mutate(
  Flowering_intensity = case_when(
  Species== "Tanacetum vulgare" & Doy == "188" ~ 15,
  Species== "Saponaria officinalis" & Doy == "188" ~ 40,
  Species== "Fuchsia magellanica" & Doy == "188" ~ 60,
  T ~ Flowering_intensity))
  
#Fix some species names first and homogenize with our dataset
d = left_join(halle_focals, flowering_data)
d %>% 
filter(is.na(Date))

#Missing phenologies
#1. Origanum vulgare----
#Plot available data of flower number
int_data %>% 
filter(!Botanical_garden == "Leipzig") %>% 
filter(Plant == "Origanum vulgare") %>%
filter(Sampling == "Focal") %>% 
distinct(Botanical_garden, Floral_abundance, Date) %>% 
ggplot(aes(Date, Floral_abundance, group = Botanical_garden)) +
geom_point()+
ggalt::geom_xspline(color = "black") 
#The dynamics are quite similar 
#Let's take the phenology pattern from Jena
jena_phenobs = read_csv("Data/Phenology_data/raw_plant_phenobs_jena.csv")
#Rename cols with underscore
colnames(jena_phenobs) = str_replace(colnames(jena_phenobs), " ", "_")
#Check pattern first
jena_phenobs %>% 
filter(Species == "Origanum vulgare") %>% 
filter(Flowers_opening == "y") %>% 
ggplot(aes(Doy, Flowering_intensity)) +
geom_point()+
ggalt::geom_xspline(color = "black") 
#Prepare data  
o_vulgare_halle = jena_phenobs %>% 
filter(Species == "Origanum vulgare") %>% 
filter(Flowers_opening == "y")
#Run a gam model in order to predict missing phenologies
#Fit a regression model
o_vulgare_gam = mgcv::gam(Flowering_intensity ~ s(Doy, k=3),
                   #bs="fs",
                   gamma = 3,
                   poisson,
                   data = o_vulgare_halle)
#Plot predicted values
predict_gam(o_vulgare_gam, tran_fun = exp) %>%
plot("Doy")
#Check if it works. Round values so very small values are considered as zero
o_vulgare_new = tibble(Doy = c(unique(halle_phenobs$Doy)))
o_vulgare_new$Flowering_intensity = round(predict(o_vulgare_gam, o_vulgare_new, type = "response"))
#Set values under 1 as 0, convert to percentage and add needed columns
o_vulgare_new = o_vulgare_new %>% 
mutate(Flowering_intensity = Flowering_intensity/ max(Flowering_intensity)*100) %>% 
mutate(Flowering_intensity = if_else(Flowering_intensity<1, 0, Flowering_intensity)) %>% 
mutate(Flowers_opening = if_else(Flowering_intensity==0, "no", "y")) %>% 
mutate(Garden = "Halle") %>% 
mutate(Species = "Origanum vulgare") %>% 
filter(Flowers_opening == "y")

#To add 0' with dates without flowering 
#let's recover the sampled dates and do left join with them
values_doy = tibble(Doy = unique(flowering_data$Doy))
o_vulgare_new = left_join(values_doy, o_vulgare_new) 
o_vulgare_new = o_vulgare_new %>% 
mutate(Flowering_intensity = 
      if_else(is.na(Flowering_intensity), 0,Flowering_intensity)) %>% 
mutate(Flowers_opening = 
      if_else(is.na(Flowers_opening), "no", Flowers_opening)) %>% 
mutate(Garden = "Halle") %>% 
mutate(Species = "Origanum vulgare")

#Add O. vulgare phenology
flowering_data = bind_rows(flowering_data, o_vulgare_new)

#Merge all----
halle_phen = left_join(halle_focals, flowering_data)
halle_phen %>% 
filter(is.na(Doy))
#Save phenology for Jena species
saveRDS(halle_phen, "Data/Phenology_data/clean_plant_phenobs_halle.rds")

#Plot all----
#Because it may not work at 1st, generate a vector and do it for a subset of spp
v = unique(halle_phen$Species)
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



