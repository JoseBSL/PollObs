#Script to prepare plant phenology (data from froPhenObs app)
#Jena only
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
jena_phenobs = read_csv("Data/Phenology_data/raw_plant_phenobs_jena.csv")
#Set ggplot theme
theme_set(theme_light())

#Exclude H. foetidus
#This species was included by mistake and only monitored properly in Halle
#First get focal species recorded in jena in the int dataset
jena_focals = int_data %>% 
filter(Botanical_garden == "Jena") %>% 
filter(Sampling == "Focal") %>% 
distinct(Plant) %>% 
select(Plant) %>% 
rename(Species = Plant) %>% 
filter(!Species == "Helleborus foetidus")

#Rename cols with underscore
colnames(jena_phenobs) = str_replace(colnames(jena_phenobs), " ", "_")

#Prepare data with right format of dates
flowering_data = jena_phenobs %>% 
select(Date, Doy, Species, Flowers_opening, Flowering_intensity) %>% 
mutate(Date = as.Date(str_replace_all(Date, "[.]", "/"), "%d/%m/%Y")) %>% 
group_by(Species) %>% 
#filter(Flowers_opening == "y") %>% 
mutate(Species = recode_factor(Species, "Anemone pulsatilla" = "Pulsatilla vulgaris")) %>% 
mutate(Species = recode_factor(Species, "Securigera varia" = "Coronilla varia")) %>% 
mutate(Species = recode_factor(Species, "Anemone nemorosa" = "Anemonoides nemorosa")) %>% 
mutate(Species = recode_factor(Species, "Paeonia mlokosewitschii" = "Paeonia daurica")) 

#Fix some species names first and homogenize with our dataset
d = left_join(jena_focals, flowering_data)
d %>% 
filter(is.na(Date))

#Missing phenologies
#1. Silene viscaria
#2. Anemone huepensis
#3. Fuchsia magellanica
#4. Leucanthemum vulgare

#1. Silene viscaria----
#Plot available data of flower number
int_data %>% 
filter(Botanical_garden == "Jena") %>% 
filter(Plant == "Silene viscaria") %>%
ggplot(aes(Date, Floral_abundance)) +
geom_point()+
ggalt::geom_xspline(color = "black")
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

#2. Anemone hupehensis----
int_data %>% 
filter(Botanical_garden == "Jena") %>% 
filter(Plant == "Anemone hupehensis") %>%
filter(Sampling == "Focal") %>% 
distinct(Date, Floral_abundance) %>% 
ggplot(aes(Date, Floral_abundance)) +
geom_point()+
ggalt::geom_xspline(color = "black")
#Prepare data
a_hupehensis_jena = int_data %>% 
filter(Botanical_garden == "Jena") %>% 
filter(Plant == "Anemone hupehensis") %>%
filter(Sampling == "Focal") %>% 
distinct(Floral_abundance, Date) %>% 
mutate(Floral_abundance1 = Floral_abundance/max(Floral_abundance)) %>% 
mutate(Doy = lubridate::yday(Date))
#Run a gam model in order to predict missing phenologies
#Fit a regression model
a_hupehensis_gam = mgcv::gam(Floral_abundance ~ s(Doy, k=5),
                   #bs="fs",
                   gamma = 0.8,
                   poisson,
                   data = a_hupehensis_jena)
#Plot predicted values
predict_gam(a_hupehensis_gam, tran_fun = exp) %>%
plot("Doy")
#Check if it works. Round values so very small values are considered as zero
a_hupehensis_new = tibble(Doy = c(unique(jena_phenobs$Doy)))
a_hupehensis_new$Flowering_intensity = round(predict(a_hupehensis_gam, a_hupehensis_new, type = "response"))
#Set values under 1 as 0, convert to percentage and add needed columns
a_hupehensis_new = a_hupehensis_new %>% 
mutate(Flowering_intensity = Flowering_intensity/ max(Flowering_intensity)*100) %>% 
mutate(Flowering_intensity = if_else(Flowering_intensity<1, 0, Flowering_intensity)) %>% 
mutate(Flowers_opening = if_else(Flowering_intensity==0, "no", "y")) %>% 
mutate(Garden = "Jena") %>% 
mutate(Species = "Anemone hupehensis") %>% 
filter(Flowers_opening == "y")

#To add 0' with dates without flowering 
#let's recover the sampled dates and do left join with them
values_doy = tibble(Doy = unique(flowering_data$Doy))
a_hupehensis_new = left_join(values_doy, a_hupehensis_new) 
a_hupehensis_new = a_hupehensis_new %>% 
mutate(Flowering_intensity = 
      if_else(is.na(Flowering_intensity), 0,Flowering_intensity)) %>% 
mutate(Flowers_opening = 
      if_else(is.na(Flowers_opening), "no", Flowers_opening)) %>% 
mutate(Garden = "Jena") %>% 
mutate(Species = "Anemone hupehensis")

#Add A. hupehensis phenology
flowering_data = bind_rows(flowering_data, a_hupehensis_new)

#3. Fuchsia magellanica----
#Plot available data of flower number
#Plotting 2 botanical gardens as we have only 2 points for Jena  
int_data %>% 
filter(!Botanical_garden == "Leipzig") %>% 
filter(Plant == "Fuchsia magellanica") %>%
filter(Sampling == "Focal") %>% 
distinct(Botanical_garden, Floral_abundance, Date) %>% 
ggplot(aes(Date, Floral_abundance, group = Botanical_garden)) +
geom_point()+
ggalt::geom_xspline(color = "black") +
ggalt::geom_xspline(color = "blue", aes(Date, Floral_abundance *0.55, group = Botanical_garden))

#By scaling the Halle flowering seems 
#that we can get an approximate fit to what would be expected 
#for Jena (based on the only 2 observations)
int_data %>% 
filter(Botanical_garden == "Halle") %>% 
filter(Plant == "Fuchsia magellanica") %>%
filter(Sampling == "Focal") %>% 
distinct(Botanical_garden, Floral_abundance, Date) %>% 
ggplot(aes(Date, Floral_abundance*0.55, group = Botanical_garden)) +
geom_point()+
ggalt::geom_xspline(color = "black")
#Prepare data
f_magellanica_jena = int_data %>% 
filter(Botanical_garden == "Halle") %>% 
filter(Plant == "Fuchsia magellanica") %>%
filter(Sampling == "Focal") %>% 
distinct(Floral_abundance, Date) %>% 
mutate(Floral_abundance1 = Floral_abundance/max(Floral_abundance)) %>% 
mutate(Doy = lubridate::yday(Date))
#Run a gam model in order to predict missing phenologies
#Fit a regression model
f_magellanica_gam = mgcv::gam(Floral_abundance ~ s(Doy, k=4),
                   #bs="fs",
                   gamma = 0.8,
                   poisson,
                   data = f_magellanica_jena)
#Plot predicted values
predict_gam(f_magellanica_gam, tran_fun = exp) %>%
plot("Doy")
#Check if it works. Round values so very small values are considered as zero
f_magellanica_new = tibble(Doy = c(unique(jena_phenobs$Doy)))
f_magellanica_new$Flowering_intensity = round(predict(f_magellanica_gam, f_magellanica_new, type = "response"))
#Set values under 15 as 0, convert to percentage and add needed columns
f_magellanica_new = f_magellanica_new %>% 
mutate(Flowering_intensity = Flowering_intensity/ max(Flowering_intensity)*100) %>% 
mutate(Flowering_intensity = if_else(Flowering_intensity<15, 0, Flowering_intensity)) %>% 
mutate(Flowers_opening = if_else(Flowering_intensity==0, "no", "y")) %>% 
mutate(Garden = "Jena") %>% 
mutate(Species = "Fuchsia magellanica") %>% 
filter(Flowers_opening == "y")

#To add 0' with dates without flowering 
#let's recover the sampled dates and do left join with them
values_doy = tibble(Doy = unique(flowering_data$Doy))
f_magellanica_new = left_join(values_doy, f_magellanica_new) 
f_magellanica_new = f_magellanica_new %>% 
mutate(Flowering_intensity = 
      if_else(is.na(Flowering_intensity), 0,Flowering_intensity)) %>% 
mutate(Flowers_opening = 
      if_else(is.na(Flowers_opening), "no", Flowers_opening)) %>% 
mutate(Garden = "Jena") %>% 
mutate(Species = "Fuchsia magellanica")

#Add F. magellanica phenology
flowering_data = bind_rows(flowering_data, f_magellanica_new)

#5. Leucanthemum vulgare----
int_data %>% 
filter(Botanical_garden == "Jena") %>% 
filter(Plant == "Leucanthemum vulgare") %>%
filter(Sampling == "Focal") %>% 
distinct(Date, Floral_abundance) %>% 
ggplot(aes(Date, Floral_abundance)) +
geom_point()+
ggalt::geom_xspline(color = "black")
#Just 3 values but seems ok with this triangle shape 
#Prepare data
l_vulgare_jena = int_data %>% 
filter(Botanical_garden == "Jena") %>% 
filter(Plant == "Leucanthemum vulgare") %>%
filter(Sampling == "Focal") %>% 
distinct(Floral_abundance, Date) %>% 
mutate(Floral_abundance1 = Floral_abundance/max(Floral_abundance)) %>% 
mutate(Doy = lubridate::yday(Date))
#Run a gam model in order to predict missing phenologies
#Fit a regression model
l_vulgare_gam = mgcv::gam(Floral_abundance ~ s(Doy, k=2),
                   #bs="fs",
                   gamma = 0.8,
                   poisson,
                   data = l_vulgare_jena)
#Plot predicted values
predict_gam(l_vulgare_gam, tran_fun = exp) %>%
plot("Doy")
#Check if it works. Round values so very small values are considered as zero
l_vulgare_new = tibble(Doy = c(unique(jena_phenobs$Doy)))
l_vulgare_new$Flowering_intensity = round(predict(l_vulgare_gam, l_vulgare_new, type = "response"))
#Set values under 1 as 0, convert to percentage and add needed columns
l_vulgare_new = l_vulgare_new %>% 
mutate(Flowering_intensity = Flowering_intensity/ max(Flowering_intensity)*100) %>% 
mutate(Flowering_intensity = if_else(Flowering_intensity<1, 0, Flowering_intensity)) %>% 
mutate(Flowers_opening = if_else(Flowering_intensity==0, "no", "y")) %>% 
mutate(Garden = "Jena") %>% 
mutate(Species = "Leucanthemum vulgare") %>% 
filter(Flowers_opening == "y")

#To add 0' with dates without flowering 
#let's recover the sampled dates and do left join with them
values_doy = tibble(Doy = unique(flowering_data$Doy))
l_vulgare_new = left_join(values_doy, l_vulgare_new) 
l_vulgare_new = l_vulgare_new %>% 
mutate(Flowering_intensity = 
      if_else(is.na(Flowering_intensity), 0,Flowering_intensity)) %>% 
mutate(Flowers_opening = 
      if_else(is.na(Flowers_opening), "no", Flowers_opening)) %>% 
mutate(Garden = "Jena") %>% 
mutate(Species = "Leucanthemum vulgare")

#Add L. vulgare phenology
flowering_data = bind_rows(flowering_data, l_vulgare_new)

#Merge all----
jena_phen = left_join(jena_focals, flowering_data)
jena_phen %>% 
filter(is.na(Doy))
#Save phenology for Jena species
saveRDS(jena_phen, "Data/Phenology_data/clean_plant_phenobs_jena.rds")


#Plot all----
#Because it may not work at 1st, generate a vector and do it for a subset of spp
v = unique(jena_phen$Species)
d1 = jena_phen %>% 
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
ggtitle("Jena Botanical Garden")
