
#Plot plant phenology together in this script

#Load libraries
library(dplyr)
library(ggplot2)

#Read botanical garden data
jena_phen = readRDS("Data/Phenology_data/clean_plant_phenobs_jena.rds")
halle_phen = readRDS("Data/Phenology_data/clean_plant_phenobs_halle.rds")
leipzig_phen = readRDS("Data/Phenology_data/clean_plant_phenobs_leipzig.rds")

#Jena----
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


#Edit this
label_data = d1 %>% 
group_by(Species) %>% 
filter(Flowers_opening == "y") %>% 
slice_min(Doy) %>% 
arrange(Doy) %>% 
mutate(Flowering_intensity = 0) %>% 
select(Species, Doy, Flowering_intensity) %>% 
mutate(Doy = Doy-30)


#Plot
jena_plot = d1 %>% 
ggplot(aes(x = Doy, y = Flowering_intensity, fill = Species)) + 
geom_text(data = label_data, aes(label = Species), fontface="italic",
            vjust = -0.3, hjust = -0.1, size = 2, color = "black") +
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
scale_fill_manual(values = cols)  +
ggtitle("Jena Botanical Garden")

#Halle----
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
#Edit this
label_data = d1 %>% 
group_by(Species) %>% 
filter(Flowers_opening == "y") %>% 
slice_min(Doy) %>% 
arrange(Doy) %>% 
mutate(Flowering_intensity = 0) %>% 
select(Species, Doy, Flowering_intensity) %>% 
mutate(Doy = Doy-30)

#Plot
halle_plot = d1 %>% 
ggplot(aes(x = Doy, y = Flowering_intensity, fill = Species)) + 
geom_text(data = label_data, aes(label = Species), fontface="italic",
            vjust = -0.3, hjust = -0.1, size = 2, color = "black") +
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
scale_fill_manual(values = cols)  +
ggtitle("Halle Botanical Garden")

##Leipzig----
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
#Edit this
label_data = d1 %>% 
group_by(Species) %>% 
filter(Flowers_opening == "y") %>% 
slice_min(Doy) %>% 
arrange(Doy) %>% 
mutate(Flowering_intensity = 0) %>% 
select(Species, Doy, Flowering_intensity) %>% 
mutate(Doy = Doy-30)

#Plot
leipzig_plot = d1 %>% 
ggplot(aes(x = Doy, y = Flowering_intensity, fill = Species)) + 
geom_text(data = label_data, aes(label = Species), fontface="italic",
            vjust = -0.3, hjust = -0.1, size = 2, color = "black") +
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
scale_fill_manual(values = cols)  +
ggtitle("Leipzig Botanical Garden")

#Order Jena-Halle-Leipzig
library(patchwork)
jena_plot + halle_plot + leipzig_plot
