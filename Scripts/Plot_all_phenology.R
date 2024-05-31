

#Read botanical garden data
halle_phen = readRDS("Data/Phenology_data/clean_plant_phenobs_halle.rds")
jena_phen = readRDS("Data/Phenology_data/clean_plant_phenobs_jena.rds")
leipzig_phen = readRDS("Data/Phenology_data/clean_plant_phenobs_leipzig.rds")

#Plot all----
#Because it may not work at 1st, generate a vector and do it for a subset of spp
v = unique(d1$Species)
d1 = d1 %>% 
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


d = bind_rows(halle_phen, jena_phen)
d1 = bind_rows(d, leipzig_phen)


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



