#Read data and cerate phenological predictions of pollinators
#Do it species by species... There are many but I see no other way for now
#As it could be context dependent and prone to errors if I do it with all
oc_extraction_coords = readRDS("Data/Working_files/oc_extraction_coords.rds")
oc_extraction_coords = sf::st_drop_geometry(oc_extraction_coords)

#Load interaction data to predict number of individuals per date



#Select columns of interest
oc = oc_extraction_coords %>% 
select(species, Doy)
#Aggregate records by date
oc1 = oc %>% 
group_by(species, Doy) %>% 
summarise(n_individuals = length(Doy))

#Let's start with those
spp_order = oc %>% 
group_by(species) %>% 
summarise(n_records = length(species)) %>% 
arrange(-n_records) %>% 
pull(species)
#Order
oc1 = oc1 %>%
arrange(match(species, spp_order))

#Gonepteryx rhamni
spp_order[1]
# Visualize the data
oc1 %>% 
filter(species == spp_order[1]) %>% 
ggplot(aes(x = Doy, y = n_individuals)) +
geom_line() +
labs(title = "Observations Over Time", x = "Date", y = "Count")
#Maybe build phenology graphs
oc %>% filter(species == spp_order[1]) %>% 
ggplot(aes(x = Doy)) + geom_density()

sp1_data = oc1 %>% filter(species == spp_order[1])
#Now try to model and create phenological predictions based on observations
library(mgcv)
library(tidygam)
sp1_model = gam(n_individuals ~ s (Doy, k=5),
                   #bs="fs",
                   gamma = 0.8,
                   poisson,
                   data = sp1_data)
#Plot predicted values
predict_gam(sp1_model, tran_fun = exp) %>%
plot("Doy")


