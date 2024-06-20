#Create phenological predictions for pollinators
#Need to be done species by species given that each species
#has different number of records and their case can be context specific
#species with no records are likely excluded as there is no enough information

#Load libraries
library(mgcv)
library(tidygam)
library(dplyr)
#Load data
#Clean occurrence data
oc_extraction_coords = readRDS("Data/Working_files/oc_extraction.rds")
#Unique dates 
unique_dates = readRDS("Data/Working_files/unique_dates.rds")

#Select columns of interest
oc = oc_extraction_coords %>% 
select(Species, Doy)
#Count individuals, number of records by date and species
oc1 = oc %>% 
group_by(Species, Doy) %>% 
summarise(n_individuals = length(Doy))
#Arrange by number of cases per species
spp_order = oc %>% 
group_by(Species) %>% 
summarise(n_records = length(Species)) %>% 
arrange(-n_records) %>% 
pull(Species)
#Order the tibble by number of cases per species
oc1 = oc1 %>%
arrange(match(Species, spp_order))

#Start with the 1st species
#Gonepteryx rhamni----
spp_order[1]
# Visualize the data
oc1 %>% 
filter(Species == spp_order[1]) %>% 
ggplot(aes(x = Doy, y = n_individuals)) +
geom_line() +
labs(title = "Observations Over Time", x = "Date", y = "Count")
#Maybe build phenology graphs
oc %>% filter(Species == spp_order[1]) %>% 
ggplot(aes(x = Doy)) + geom_density()




#Make this code functional!!
create_prediction = function(data, species){
#Species 1
sp1_data = oc1 %>% filter(Species == species)
#Now try to model and create phenological predictions based on observations
sp1_model = gam(n_individuals ~ s (Doy, k=5),
                   #bs="fs",
                   gamma = 0.8,
                   poisson,
                   data = sp1_data)
#Plot predicted values
predict_gam(sp1_model, tran_fun = exp) %>%
plot("Doy")
#Doy
sp1_new = tibble(Doy = c(unique(unique_dates$Doy)))
sp1_new$Abundances = round(predict(sp1_model, sp1_new, type = "response"))
sp1_new$Species = species

}

species_1 <- spp_order[2]
sp1_predictions <- create_prediction(data = oc1, species = species_1)
print(sp1_predictions)
