#Create phenological predictions for pollinators
#Need to be done species by species given that each species
#has different number of records and their case can be context specific
#species with no records are likely excluded as there is no enough information

#Load libraries
library(mgcv)
library(tidygam)
library(dplyr)
library(ggplot2)

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
summarise(n_individuals = length(Doy)) %>%
mutate(Week = ceiling(Doy / 7))
#Arrange by number of cases per species
spp_order = oc %>% 
group_by(Species) %>% 
summarise(n_records = length(Species)) %>% 
arrange(-n_records) %>% 
filter(n_records > 50) %>% 
pull(Species)
#Order the tibble by number of cases per species
oc1 = oc1 %>%
arrange(match(Species, spp_order))

#Aggregate by week
#Let's see if models work better with aggregated data
oc2 = oc1 %>% 
group_by(Species, Week) %>% 
summarise(n_ind_week = sum(n_individuals))


#Create phenological predictions for pollinators
#Need to be done species by species given that each species
#has different number of records and their case can be context specific
#species with no records are likely excluded as there is no enough information

#Load libraries
library(mgcv)
library(tidygam)
library(dplyr)
library(ggplot2)

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
filter(n_records > 50) %>% 
pull(Species)
#Order the tibble by number of cases per species
oc1 = oc1 %>%
arrange(match(Species, spp_order))

#Now make the absence of value as a zero so it models better the extremes
#Create for each species 
#Create tibble
unique_species_sequence = oc1 %>% 
distinct(Species) %>% 
group_by(Species) %>% 
tidyr::crossing(Doy = 1:365)

#Bind both datasets
oc2 = left_join(unique_species_sequence, oc1)
#Convert NA's into 0's
oc2 = oc2 %>% 
mutate(n_individuals = case_when(is.na(n_individuals) ~ 0,
                                 TRUE ~ n_individuals))


#Create general function 
#This function runs a GAM and plots 
#the probabilities relative to the maximum abundance
#Define the function
create_prediction <- function(species) {
  #Filter data for the specified species
  sp_data = oc2 %>% filter(Species == species)
  #Fit the GAM model
  sp_model = gam(n_individuals ~ s(Doy, k = 18, m=2),
  #default is m=2 to avoid overfitting m=1 will provide less smooth outputs 
  family = nb(link = "log"), data = sp_data, method = "REML")
  #Create a new data frame with unique Doy values
  unique_dates = tibble(Doy = seq(from = 1, to = 365, by = 1))
  #Make predictions using the new data frame
  predicted_values = predict(sp_model, unique_dates, type = "response")
  #Assign the predicted values to the new data frame
  unique_dates$Abundances = round(predicted_values)
  #Normalize the predicted values to convert to probabilities
  max_abundance = max(unique_dates$Abundances)
  unique_dates = unique_dates %>%
    mutate(Probability = Abundances / max_abundance)
  #Add the species name to the data frame
  unique_dates$Species = species
  #Plot the predicted probabilities
  plot = ggplot(unique_dates, aes(x = Doy, y = Probability)) +
    geom_line(color = "blue") +
    labs(title = paste("Predicted Probabilities for", species, "Over Day of Year (Doy)"),
         x = "Day of Year (Doy)",
         y = "Predicted Probability") +
    theme_minimal()
# Return a tibble with the predictions and the plot
  return(tibble(Species = species, Predictions = list(unique_dates), Plot = list(plot)))
  }

#Run it species by species
# Assuming oc1 and spp_order are already defined
predictions_list <- list()

for (i in 1:length(spp_order)) {
  species <- spp_order[i]
  predictions <- create_prediction(species = species)
  predictions_list[[species]] <- predictions
  print(paste("Completed predictions for", species))
}

#Combine all predictions into a single data frame if needed
all_predictions <- bind_rows(predictions_list, .id = "Species")

#Now save all plots in a folder ordered by number so I can see if it worked
#For some species is not working, let's ensure that we have a plot for all species
sp_tibble = tibble(Species = spp_order)
all_predictions1 = left_join(sp_tibble, all_predictions) 

for (i in 1:length(all_predictions1$Species)) {
  species <- all_predictions1$Species[i]
  
  # Extract the plot from the tibble using double brackets [[i]]
  plot <- all_predictions1$Plot[[i]]
  
  # Check if plot is not NULL before saving
  if (!is.null(plot)) {
    # Construct the filename with the species index and name
    plot_filename <- paste0("Data/Pollinator_phenology/", i, "_", stringr::str_replace(species, " ","_"), ".png")
    
    # Save the plot to a file
    ggsave(filename = plot_filename, plot = plot, width = 8, height = 6)
    
    print(paste("Saved plot for", species))
  } else {
    print(paste("Plot for", species, "is NULL, skipping saving."))
  }
}

# Note the fit of the models look ok/check for examples...
sp_data = oc1 %>% filter(Species == spp_order[2])
sp_model = gam(n_individuals ~ s(Doy, k = 30),
               family = nb(link = "log"), data = sp_data, method = "REML")
qq.gam(sp_model, main = "QQ plot of residuals")

#Now check the generated plots in the folder one by one
#Create vector of plots that are not creating a nice phenology







#Looks good
#Create a new data frame with unique Doy values
unique_dates = tibble(Doy = seq(from = 1, to = 365, by = 1))
#Make predictions using the new data frame
predicted_values = predict(sp_model, unique_dates, type = "response")
#Assign the predicted values to the new data frame
unique_dates$Abundances = round(predicted_values)
#Normalize the predicted values to convert to probabilities
max_abundance = max(unique_dates$Abundances)
unique_dates = unique_dates %>%
mutate(Probability = Abundances / max_abundance)
#Add the species name to the data frame
unique_dates$Species = spp_order[48]
#Plot the predicted probabilities
ggplot(unique_dates, aes(x = Doy, y = Probability)) +
  geom_line(color = "blue") +
    labs(title = paste("Predicted Probabilities for", spp_order[48], "Over Day of Year (Doy)"),
         x = "Day of Year (Doy)",
         y = "Predicted Probability") +
    theme_minimal()


