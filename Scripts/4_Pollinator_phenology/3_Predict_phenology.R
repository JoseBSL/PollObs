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
pull(Species)
#Order the tibble by number of cases per species
oc1 = oc1 %>%
arrange(match(Species, spp_order))

#Start with the 1st species
spp_order[1]

#Create general function 
#This function runs a GAM and plots 
#the probabilities relative to the maximum abundance
#Define the function
create_prediction <- function(species) {
  #Filter data for the specified species
  sp_data = oc1 %>% filter(Species == species)
  #Fit the GAM model
  sp_model = gam(n_individuals ~ s(Doy, k = 5),
                   gamma = 0.8,
                   family = poisson,
                   data = sp_data)
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
  
  # Save the plot to a file
  plot_filename <- paste0("plot_", species, ".png")
  ggsave(filename = plot_filename, plot = plot, width = 8, height = 6)  

  print(plot) #Ensure the plot is printed
  #Return the data frame with predictions
# Return a tibble with the predictions and the plot
  return(tibble(Species = species, Predictions = list(sp1_new), Plot = list(plot)))
  }

#Run it species by species
# Assuming oc1 and spp_order are already defined
predictions_list <- list()

for (i in 1:min(50, length(spp_order))) {
  species <- spp_order[i]
  predictions <- create_prediction(species = species)
  predictions_list[[species]] <- predictions
  print(paste("Completed predictions for", species))
}

# Combine all predictions into a single data frame if needed
all_predictions <- bind_rows(predictions_list, .id = "Species")

#Check plot by plot
all_predictions$Plot[1] #Weird pattern
all_predictions$Plot[2] #Weird pattern
all_predictions$Plot[3] #Weird pattern
all_predictions$Plot[4] #Weird pattern
all_predictions$Plot[5] #Weird pattern
all_predictions$Plot[6] #Weird pattern
all_predictions$Plot[7] #Weird pattern
all_predictions$Plot[8] #Weird pattern
all_predictions$Plot[9] #Weird pattern
all_predictions$Plot[10] #Weird pattern
all_predictions$Plot[11] #Weird pattern
all_predictions$Plot[12] #Weird pattern
all_predictions$Plot[13] #Weird pattern
all_predictions$Plot[14] #Weird pattern
all_predictions$Plot[15] #Weird pattern
all_predictions$Plot[16] #Weird pattern
all_predictions$Plot[17] #Weird pattern
all_predictions$Plot[18] #Weird pattern
all_predictions$Plot[19] #Weird pattern
all_predictions$Plot[20] #Weird pattern
all_predictions$Plot[21] #Weird pattern
all_predictions$Plot[15] #Weird pattern
all_predictions$Plot[15] #Weird pattern
all_predictions$Plot[15] #Weird pattern
all_predictions$Plot[15] #Weird pattern
all_predictions$Plot[15] #Weird pattern
all_predictions$Plot[15] #Weird pattern
all_predictions$Plot[15] #Weird pattern
all_predictions$Plot[15] #Weird pattern
all_predictions$Plot[15] #Weird pattern
all_predictions$Plot[15] #Weird pattern
all_predictions$Plot[15] #Weird pattern
all_predictions$Plot[15] #Weird pattern



all_predictions$Plot[4] #Weird pattern
all_predictions$Plot[6] #Cool example of a narrow phenology
all_predictions$Plot[10] #Cool example of a narrow phenology
all_predictions$Plot[13] #Weird pattern
all_predictions$Plot[14] #Weird pattern
all_predictions$Plot[15] #Weird pattern
all_predictions$Plot[17] #Weird pattern
