
#load libraries
library(dplyr)
library(mgcv)
library(ggplot2)
library(tibble)

#Load Leipzig data
plant_phen_leipzig = readRDS("Data/Phenology_data/clean_plant_phenobs_leipzig.rds")

spp_name_leipzig = plant_phen_leipzig %>% 
distinct(Species) %>%
pull()

unique_species_sequence = plant_phen_leipzig %>% 
distinct(Species) %>% 
group_by(Species) %>% 
tidyr::crossing(Doy = 1:365)

#Bind both datasets
plant_phen = left_join(unique_species_sequence, plant_phen_leipzig)

#Add default K and m values for each species
plant_phen = plant_phen %>% 
mutate(k_value = 4) %>% 
mutate(m_value = 2) 


plant_phen = plant_phen %>% 
mutate(k_value = case_when(
            Species == "Helianthemum nummularium" ~ 5,
            Species == "Hypericum olympicum" ~ 5,

             TRUE ~ k_value))


plant_phen = plant_phen %>% 
mutate(m_value = case_when(
            Species == "Primula veris" ~ 1,
             TRUE ~ m_value))

#To my knowleged the low dots of scabiosa and anemone hue are mistakes,
#this is way they are modelled as gaussian

#Define the function
predict_flowering_intensity <- function(spp_name_leipzig, plant_phen) {
  # Filter data for the specified species
  sp_data <- plant_phen %>% filter(Species == spp_name_leipzig)
  k_value = sp_data %>% distinct(k_value) %>% pull()
  m_value = sp_data %>% distinct(m_value) %>% pull()
  sp_data <- sp_data %>% 
    mutate(Flowering_intensity = if_else(Doy < min(sp_data$Doy[sp_data$Flowers_opening == "y"], na.rm = TRUE),
                                         0, Flowering_intensity)) %>%
    mutate(Flowering_intensity = if_else(Doy > max(sp_data$Doy[sp_data$Flowers_opening == "y"], na.rm = TRUE),
                                         0, Flowering_intensity))
  
   # Exclude 0's for Primula veris between day 100 and 300
  if (spp_name_leipzig == "Primula veris") {
    sp_data <- sp_data %>%
      mutate(Flowering_intensity = if_else(Doy > 5 & Doy < 300 & Flowering_intensity == 0, NA, Flowering_intensity))
  }
  
     # Exclude 0's for Helianthemum nummularium between day 100 and 300
  if (spp_name_leipzig == "Helianthemum nummularium") {
    sp_data <- sp_data %>%
      mutate(Flowering_intensity = if_else(Doy > 100 & Doy < 320 & Flowering_intensity == 0, NA, Flowering_intensity))
  }
  
  
  # Fit the GAM model
  sp_model <- mgcv::gam(Flowering_intensity ~ s(Doy, k = k_value, m = m_value),
                        gamma = 3,
                        family = poisson,
                        data = sp_data)
  
  # Create a new data frame with unique Doy values
  unique_dates <- tibble(Doy = seq(from = 1, to = 365, by = 1))
  
  # Make predictions using the new data frame
  predicted_values <- predict(sp_model, unique_dates, type = "response")
  
  # Assign the predicted values to the new data frame
  unique_dates$Abundances <- round(predicted_values)
  
  # Normalize the predicted values to convert to probabilities
  max_abundance <- max(unique_dates$Abundances)
  unique_dates <- unique_dates %>%
    mutate(Probability = Abundances / max_abundance)
  
  # Add the species name to the data frame
  unique_dates$Species <- spp_name_leipzig

  # Add column of flowering period
  unique_dates$Flowering_period <- if_else(unique_dates$Probability <= 0.1, "No", "Yes")
  
  # Plot the predicted probabilities
  plot <- ggplot(unique_dates, aes(x = Doy, y = Probability)) +
    geom_line(color = "black") +
    labs(title = paste("Predicted Probabilities for", spp_name_leipzig, "Over Day of Year (Doy)"),
         x = "Day of Year (Doy)",
         y = "Predicted Probability") +
    theme_minimal() +
    geom_point(data = sp_data, aes(x = Doy, y = Flowering_intensity/max(Flowering_intensity, na.rm = TRUE)))
  
  # Create tibble
  result <- tibble(Species = spp_name_leipzig, Predictions = list(unique_dates), Plot = list(plot))
  
  return(result)
}

# Run it species by species
# Assuming plant_phen and spp_name_leipzig are already defined
predictions_list <- list()

for (species in spp_name_leipzig) {
  predictions <- predict_flowering_intensity(spp_name_leipzig = species, plant_phen = plant_phen)
  predictions_list[[species]] <- predictions
  print(paste("Completed predictions for", species))
}

#Combine all predictions into a single data frame if needed
all_predictions <- bind_rows(predictions_list, .id = "Species")

#Now save all plots in a folder ordered by number so I can see if it worked
#For some species is not working, let's ensure that we have a plot for all species
sp_tibble = tibble(Species = spp_name_leipzig)
all_predictions1 = left_join(sp_tibble, all_predictions) 

for (i in 1:length(all_predictions1$Species)) {
  species <- all_predictions1$Species[i]
  
  # Extract the plot from the tibble using double brackets [[i]]
  plot <- all_predictions1$Plot[[i]]
  
  # Check if plot is not NULL before saving
  if (!is.null(plot)) {
    # Construct the filename with the species index and name
    plot_filename <- paste0("Data/Plant_phenology/Leipzig/", i, "_", stringr::str_replace(species, " ","_"), ".png")
    
    # Save the plot to a file
    ggsave(filename = plot_filename, plot = plot, width = 8, height = 6)
    
    print(paste("Saved plot for", species))
  } else {
    print(paste("Plot for", species, "is NULL, skipping saving."))
  }
}

#Create now long tibble with everything
all_to_bind = all_predictions1 %>% select(Predictions)
#Bind all
all_binded1 = purrr::map(all_to_bind, bind_rows)
all_binded2 = bind_rows(all_binded1)

#Save data
saveRDS(all_binded2, "Data/Working_files/Leipzig_plant_phenology_predictions.rds")
