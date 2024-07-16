
#load libraries
library(dplyr)
library(mgcv)
library(ggplot2)
library(tibble)

#Load Jena data
plant_phen_jena = readRDS("Data/Phenology_data/clean_plant_phenobs_jena.rds")

spp_name_jena = plant_phen_jena %>% 
distinct(Species) %>%
pull()

unique_species_sequence = plant_phen_jena %>% 
distinct(Species) %>% 
group_by(Species) %>% 
tidyr::crossing(Doy = 1:365)

#Bind both datasets
plant_phen = left_join(unique_species_sequence, plant_phen_jena)

#Add default K and m values for each species
plant_phen = plant_phen %>% 
mutate(k_value = 5) %>% 
mutate(m_value = 2) 

plant_phen = plant_phen %>% 
mutate(k_value = case_when(
            Species == "Lamium album" ~ 4,
            Species == "Chelidonium majus" ~ 4,
            Species == "Paeonia delavayi" ~ 4,
            Species == "Geranium sanguineum" ~ 5,
            Species == "Asphodelus albus" ~ 4,
            Species == "Linum perenne" ~ 4,
            Species == "Silene nutans" ~ 4,
            Species == "Salvia pratensis" ~ 4,
            Species == "Leucanthemum vulgare" ~ 4,
            Species == "Clematis recta" ~ 4,
            Species == "Lavandula angustifolia" ~ 4,
            Species == "Hypericum perforatum" ~ 4,
            Species == "Geum rivale" ~ 4,
            Species == "Trillium sessile" ~ 4,
             TRUE ~ k_value))

plant_phen = plant_phen %>% 
mutate(m_value = case_when(
            Species == "Chelidonium majus" ~ 1,
             Species == "Geranium sanguineum" ~ 1,
                         Species == "Linum perenne" ~ 1,


             TRUE ~ m_value))

#Define the function
predict_flowering_intensity <- function(spp_name_jena, plant_phen) {
  # Filter data for the specified species
  sp_data <- plant_phen %>% filter(Species == spp_name_jena)
  k_value = sp_data %>% distinct(k_value) %>% pull()
  m_value = sp_data %>% distinct(m_value) %>% pull()
  sp_data <- sp_data %>% 
    mutate(Flowering_intensity = if_else(Doy < min(sp_data$Doy[sp_data$Flowers_opening == "y"], na.rm = TRUE),
                                         0, Flowering_intensity)) %>%
    mutate(Flowering_intensity = if_else(Doy > max(sp_data$Doy[sp_data$Flowers_opening == "y"], na.rm = TRUE),
                                         0, Flowering_intensity))
 # Exclude 0's for Linum perenne between day 100 and 300
  if (spp_name_jena == "Linum perenne") {
    sp_data <- sp_data %>%
      mutate(Flowering_intensity = if_else(Doy > 100 & Doy < 345 & Flowering_intensity == 0, NA, Flowering_intensity))
  }
  
   # Exclude 0's for Linum perenne between day 100 and 300
  if (spp_name_jena == "Lavandula angustifolia") {
    sp_data <- sp_data %>%
      mutate(Flowering_intensity = if_else(Doy > 100 & Doy < 300 & Flowering_intensity == 0, NA, Flowering_intensity))
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
  unique_dates$Species <- spp_name_jena
  # Add column of flowering period
  unique_dates$Flowering_period <- if_else(unique_dates$Probability == 0, "No", "Yes")
  
  # Plot the predicted probabilities
  plot <- ggplot(unique_dates, aes(x = Doy, y = Probability)) +
    geom_line(color = "black") +
    labs(title = paste("Predicted Probabilities for", spp_name_jena, "Over Day of Year (Doy)"),
         x = "Day of Year (Doy)",
         y = "Predicted Probability") +
    theme_minimal() +
    geom_point(data = sp_data, aes(x = Doy, y = Flowering_intensity/max(Flowering_intensity, na.rm = TRUE)))
  
  # Create tibble
  result <- tibble(Species = spp_name_jena, Predictions = list(unique_dates), Plot = list(plot))
  
  return(result)
}

# Run it species by species
# Assuming plant_phen and spp_name_jena are already defined
predictions_list <- list()

for (species in spp_name_jena) {
  predictions <- predict_flowering_intensity(spp_name_jena = species, plant_phen = plant_phen)
  predictions_list[[species]] <- predictions
  print(paste("Completed predictions for", species))
}

#Combine all predictions into a single data frame if needed
all_predictions <- bind_rows(predictions_list, .id = "Species")

#Now save all plots in a folder ordered by number so I can see if it worked
#For some species is not working, let's ensure that we have a plot for all species
sp_tibble = tibble(Species = spp_name_jena)
all_predictions1 = left_join(sp_tibble, all_predictions) 

for (i in 1:length(all_predictions1$Species)) {
  species <- all_predictions1$Species[i]
  
  # Extract the plot from the tibble using double brackets [[i]]
  plot <- all_predictions1$Plot[[i]]
  
  # Check if plot is not NULL before saving
  if (!is.null(plot)) {
    # Construct the filename with the species index and name
    plot_filename <- paste0("Data/Plant_phenology/Jena/", i, "_", stringr::str_replace(species, " ","_"), ".png")
    
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
saveRDS(all_binded2, "Data/Working_files/Jena_plant_phenology_predictions.rds")
