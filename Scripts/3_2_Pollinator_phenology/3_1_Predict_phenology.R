#Create phenological predictions for pollinators

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
spp_order_raw = oc %>% 
group_by(Species) %>% 
summarise(n_records = length(Species)) %>% 
arrange(-n_records) 
#Save spp order raw
saveRDS(spp_order_raw, "Data/Working_files/poll_spp_order_raw.rds")

#Filter out species with insufficient number of records
spp_order = spp_order_raw %>% 
filter(n_records > 60) %>% 
pull(Species)

#Order the tibble by number of cases per species
oc1 = oc1 %>%
arrange(match(Species, spp_order))

#Aggregate by week
#Let's see if models work better with aggregated data
oc2 = oc1 %>% 
group_by(Species) %>% 
summarise(n_ind_week = sum(n_individuals))

#Add our own observations to increase precision (specially for weird species)
#Load interaction data
int_data = readRDS("Data/Working_files/interaction_data.rds")
#Aggregate individuals per date
#and convert Date to DOY
int_data_polls = int_data %>% 
filter(Pollinator_rank =="SPECIES") %>%
filter(!Pollinator == "None") %>% 
select(Pollinator_accepted_name, Date) %>% 
group_by(Pollinator_accepted_name, Date) %>% 
summarise(n_individuals = length(Pollinator_accepted_name)) %>% 
mutate(Doy = lubridate::yday(Date)) %>% 
select(!Date) %>% 
rename(Species = Pollinator_accepted_name) %>% 
mutate(PollObs = "Yes")
#To add when species are flying
doy_yes = int_data_polls %>% 
select(Species, Doy, PollObs) %>% 
distinct() 

#Bind rows
oc1 = bind_rows(oc1, int_data_polls)

#Aggregate individuals per species and date
oc1 = oc1 %>%  
group_by(Species, Doy) %>% 
summarise(n_individuals = sum(n_individuals))

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

oc2 = left_join(oc2, doy_yes, by = c("Species", "Doy"))

unique(oc2$Species)

#Add default K and m values for each species
oc2 = oc2 %>% 
mutate(k_value = 18) %>% 
mutate(m_value = 2) %>% 
mutate(prob_value = 0.10)
#Save oc2, to re-run in the next script
#When we have check eacg graph for all the species
saveRDS(oc2, "Data/Working_files/poll_occurrences_over_60_records.rds")

#Adjust after for species that these doesn't work
#To show vertical lines that indicate when they where detected with sampling
oc3 = oc2 %>% 
filter(!is.na(PollObs))

#Create general function 
#This function runs a GAM and plots 
#the probabilities relative to the maximum abundance
#Define the function
create_prediction <- function(species) {
  #Filter data for the specified species
  #Generate dataset and store critical values (for modelling and cutoff Proportion)
  sp_data = oc2 %>% filter(Species == species)
  k_value = sp_data %>% distinct(k_value) %>% pull()
  m_value = sp_data %>% distinct(m_value) %>% pull()
  prob_value = sp_data %>% distinct(prob_value) %>% pull()
  sp_data1 = oc3 %>% filter(Species == species)
  #Fit the GAM model
  sp_model = gam(n_individuals ~ s(Doy, k = k_value, m=m_value),
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
  mutate(Proportion = Abundances / max_abundance)
  #Add the species name to the data frame
  unique_dates$Species = species
  #Add column of flying period
  #default cutoff 0.15
  unique_dates$Flying_period = if_else(unique_dates$Proportion <= prob_value, "No", "Yes")
  #Store again critical values in case we re-run them again
  unique_dates$k_value = k_value
  unique_dates$m_value = m_value
  unique_dates$prob_value = prob_value
  #Plot the predicted probabilities
  plot = ggplot(unique_dates, aes(x = Doy, y = Proportion)) +
  geom_line(color = "blue") +
  labs(title = paste("Predicted Probabilities for", species, "Over Day of Year (Doy)"),
         x = "Day of Year (Doy)",
         y = "Predicted Proportion") +
  theme_minimal() +
  geom_point(data = sp_data, aes(x = Doy, y = n_individuals/(max(n_individuals)))) +
  geom_vline(data = sp_data1, aes(xintercept = Doy), linetype = "dashed") 

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

#Create now long tibble with everything
all_to_bind = all_predictions1 %>% select(Predictions)
#Bind all
all_binded1 = purrr::map(all_to_bind, bind_rows)
all_binded2 = bind_rows(all_binded1)




