#Same script as before but adjusting species according to
#their phenology (e.g., unimodal or bimodal not fully associated with voltinism)
#as some species have different peaks also associated with their life cycle
#eg., species that hybernate as adults 


#Load libraries
library(mgcv)
library(tidygam)
library(dplyr)
library(ggplot2)

#Load data
oc2 = readRDS("Data/Working_files/poll_occurrences_over_60_records.rds")
poll_phenology_info = readr::read_csv("Data/Working_files/filled_poll_phenology.csv")
#select cols of interest
poll_phenology_info = poll_phenology_info %>% 
select(Species, Phenology)
#Use this expected phenologies
#To adjust values of models in accordance to the 
#species phenology
oc2 = left_join(poll_phenology_info, oc2)
oc2_raw = oc2
#Now adjust with a case_when
#Add k equal to 5 when unimodal
#Add k equal to 20 when bimodal 
#Keep uni-bimodal as it is
#Check levels
#Add default K and m values for each species
oc2 = oc2 %>% 
mutate(k_value = case_when(
   Phenology == "bimodal" ~ 20,
   Phenology == "unimodal" ~ 5,
   TRUE ~ k_value))
#Now fix some particular cases that I have checked manually
oc2 = oc2 %>% 
mutate(m_value = case_when(
   Species == "Nomada zonata" ~ 1,
   Species == "Eucera nigrescens" ~ 1,
   TRUE ~ m_value))
#Now fix some particular cases that I have checked manually
oc2 = oc2 %>% 
mutate(k_value = case_when(
   Species == "Pieris rapae" ~ 9,
   Species == "Pieris napi" ~ 16,
   Species == "Xylocopa violacea" ~ 8,
   Species == "Timarcha goettingensis" ~ 4,
   Species == "Meliscaeva auricollis" ~ 8,
   Species == "Minettia longipennis" ~ 4,
   Species == "Eristalinus aeneus" ~ 4,
   Species == "Bombus pascuorum" ~ 5,
   Species == "Coccinella septempunctata" ~ 5,
   Species == "Calliphora vicina" ~ 4,
   Species == "Andrena gravida" ~ 4,
   Species == "Osmia caerulescens" ~ 4,
   Species == "Andrena nitida" ~ 4,
   Species == "Eucera nigrescens" ~ 20,
   TRUE ~ k_value))
#Adjust probability cutoff for some species 
#that have our phenological observations on the edge
#Eristalis hybernates as adults, reduce probability
oc2 = oc2 %>% 
mutate(prob_value = case_when(
   Species == "Eristalis tenax" ~ 0.02,
   Species == "Macroglossum stellatarum" ~ 0.05,
   Species == "Volucella zonaria" ~ 0.05,
   Species == "Sphaerophoria scripta" ~ 0.05,
   Species == "Bombus pratorum" ~ 0.02,
   Species == "Osmia cornuta" ~ 0.05,
   Species == "Valgus hemipterus" ~ 0.02,
   Species == "Pseudovadonia livida" ~ 0.05,
   Species == "Andrena flavipes" ~ 0.05,
   Species == "Anthophora plumipes" ~ 0.05,
   Species == "Andrena bicolor" ~ 0.05,
   Species == "Andrena hattorfiana" ~ 0.05,
   Species == "Eucera nigrescens" ~ 0.05,
   Species == "Nomada fulvicornis" ~ 0.05,
   TRUE ~ prob_value))

#Maybe adjust p for Minettia longipennis 0.35
spp_order = oc2 %>% distinct(Species) %>% pull()
#spp_order = spp_order[1:133] #Se
#Adjust after for species that these doesn't work
#To show vertical lines that indicate when they where detected with sampling
oc3 = oc2 %>% 
filter(!is.na(PollObs))

#Exclude values equal to zero between min and max observations
#These are an artefact that we added in order to force the curves being zero on the extremes
#We do it for these species because they have low records
#So absent values appear within their pheneology
species_to_filter = spp_order[c(16, 30:32,35:40,55,65:71, 73:80, 82:133)]

# Define the filtering function
filter_sp_data <- function(data) {
  # Calculate the min and max Doy where n_individuals is greater than 0
  min_doy <- min(data$Doy[data$n_individuals > 0])
  max_doy <- max(data$Doy[data$n_individuals > 0])
  # Filter the data based on the conditions
  filtered_data <- data %>%
    filter(!(n_individuals == 0 & Doy > min_doy & Doy < max_doy))
  return(filtered_data)}
# Filter the data for the specified species
oc2_to_filter = oc2 %>%
  filter(Species %in% species_to_filter)
oc2 = oc2 %>%
  filter(!Species %in% species_to_filter)
# Apply the function to the filtered data grouped by species
oc2_filtered = oc2_to_filter %>%
  group_by(Species) %>%
  group_modify(~ filter_sp_data(.x)) %>%
  ungroup()
#Combine the filtered data with the unmodified data for the other species
oc2 = bind_rows(oc2, oc2_filtered)  
  
#Create general function 
#This function runs a GAM and plots 
#the probabilities relative to the maximum abundance
#Define the function
create_prediction <- function(species) {
  #Filter data for the specified species
  #Generate dataset and store critical values (for modelling and cutoff probability)
  sp_data = oc2 %>% filter(Species == species)
  sp_data_raw = oc2_raw %>% filter(Species == species)
  k_value = sp_data %>% distinct(k_value) %>% pull()
  m_value = sp_data %>% distinct(m_value) %>% pull()
  prob_value = sp_data %>% distinct(prob_value) %>% pull()
  phen = sp_data %>% distinct(Phenology) %>% pull()

  sp_data1 = oc3 %>% filter(Species == species)
  #Fit the GAM model
  sp_model = gam(n_individuals ~ s(Doy, k = k_value, m=m_value),
  #default is m=2 to avoid overfitting m=1 will provide less smooth outputs 
  family = nb(link = "log"), data = sp_data, method = "REML")
  #Create a new data frame with unique Doy values
  unique_dates = tibble(Doy = seq(from = 1, to = 365, by = 1))
  #Make predictions using the new data frame
  predicted_values = predict(sp_model, unique_dates, type = "response")
  unlist(predicted_values)
  #Assign the predicted values to the new data frame
  unique_dates$Abundances = round(predicted_values)
  #Normalize the predicted values to convert to probabilities
  max_abundance = max(unique_dates$Abundances)
  unique_dates = unique_dates %>%
  mutate(Probability = Abundances / max_abundance)
  #Add the species name to the data frame
  unique_dates$Species = species
  #Add column of flying period
  #default cutoff 0.15
  unique_dates$Flying_period = if_else(unique_dates$Probability < prob_value, "No", "Yes")
  #Store again critical values in case we re-run them again
  unique_dates$k_value = k_value
  unique_dates$m_value = m_value
  unique_dates$prob_value = prob_value
  unique_dates$PollObs = sp_data_raw$PollObs
  #Plot the predicted probabilities
  plot = ggplot(unique_dates, aes(x = Doy, y = Probability)) +
  geom_line(color = "blue") +
  labs(title = paste(species, phen),
         x = "Day of Year (Doy)",
         y = "Predicted Probability") +
  theme_minimal() +
  geom_point(data = sp_data, aes(x = Doy, y = n_individuals/(max(n_individuals)))) +
  geom_vline(data = sp_data1, aes(xintercept = Doy), linetype = "dashed") 

# Return a tibble with the predictions and the plot
  return(tibble(Species = species, Predictions = list(unique_dates), Plot = list(plot)))
  }

#Run it species by species
# Assuming oc1 and spp_order are already defined
predictions_list <- list()

spp_order = spp_order[1:133]

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

#Safety check
#Evaluate which records of PollObs are considered out of the flying period
colnames(all_binded2)
checks = all_binded2 %>% 
filter(PollObs == "Yes" & Flying_period == "No")
nrow(checks) #35 rows! Try to fix it

#Create funtion that
#if PollObs == "Yes" & Flying_period == "No"
colnames(all_binded2)

unique(all_binded2$Species)

#Filter out species with a critical value that is out of the modelling
spp_to_filter = c("Pseudovadonia livida", "Nomada fulvicornis", "Andrena hattorfiana")
#Fix manually not worth to create a function for this
p_livida = all_binded2 %>% 
filter(Species %in% "Pseudovadonia livida")
unique_dates = p_livida
max_day = max(unique_dates$Doy[unique_dates$Probability > 0 & unique_dates$Flying_period == "No"], na.rm = TRUE)
max_day1 = max(unique_dates$Doy[unique_dates$PollObs == "Yes"], na.rm = TRUE)
min_prob_max_day = unique_dates %>% filter(Doy == max_day) %>%  pull(Probability)
#Assign to every value between max_day+1 and min_prob_max_day max_day1 this min_prob_max_day
unique_dates1 = unique_dates %>% 
mutate(Probability = if_else(Doy > (max_day) & Doy <= max_day1, min_prob_max_day, Probability))
#Fix Flyin_probability == No
p_livida1 = unique_dates1 %>% 
mutate(Flying_period = if_else(Doy > (max_day) & Doy <= max_day1 & Flying_period == "No", "Yes", Flying_period))

#Next species
n_fulvicornis = all_binded2 %>% 
filter(Species %in% "Nomada fulvicornis")
prob_val_before = n_fulvicornis %>% 
filter(Doy==199) %>% 
pull(Probability)
n_fulvicornis1 = n_fulvicornis %>% 
mutate(Probability = if_else(Doy == 200, prob_val_before, Probability)) %>% 
mutate(Flying_period = if_else(Doy == 200, "Yes", Flying_period))

#Next species
a_hattorfiana = all_binded2 %>% 
filter(Species %in% "Andrena hattorfiana")
prob_val_after = a_hattorfiana %>% 
filter(Doy==136) %>% 
pull(Probability)
a_hattorfiana1 = a_hattorfiana %>% 
mutate(Probability = if_else(Doy == 135, prob_val_after, Probability)) %>% 
mutate(Flying_period = if_else(Doy == 135, "Yes", Flying_period))
#bind all fixed spp
fixed = bind_rows(p_livida1, n_fulvicornis1, a_hattorfiana1)
#Filter out these species and paste them back
all_binded2 = all_binded2 %>% 
filter(!Species %in% spp_to_filter) 
#Bind them back
all_binded2 = bind_rows(all_binded2, fixed)
#Conduct safety check now
colnames(all_binded2)
checks = all_binded2 %>% 
filter(PollObs == "Yes" & Flying_period == "No")
nrow(checks) #35 rows! Try to fix it

unique(all_binded2$Species)

#Now save data (these 133 spp seem ready for now)
saveRDS(all_binded2, "Data/Working_files/poll_phenology_first_133_spp.rds")

