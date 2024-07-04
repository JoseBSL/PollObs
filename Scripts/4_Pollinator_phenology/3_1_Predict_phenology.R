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
group_by(Species, Week) %>% 
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

#Add default K and m values for each species
oc2 = oc2 %>% 
mutate(k_value = 18) %>% 
mutate(m_value = 2) %>% 
mutate(flying_probability = 0.15)
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
  #Generate dataset and store critical values (for modelling and cutoff probability)
  sp_data = oc2 %>% filter(Species == species)
  k_value = sp_data %>% distinct(k_value) %>% pull()
  m_value = sp_data %>% distinct(m_value) %>% pull()
  prob_value = sp_data %>% distinct(flying_probability) %>% pull()
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
  mutate(Probability = Abundances / max_abundance)
  #Add the species name to the data frame
  unique_dates$Species = species
  #Add column of flying period
  #default cutoff 0.15
  unique_dates$Flying_period = if_else(unique_dates$Probability <= prob_value, "No", "Yes")
  #Store again critical values in case we re-run them again
  unique_dates$k_value = k_value
  unique_dates$m_value = m_value
  unique_dates$prob_value = prob_value
  #Plot the predicted probabilities
  plot = ggplot(unique_dates, aes(x = Doy, y = Probability)) +
  geom_line(color = "blue") +
  labs(title = paste("Predicted Probabilities for", species, "Over Day of Year (Doy)"),
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
#Now group by species
all_binded3 = all_binded2 %>% 
group_by(Species) 


#Now based on natural history and model outputs
#Edit critical values if necessary
#and rerun those that are not making ecological sense or fitting well
#Save species tibble and load it back with some natural history information
to_fill = all_binded3 %>% 
distinct(Species) %>% 
mutate(Phenology = NA_character_) %>% 
mutate(Comment = NA_character_) %>% 
mutate(Reference = NA_character_)
#Save 
readr::write_csv(to_fill, "Data/Working_files/to_fill_poll_phenology.csv")
#Load data with natural history




#Create now long tibble with everything
all_to_bind = all_predictions1 %>% select(Predictions)
#Bind all
all_binded1 = purrr::map(all_to_bind, bind_rows)
all_binded2 = bind_rows(all_binded1)

#Now group by species
all_binded3 = all_binded2 %>% 
group_by(Species) 

#Would be nice to check if all species retain the pattern No-Yes-No
pattern_check = all_binded3 %>% 
group_by(Species) %>% 
mutate(Next_value = lead(Flying_period)) %>%
filter(Flying_period != Next_value | is.na(Next_value)) %>%
select(-Next_value)
#Now filter species that do not contain the pattern
p_c = pattern_check %>% 
group_by(Species) %>%
filter(!n() == 3 & first(Flying_period) == "No" & 
         nth(Flying_period, 2) == "Yes" & 
         last(Flying_period) == "No") %>% 
distinct(Species)
#1 Aglais io            
#Has become a bivoltine species in Europe
#10.3390/insects12080683
#we can confirm this phenological pattern
#2 Harmonia axyridis
#Fix with a softer modelling approach
sp = "Harmonia axyridis"
sp_data = oc2 %>% filter(Species == sp)
sp_data1 = oc3 %>% filter(Species == sp)
sp_model = gam(n_individuals ~ s(Doy, k = 5, m=1),
               family = nb(link = "log"), data = sp_data, method = "REML")
#Create a new data frame with unique Doy values
h_axyridis = tibble(Doy = seq(from = 1, to = 365, by = 1))
#Make predictions using the new data frame
predicted_values = predict(sp_model, h_axyridis, type = "response")
#Assign the predicted values to the new data frame
h_axyridis$Abundances = round(predicted_values)
#Normalize the predicted values to convert to probabilities
max_abundance = max(h_axyridis$Abundances)
h_axyridis = h_axyridis %>%
mutate(Probability = Abundances / max_abundance)
#Add the species name to the data frame
h_axyridis$Species = sp
#Plot the predicted probabilities
h_axyridis_plot = ggplot(h_axyridis, aes(x = Doy, y = Probability)) +
  geom_line(color = "blue") +
    labs(title = paste("Predicted Probabilities for", sp, "Over Day of Year (Doy)"),
         x = "Day of Year (Doy)",
         y = "Predicted Probability") +
theme_minimal() +
  geom_point(data = sp_data, aes(x = Doy, y = n_individuals/(max(n_individuals)))) +
  geom_vline(data = sp_data1, aes(xintercept = Doy), linetype = "dashed") 
h_axyridis_plot

plot_filename <- paste0("Data/Pollinator_phenology/", match(sp,spp_order), "_", stringr::str_replace(sp, " ","_"), ".png")
ggsave(filename = plot_filename, plot = h_axyridis_plot, width = 8, height = 6)

#Fix
h_axyridis = h_axyridis %>% 
group_by(Species) %>% 
mutate(Flying_period = if_else(Probability <= 0.15, "No", "Yes"))

fixed_phenologies = h_axyridis
#3Pieris rapae         
#Shows also a bivoltine phenology https://doi.org/10.1038/s41467-023-39359-8
#We can improve it a bit
#Fix with a softer modelling approach
sp = "Pieris rapae"
sp_data = oc2 %>% filter(Species == sp)
sp_data1 = oc3 %>% filter(Species == sp)

sp_model = gam(n_individuals ~ s(Doy, k = 9, m=2),
               family = nb(link = "log"), data = sp_data, method = "REML")
#Create a new data frame with unique Doy values
p_rapae = tibble(Doy = seq(from = 1, to = 365, by = 1))
#Make predictions using the new data frame
predicted_values = predict(sp_model, p_rapae, type = "response")
#Assign the predicted values to the new data frame
p_rapae$Abundances = round(predicted_values)
#Normalize the predicted values to convert to probabilities
max_abundance = max(p_rapae$Abundances)
p_rapae = p_rapae %>%
mutate(Probability = Abundances / max_abundance)
#Add the species name to the data frame
p_rapae$Species = sp
#Plot the predicted probabilities
p_rapae_plot = ggplot(p_rapae, aes(x = Doy, y = Probability)) +
  geom_line(color = "blue") +
    labs(title = paste("Predicted Probabilities for", sp, "Over Day of Year (Doy)"),
         x = "Day of Year (Doy)",
         y = "Predicted Probability") +
theme_minimal() +
  geom_point(data = sp_data, aes(x = Doy, y = n_individuals/(max(n_individuals)))) +
  geom_vline(data = sp_data1, aes(xintercept = Doy), linetype = "dashed") 
p_rapae_plot
plot_filename <- paste0("Data/Pollinator_phenology/", match(sp,spp_order), "_", stringr::str_replace(sp, " ","_"), ".png")
ggsave(filename = plot_filename, plot = p_rapae_plot, width = 8, height = 6)

#Looks a bit better now
#Fix
p_rapae = p_rapae %>% 
group_by(Species) %>% 
mutate(Flying_period = if_else(Probability <= 0.15, "No", "Yes"))
#save and bind rows
fixed_phenologies = bind_rows(fixed_phenologies, p_rapae)

#4Pieris napi        
#Shows also a bivoltine phenology https://doi.org/10.1038/s41467-023-39359-8
#We can improve it a bit, force it to be bimodal, even if the best fit includes three peaks
#Fix with a softer modelling approach
sp = "Pieris napi"
sp_data = oc2 %>% filter(Species == sp)
sp_data1 = oc3 %>% filter(Species == sp)

sp_model = gam(n_individuals ~ s(Doy, k = 16, m=2),
               family = nb(link = "log"), data = sp_data, method = "REML")
#Create a new data frame with unique Doy values
p_napi = tibble(Doy = seq(from = 1, to = 365, by = 1))
#Make predictions using the new data frame
predicted_values = predict(sp_model, p_napi, type = "response")
#Assign the predicted values to the new data frame
p_napi$Abundances = round(predicted_values)
#Normalize the predicted values to convert to probabilities
max_abundance = max(p_napi$Abundances)
p_napi = p_napi %>%
mutate(Probability = Abundances / max_abundance)
#Add the species name to the data frame
p_napi$Species = sp
#Plot the predicted probabilities
p_napi_plot = ggplot(p_napi, aes(x = Doy, y = Probability)) +
  geom_line(color = "blue") +
    labs(title = paste("Predicted Probabilities for", sp, "Over Day of Year (Doy)"),
         x = "Day of Year (Doy)",
         y = "Predicted Probability")  +
theme_minimal() +
  geom_point(data = sp_data, aes(x = Doy, y = n_individuals/(max(n_individuals)))) +
  geom_vline(data = sp_data1, aes(xintercept = Doy), linetype = "dashed") 
p_napi_plot
plot_filename <- paste0("Data/Pollinator_phenology/", match(sp,spp_order), "_", stringr::str_replace(sp, " ","_"), ".png")
ggsave(filename = plot_filename, plot = p_napi_plot, width = 8, height = 6)

#Looks a bit better now
#Fix
p_napi = p_napi %>% 
group_by(Species) %>% 
mutate(Flying_period = if_else(Probability <= 0.15, "No", "Yes"))
#save and bind rows
fixed_phenologies = bind_rows(fixed_phenologies, p_napi)
fixed_phenologies %>%  distinct(Species) %>% pull()

#5 Cydalima perspectalis
#several generations per year https://doi.org/10.1093/jipm/pmac020
#The observed pattern seems right

#6 Polygonia c-album
# bivoltine https://www.monaconatureencyclopedia.com/polygonia-c-album/?lang=en
#Looks good too

#7 Polyommatus icarus   

# 8 Bombus lapidarius    
# Maybe we can improve it a bit
#This should follow more a Guassian than a bivoltine phenology
#Fix with a softer modelling approach
sp = "Bombus lapidarius"
sp_data = oc2 %>% filter(Species == sp)
sp_data1 = oc3 %>% filter(Species == sp)

sp_model = gam(n_individuals ~ s(Doy, k = 5, m=2),
               family = nb(link = "log"), data = sp_data, method = "REML")
#Create a new data frame with unique Doy values
b_lapidarius = tibble(Doy = seq(from = 1, to = 365, by = 1))
#Make predictions using the new data frame
predicted_values = predict(sp_model, b_lapidarius, type = "response")
#Assign the predicted values to the new data frame
b_lapidarius$Abundances = round(predicted_values)
#Normalize the predicted values to convert to probabilities
max_abundance = max(b_lapidarius$Abundances)
b_lapidarius = b_lapidarius %>%
mutate(Probability = Abundances / max_abundance)
#Add the species name to the data frame
b_lapidarius$Species = sp
#Plot the predicted probabilities
b_lapidarius_plot = ggplot(b_lapidarius, aes(x = Doy, y = Probability)) +
  geom_line(color = "blue") +
    labs(title = paste("Predicted Probabilities for", sp, "Over Day of Year (Doy)"),
         x = "Day of Year (Doy)",
         y = "Predicted Probability") +
theme_minimal() +
  geom_point(data = sp_data, aes(x = Doy, y = n_individuals/(max(n_individuals)))) +
  geom_vline(data = sp_data1, aes(xintercept = Doy), linetype = "dashed")

b_lapidarius_plot

plot_filename = paste0("Data/Pollinator_phenology/", match(sp,spp_order), "_", stringr::str_replace(sp, " ","_"), ".png")
ggsave(filename = plot_filename, plot = b_lapidarius_plot, width = 8, height = 6)
#Looks a bit better now
#Fix
b_lapidarius = b_lapidarius %>% 
group_by(Species) %>% 
mutate(Flying_period = if_else(Probability <= 0.15, "No", "Yes"))
#save and bind rows
fixed_phenologies = bind_rows(fixed_phenologies, b_lapidarius)
fixed_phenologies %>%  distinct(Species) %>% pull()

#9 Aricia agestis       
#https://gdoremi.altervista.org/lycaenidae/Aricia_agestis_en.html
#Bivoltine species, maybe improve a bit but doesn't look terrible

#10 Aglais urticae       
#Same is bivoltine and the fit could be further improved.

#11Eristalis pertinax
#Bivoltine species

#12Xylocopa violacea
sp = "Xylocopa violacea"
sp_data = oc2 %>% filter(Species == sp)
sp_data1 = oc3 %>% filter(Species == sp)

sp_model = gam(n_individuals ~ s(Doy, k = 8, m=2),
               family = nb(link = "log"), data = sp_data, method = "REML")
#Create a new data frame with unique Doy values
x_violacea = tibble(Doy = seq(from = 1, to = 365, by = 1))
#Make predictions using the new data frame
predicted_values = predict(sp_model, x_violacea, type = "response")
#Assign the predicted values to the new data frame
x_violacea$Abundances = round(predicted_values)
#Normalize the predicted values to convert to probabilities
max_abundance = max(x_violacea$Abundances)
x_violacea = x_violacea %>%
mutate(Probability = Abundances / max_abundance)
#Add the species name to the data frame
x_violacea$Species = "Xylocopa violacea"
#Plot the predicted probabilities
x_violacea_plot = ggplot(x_violacea, aes(x = Doy, y = Probability)) +
  geom_line(color = "blue") +
    labs(title = paste("Predicted Probabilities for", "Xylocopa violacea", "Over Day of Year (Doy)"),
         x = "Day of Year (Doy)",
         y = "Predicted Probability") +
theme_minimal() +
geom_point(data = sp_data, aes(x = Doy, y = n_individuals/(max(n_individuals)))) +
geom_vline(data = sp_data1, aes(xintercept = Doy), linetype = "dashed") 

x_violacea_plot

plot_filename = paste0("Data/Pollinator_phenology/", match(sp,spp_order), "_", stringr::str_replace(sp, " ","_"), ".png")
ggsave(filename = plot_filename, plot = x_violacea_plot, width = 8, height = 6)

x_violacea = x_violacea %>% 
group_by(Species) %>% 
mutate(Flying_period = if_else(Probability <= 0.15, "No", "Yes"))
#save and bind rows
fixed_phenologies = bind_rows(fixed_phenologies, x_violacea)
fixed_phenologies %>%  distinct(Species) %>% pull()


#13 Tachina fera
#Is a bivoltine species so the observed pattern is completely logical
#https://en.wikipedia.org/wiki/Tachina_fera

#14 Andrena flavipes
#It is also a bivoltine species
#https://bwars.com/index.php/bee/andrenidae/andrena-flavipes

#15Bombus campestris
#It is a cucko species that does not seem to have more than 1 generation per gear
#But their host can be bivoltine so it cannot be discarded
#Cannot improve the model for now
sp = "Bombus campestris"
sp_data = oc2 %>% filter(Species == sp)
sp_data1 = oc3 %>% filter(Species == sp)

sp_model = gam(n_individuals ~ s(Doy, k = 5, m=2),
               family = nb(link = "log"), data = sp_data, method = "REML")
#Create a new data frame with unique Doy values
b_campestris = tibble(Doy = seq(from = 1, to = 365, by = 1))
#Make predictions using the new data frame
predicted_values = predict(sp_model, b_campestris, type = "response")
#Assign the predicted values to the new data frame
b_campestris$Abundances = round(predicted_values)
#Normalize the predicted values to convert to probabilities
max_abundance = max(b_campestris$Abundances)
b_campestris = b_campestris %>%
mutate(Probability = Abundances / max_abundance)
#Add the species name to the data frame
b_campestris$Species = sp
#Plot the predicted probabilities
b_campestris_plot = ggplot(b_campestris, aes(x = Doy, y = Probability)) +
  geom_line(color = "blue") +
    labs(title = paste("Predicted Probabilities for", sp, "Over Day of Year (Doy)"),
         x = "Day of Year (Doy)",
         y = "Predicted Probability") +
theme_minimal() +
geom_point(data = sp_data, aes(x = Doy, y = n_individuals/(max(n_individuals)))) +
geom_vline(data = sp_data1, aes(xintercept = Doy), linetype = "dashed") 

b_campestris_plot

plot_filename = paste0("Data/Pollinator_phenology/", match(sp,spp_order), "_", stringr::str_replace(sp, " ","_"), ".png")
ggsave(filename = plot_filename, plot = b_campestris_plot, width = 8, height = 6)

#Looks a bit better now
#Fix
a_procellaris = a_procellaris %>% 
group_by(Species) %>% 
mutate(Flying_period = if_else(Probability <= 0.15, "No", "Yes"))
#save and bind rows
fixed_phenologies = bind_rows(fixed_phenologies, a_procellaris)
fixed_phenologies %>%  distinct(Species) %>% pull()



#16 Sphecodes albilabris
#Females over winter and search for nest in spring
#Then males and females appear
#https://www.wildbienen.de/wba-kale.htm
#The pattern is correct

#17Andrena bicolor
#Andrena bicolor is a bivoltine species and has two generations per year
#https://www.wildbienen.de/wba-kale.htm
#The pattern is correct

#18Anthomyia procellaris
#doesn't seem to have two generations, maybe soften fit
sp = "Anthomyia procellaris"
sp_data = oc2 %>% filter(Species == sp)
sp_data1 = oc3 %>% filter(Species == sp)

sp_model = gam(n_individuals ~ s(Doy, k = 5, m=2),
               family = nb(link = "log"), data = sp_data, method = "REML")
#Create a new data frame with unique Doy values
a_procellaris = tibble(Doy = seq(from = 1, to = 365, by = 1))
#Make predictions using the new data frame
predicted_values = predict(sp_model, a_procellaris, type = "response")
#Assign the predicted values to the new data frame
a_procellaris$Abundances = round(predicted_values)
#Normalize the predicted values to convert to probabilities
max_abundance = max(a_procellaris$Abundances)
a_procellaris = a_procellaris %>%
mutate(Probability = Abundances / max_abundance)
#Add the species name to the data frame
a_procellaris$Species = sp
#Plot the predicted probabilities
a_procellaris_plot = ggplot(a_procellaris, aes(x = Doy, y = Probability)) +
  geom_line(color = "blue") +
    labs(title = paste("Predicted Probabilities for", sp, "Over Day of Year (Doy)"),
         x = "Day of Year (Doy)",
         y = "Predicted Probability") +
theme_minimal() +
geom_point(data = sp_data, aes(x = Doy, y = n_individuals/(max(n_individuals)))) +
geom_vline(data = sp_data1, aes(xintercept = Doy), linetype = "dashed") 

a_procellaris_plot

plot_filename = paste0("Data/Pollinator_phenology/", match(sp,spp_order), "_", stringr::str_replace(sp, " ","_"), ".png")
ggsave(filename = plot_filename, plot = a_procellaris_plot, width = 8, height = 6)

#Looks a bit better now
#Fix
a_procellaris = a_procellaris %>% 
group_by(Species) %>% 
mutate(Flying_period = if_else(Probability <= 0.15, "No", "Yes"))
#save and bind rows
fixed_phenologies = bind_rows(fixed_phenologies, a_procellaris)
fixed_phenologies %>%  distinct(Species) %>% pull()

#19Timarcha goettingensis
sp = "Timarcha goettingensis"
sp_data = oc2 %>% filter(Species == sp)
sp_data1 = oc3 %>% filter(Species == sp)

sp_model = gam(n_individuals ~ s(Doy, k = 4, m=2),
               family = nb(link = "log"), data = sp_data, method = "REML")
#Create a new data frame with unique Doy values
t_goettingensis = tibble(Doy = seq(from = 1, to = 365, by = 1))
#Make predictions using the new data frame
predicted_values = predict(sp_model, t_goettingensis, type = "response")
#Assign the predicted values to the new data frame
t_goettingensis$Abundances = round(predicted_values)
#Normalize the predicted values to convert to probabilities
max_abundance = max(t_goettingensis$Abundances)
t_goettingensis = t_goettingensis %>%
mutate(Probability = Abundances / max_abundance)
#Add the species name to the data frame
t_goettingensis$Species = sp
#Plot the predicted probabilities
t_goettingensis_plot = ggplot(t_goettingensis, aes(x = Doy, y = Probability)) +
  geom_line(color = "blue") +
    labs(title = paste("Predicted Probabilities for", "Timarcha goettingensis", "Over Day of Year (Doy)"),
         x = "Day of Year (Doy)",
         y = "Predicted Probability") +
theme_minimal() +
geom_point(data = sp_data, aes(x = Doy, y = n_individuals/(max(n_individuals)))) +
geom_vline(data = sp_data1, aes(xintercept = Doy), linetype = "dashed") 

t_goettingensis_plot

plot_filename = paste0("Data/Pollinator_phenology/", match(sp,spp_order), "_", stringr::str_replace(sp, " ","_"), ".png")
ggsave(filename = plot_filename, plot = t_goettingensis_plot, width = 8, height = 6)

#Looks a bit better now
#Fix
t_goettingensis = t_goettingensis %>% 
group_by(Species) %>% 
mutate(Flying_period = if_else(Probability <= 0.15, "No", "Yes"))
#save and bind rows
fixed_phenologies = bind_rows(fixed_phenologies, t_goettingensis)
fixed_phenologies %>%  distinct(Species) %>% pull()

#20 Meliscaeva auricollis
#Fit again as is not dtecting a smooth pattern with ecological sense
sp = "Meliscaeva auricollis"
sp_data = oc2 %>% filter(Species == sp)
sp_data1 = oc3 %>% filter(Species == sp)

sp_model = gam(n_individuals ~ s(Doy, k = 8, m=2),
               family = nb(link = "log"), data = sp_data, method = "REML")
#Create a new data frame with unique Doy values
m_auricollis = tibble(Doy = seq(from = 1, to = 365, by = 1))
#Make predictions using the new data frame
predicted_values = predict(sp_model, m_auricollis, type = "response")
#Assign the predicted values to the new data frame
m_auricollis$Abundances = round(predicted_values)
#Normalize the predicted values to convert to probabilities
max_abundance = max(m_auricollis$Abundances)
m_auricollis = m_auricollis %>%
mutate(Probability = Abundances / max_abundance)
#Add the species name to the data frame
m_auricollis$Species = sp
#Plot the predicted probabilities
m_auricollis_plot = ggplot(m_auricollis, aes(x = Doy, y = Probability)) +
  geom_line(color = "blue") +
    labs(title = paste("Predicted Probabilities for", sp, "Over Day of Year (Doy)"),
         x = "Day of Year (Doy)",
         y = "Predicted Probability") +
theme_minimal() +
geom_point(data = sp_data, aes(x = Doy, y = n_individuals/(max(n_individuals)))) +
geom_vline(data = sp_data1, aes(xintercept = Doy), linetype = "dashed") 

m_auricollis_plot

plot_filename = paste0("Data/Pollinator_phenology/", match(sp,spp_order), "_", stringr::str_replace(sp, " ","_"), ".png")
ggsave(filename = plot_filename, plot = m_auricollis_plot, width = 8, height = 6)

#Looks a bit better now
#Fix, in this case we set a less generous tresshold, to keep a narrow phenology
#m_auricollis = m_auricollis %>% 
#group_by(Species) %>% 
#mutate(Flying_period = if_else(Probability <= 0.35, "No", "Yes"))
##save and bind rows
#fixed_phenologies = bind_rows(fixed_phenologies, m_auricollis)
#fixed_phenologies %>%  distinct(Species) %>% pull()

##21 Minettia longipennis
sp = "Minettia longipennis"
sp_data = oc2 %>% filter(Species == sp)
sp_data1 = oc3 %>% filter(Species == sp)

sp_model = gam(n_individuals ~ s(Doy, k = 4, m=2),
               family = nb(link = "log"), data = sp_data, method = "REML")
#Create a new data frame with unique Doy values
m_longipennis = tibble(Doy = seq(from = 1, to = 365, by = 1))
#Make predictions using the new data frame
predicted_values = predict(sp_model, m_longipennis, type = "response")
#Assign the predicted values to the new data frame
m_longipennis$Abundances = round(predicted_values)
#Normalize the predicted values to convert to probabilities
max_abundance = max(m_longipennis$Abundances)
m_longipennis = m_longipennis %>%
mutate(Probability = Abundances / max_abundance)
#Add the species name to the data frame
m_longipennis$Species = sp
#Plot the predicted probabilities
m_longipennis_plot = ggplot(m_longipennis, aes(x = Doy, y = Probability)) +
  geom_line(color = "blue") +
    labs(title = paste("Predicted Probabilities for", sp, "Over Day of Year (Doy)"),
         x = "Day of Year (Doy)",
         y = "Predicted Probability") +
theme_minimal() +
geom_point(data = sp_data, aes(x = Doy, y = n_individuals/(max(n_individuals)))) +
geom_vline(data = sp_data1, aes(xintercept = Doy), linetype = "dashed") 

m_longipennis_plot

plot_filename = paste0("Data/Pollinator_phenology/", match(sp,spp_order), "_", stringr::str_replace(sp, " ","_"), ".png")
ggsave(filename = plot_filename, plot = m_longipennis_plot, width = 8, height = 6)

#Looks a bit better now
#Fix, in this case we set a less generous tresshold, to keep a narrow phenology
m_longipennis = m_longipennis %>% 
group_by(Species) %>% 
mutate(Flying_period = if_else(Probability <= 0.35, "No", "Yes"))
#save and bind rows
fixed_phenologies = bind_rows(fixed_phenologies, m_longipennis)
fixed_phenologies %>%  distinct(Species) %>% pull()

#22Nomada zonata
#It is a bivoltine species
#https://www.wildbienen.de/eb-nzona.htm
sp = "Nomada zonata"
sp_data = oc2 %>% filter(Species == sp)
sp_data1 = oc3 %>% filter(Species == sp)

sp_model = gam(n_individuals ~ s(Doy, k = 5, m=1),
               family = nb(link = "log"), data = sp_data, method = "REML")
#Create a new data frame with unique Doy values
n_zonata = tibble(Doy = seq(from = 1, to = 365, by = 1))
#Make predictions using the new data frame
predicted_values = predict(sp_model, n_zonata, type = "response")
#Assign the predicted values to the new data frame
n_zonata$Abundances = round(predicted_values)
#Normalize the predicted values to convert to probabilities
max_abundance = max(n_zonata$Abundances)
n_zonata = n_zonata %>%
mutate(Probability = Abundances / max_abundance)
#Add the species name to the data frame
n_zonata$Species = sp
#Plot the predicted probabilities
n_zonata_plot = ggplot(n_zonata, aes(x = Doy, y = Probability)) +
  geom_line(color = "blue") +
    labs(title = paste("Predicted Probabilities for", sp, "Over Day of Year (Doy)"),
         x = "Day of Year (Doy)",
         y = "Predicted Probability") +
theme_minimal() +
geom_point(data = sp_data, aes(x = Doy, y = n_individuals/(max(n_individuals)))) +
geom_vline(data = sp_data1, aes(xintercept = Doy), linetype = "dashed") 

n_zonata_plot

plot_filename = paste0("Data/Pollinator_phenology/", match(sp,spp_order), "_", stringr::str_replace(sp, " ","_"), ".png")
ggsave(filename = plot_filename, plot = n_zonata_plot, width = 8, height = 6)

#Looks a bit better now
#Fix, in this case we set a less generous tresshold, to keep a narrow phenology
n_zonata = n_zonata %>% 
group_by(Species) %>% 
mutate(Flying_period = if_else(Probability <= 0.35, "No", "Yes"))
#save and bind rows
fixed_phenologies = bind_rows(fixed_phenologies, n_zonata)
fixed_phenologies %>%  distinct(Species) %>% pull()

#23 Eristalinus aeneus
sp = "Eristalinus aeneus"
sp_data = oc2 %>% filter(Species == sp)
sp_data1 = oc3 %>% filter(Species == sp)

sp_model = gam(n_individuals ~ s(Doy, k = 4, m=2),
               family = nb(link = "log"), data = sp_data, method = "REML")
#Create a new data frame with unique Doy values
e_aeneus = tibble(Doy = seq(from = 1, to = 365, by = 1))
#Make predictions using the new data frame
predicted_values = predict(sp_model, e_aeneus, type = "response")
#Assign the predicted values to the new data frame
e_aeneus$Abundances = round(predicted_values)
#Normalize the predicted values to convert to probabilities
max_abundance = max(e_aeneus$Abundances)
e_aeneus = e_aeneus %>%
mutate(Probability = Abundances / max_abundance)
#Add the species name to the data frame
e_aeneus$Species = sp
#Plot the predicted probabilities
e_aeneus_plot = ggplot(e_aeneus, aes(x = Doy, y = Probability)) +
  geom_line(color = "blue") +
    labs(title = paste("Predicted Probabilities for", sp, "Over Day of Year (Doy)"),
         x = "Day of Year (Doy)",
         y = "Predicted Probability") +
theme_minimal() +
geom_point(data = sp_data, aes(x = Doy, y = n_individuals/(max(n_individuals)))) +
geom_vline(data = sp_data1, aes(xintercept = Doy), linetype = "dashed") 

e_aeneus_plot

plot_filename = paste0("Data/Pollinator_phenology/", match(sp,spp_order), "_", stringr::str_replace(sp, " ","_"), ".png")
ggsave(filename = plot_filename, plot = e_aeneus_plot, width = 8, height = 6)

#Looks a bit better now
#Fix, in this case we set a less generous tresshold, to keep a narrow phenology
e_aeneus = e_aeneus %>% 
group_by(Species) %>% 
mutate(Flying_period = if_else(Probability <= 0.35, "No", "Yes"))
#save and bind rows
fixed_phenologies = bind_rows(fixed_phenologies, e_aeneus)
fixed_phenologies %>%  distinct(Species) %>% pull()


#24 Lasioglossum calceatum
#Try with more data, not really working
#Bivoltine species

# 25 Bombus bohemicus
sp = "Bombus bohemicus"
sp_data = oc2 %>% filter(Species == sp)
sp_data1 = oc3 %>% filter(Species == sp)

#Filter out values equal to 0 lower under minimum and over than maximum
#We do this as is not a bivoltine species
sp_data = sp_data %>% 
filter(!(n_individuals == 0 & Doy > min(sp_data$Doy[sp_data$n_individuals > 0]) & 
           Doy < max(sp_data$Doy[sp_data$n_individuals > 0])))

sp_data1 = oc3 %>% filter(Species == sp)

sp_model = gam(n_individuals ~ s(Doy, k = 5, m=2),
               family = nb(link = "log"), data = sp_data, method = "REML")
#Create a new data frame with unique Doy values
e_aeneus = tibble(Doy = seq(from = 1, to = 365, by = 1))
#Make predictions using the new data frame
predicted_values = predict(sp_model, e_aeneus, type = "response")
#Assign the predicted values to the new data frame
e_aeneus$Abundances = round(predicted_values)
#Normalize the predicted values to convert to probabilities
max_abundance = max(e_aeneus$Abundances)
e_aeneus = e_aeneus %>%
mutate(Probability = Abundances / max_abundance)
#Add the species name to the data frame
e_aeneus$Species = sp
#Plot the predicted probabilities
e_aeneus_plot = ggplot(e_aeneus, aes(x = Doy, y = Probability)) +
  geom_line(color = "blue") +
    labs(title = paste("Predicted Probabilities for", sp, "Over Day of Year (Doy)"),
         x = "Day of Year (Doy)",
         y = "Predicted Probability") +
theme_minimal() +
geom_point(data = sp_data, aes(x = Doy, y = n_individuals/(max(n_individuals)))) +
geom_vline(data = sp_data1, aes(xintercept = Doy), linetype = "dashed") 

e_aeneus_plot

plot_filename = paste0("Data/Pollinator_phenology/", match(sp,spp_order), "_", stringr::str_replace(sp, " ","_"), ".png")
ggsave(filename = plot_filename, plot = e_aeneus_plot, width = 8, height = 6)

#Looks a bit better now
#Fix, in this case we set a less generous tresshold, to keep a narrow phenology
e_aeneus = e_aeneus %>% 
group_by(Species) %>% 
mutate(Flying_period = if_else(Probability <= 0.35, "No", "Yes"))
#save and bind rows
fixed_phenologies = bind_rows(fixed_phenologies, e_aeneus)
fixed_phenologies %>%  distinct(Species) %>% pull()





# 26 Lasioglossum morio


#Now check the generated plots in the folder one by one
#Create vector of plots that are not creating a nice phenology
#and re-rerun GAM with all occurrences! Script 3_2
to_fix = c(44, 56, 58, 63, 64, 68, 78, 86, 87, 
           101, 102,  117, 120, 121, 124, 128, 
           131, 132, 133)

#Include also species that did not have sufficient records
spp_order_raw = spp_order_raw %>% pull(Species)
until_max = seq(from = max(to_fix), to = length(spp_order_raw), by = 1)
to_fix1 = c(to_fix, until_max)

spp_missing_phenology = spp_order_raw[to_fix1]
#save species with missing phenology
saveRDS(spp_missing_phenology, "Data/Working_files/pollinator_spp_with_missing_phenology.rds")



p_c$Species


# Check fit of some models
sp_data = oc1 %>% filter(Species == spp_order[4])
sp_model = gam(n_individuals ~ s(Doy, k = 12, m=2),
               family = nb(link = "log"), data = sp_data, method = "REML")
qq.gam(sp_model, main = "QQ plot of residuals")

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
unique_dates$Species = spp_order[4]
#Plot the predicted probabilities
ggplot(unique_dates, aes(x = Doy, y = Probability)) +
  geom_line(color = "blue") +
    labs(title = paste("Predicted Probabilities for", spp_order[4], "Over Day of Year (Doy)"),
         x = "Day of Year (Doy)",
         y = "Predicted Probability") +
    theme_minimal()


