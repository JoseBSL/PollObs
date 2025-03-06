# Script to calculate interaction probability based on trait-matching

library(dplyr)


#Load pollinator trait data
poll_traits = readRDS("Data/Trait_data/Processed/PollTraits.rds")
poll_width = poll_traits %>% 
select(Pollinator, IT_mm) %>% 
group_by(Pollinator) %>% 
summarise(Average_IT_mm = mean(IT_mm, na.rm=T))


#Load plant trait data
plant_traits = read_csv("Data/Trait_data/Raw/ReproductiveTraits_Morphometrics.csv")
#From plant traits 
#Select for now tube width
tube_width = plant_traits %>% 
select(Species, Floral_tube_width) %>% 
group_by(Species) %>% 
summarise(Average_tube_width = mean(Floral_tube_width, na.rm=T))

#Create long format tibble with all combinations
species_combinations = expand.grid(Poll_Species = poll_width$Pollinator, Plant_Species = tube_width$Species)

# Add average values of poll and plant traits
species_combinations = species_combinations %>%
  left_join(poll_width, by = c("Poll_Species" = "Pollinator")) %>%
  left_join(tube_width, by = c("Plant_Species" = "Species")) %>%
  mutate(Poll_size = Average_IT_mm, 
         Tube_size = Average_tube_width) %>%
  select(-Average_IT_mm, -Average_tube_width)  


# Function to compute the logarithmic fraction distance
log_fraction <- function(a, b) {
  log(a / b)
}

#Calculate a ratio and log it
#Explore it with a probabilistic gaussian function
species_combinations1 = species_combinations %>% 
mutate(Log_distance =  log(Poll_size / Tube_size)) %>% 
mutate(Gaussian_niche =  exp(- (Log_distance^2) / (2 * 0.2^2)))

#Visualise distributions
species_combinations1 %>% 
ggplot(aes(Log_distance)) +
geom_histogram()

species_combinations1 %>% 
ggplot(aes(Gaussian_niche)) +
geom_histogram()

#Load interactions
interactions = readRDS("Data/Working_files/interaction_data.rds")
interactions1 = interactions %>% 
select(Pollinator_accepted_name, Interactions, Plant_accepted_name, Botanical_garden) %>% 
rename(Poll_Species = Pollinator_accepted_name) %>% 
rename(Plant_Species = Plant_accepted_name)

#Join with interaction data
d = left_join(interactions1, species_combinations1, 
          by=c("Poll_Species", "Plant_Species"))

colnames(d)

#Model interactions

# Filter original data to remove rows with missing values for model fitting
d_filtered <- d %>% filter(!is.na(Interactions) & !is.na(Gaussian_niche))

model = lm(log(Interactions +1) ~ Gaussian_niche, data = d)
summary(model)
# Create predictions for the fitted line
d_filtered$Fitted <- predict(model, newdata = d_filtered)

# Plot the data and the fitted regression line
ggplot(d_filtered, aes(x = Gaussian_niche, y = log(Interactions +1))) +
  geom_point(alpha = 0.6) +  # Plot observed data points
  geom_line(aes(y = Fitted), color = "blue", size = 1) +  # Plot the fitted line
  labs(title = "Trend of Interactions vs. Gaussian Niche",
       x = "Gaussian Niche Values",
       y = "Number of Interactions") +
  theme_minimal()

