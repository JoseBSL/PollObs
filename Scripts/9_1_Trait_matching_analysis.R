# Script to calculate interaction probability based on trait-matching

#Load libraries
library(dplyr)
#Load pollinator trait data
poll_traits = readRDS("Data/Trait_data/Processed/PollTraits.rds")
poll_width = poll_traits %>% 
  select(Pollinator, IT_mm) %>% 
  group_by(Pollinator) %>% 
  summarise(Average_IT_mm = mean(IT_mm, na.rm=T))
#Load plant trait data
plant_traits = read_csv("Data/Trait_data/Raw/ReproductiveTraits_Morphometrics.csv")
colnames(plant_traits)
#From plant traits 
#Select for now tube width/flower width-> Try with both
tube_width = plant_traits %>% 
  select(Species, Flower_width, Floral_tube_width) %>% 
  group_by(Species) %>% 
  summarise(Average_tube_width = mean(Floral_tube_width, na.rm=T),
            Average_flower_width = mean(Flower_width, na.rm=T))
#Create long format tibble with all combinations
species_combinations = expand.grid(Poll_Species = poll_width$Pollinator, Plant_Species = tube_width$Species)
#Add average values of poll and plant traits
species_combinations = species_combinations %>%
  left_join(poll_width, by = c("Poll_Species" = "Pollinator")) %>%
  left_join(tube_width, by = c("Plant_Species" = "Species")) %>%
  mutate(Poll_size = Average_IT_mm, 
         Tube_size = Average_tube_width,
         Flower_size = Average_flower_width) %>%
  select(-Average_IT_mm, -Average_tube_width, -Average_flower_width)  


#Function to compute the logarithmic fraction distance
log_fraction <- function(a, b) {
  log(a / b)
}

#Calculate a ratio and log it
#Explore it with a probabilistic gaussian function
species_combinations1 = species_combinations %>% 
  mutate(Log_distance_poll_tube =  log(Poll_size / Tube_size)) %>% 
  mutate(Log_distance_poll_flower =  log(Poll_size / Flower_size)) %>% 
  mutate(Gaussian_niche_poll_tube =  exp(- (Log_distance_poll_tube^2) / (2 * 0.2^2))) %>% 
  mutate(Gaussian_niche_poll_flower =  exp(- (Log_distance_poll_flower^2) / (2 * 0.2^2)))

#Visualise distributions
species_combinations1 %>% 
  ggplot(aes(Log_distance_poll_tube)) +
  geom_histogram()

species_combinations1 %>% 
  ggplot(aes(Gaussian_niche_poll_tube)) +
  geom_histogram()


species_combinations1 %>% 
  ggplot(aes(Log_distance_poll_tube, Gaussian_niche_poll_tube)) +
  geom_line()

species_combinations1 %>% 
  ggplot(aes(Log_distance_poll_flower)) +
  geom_histogram()

species_combinations1 %>% 
  ggplot(aes(Gaussian_niche_poll_flower)) +
  geom_histogram()

#Load interactions
interactions = readRDS("Data/Working_files/interaction_data.rds")
interactions1 = interactions %>% 
  select(Pollinator_accepted_name, Interactions, 
         Plant_accepted_name, Botanical_garden, Date,
         Pollinator_rank) %>% 
  rename(Poll_Species = Pollinator_accepted_name) %>% 
  rename(Plant_Species = Plant_accepted_name)


leipzig_interactions = interactions1 %>% 
filter(Botanical_garden == "Leipzig") %>%
filter(Interactions > 0) %>%
filter(Pollinator_rank == "SPECIES") %>% 
select(Poll_Species, Plant_Species) %>% 
mutate(Presence = 1) %>% 
distinct()


library(tidyr)
leipzig_matrix = leipzig_interactions %>% 
pivot_wider(names_from = Plant_Species,
            values_from = Presence,
            values_fill = 0) %>% 
column_to_rownames("Poll_Species")


# Convert back to long format
leipzig_long = leipzig_matrix %>%
  pivot_longer(
    cols = -Poll_Species,  # Preserve the Poll_Species column
    names_to = "Plant_Species",  # New column for Plant Species
    values_to = "Presence"  # New column for Presence (1 or 0)
  )





#Join with interaction data
d = left_join(interactions1, species_combinations1, 
              by=c("Poll_Species", "Plant_Species"))

colnames(d)

#Model interactions

# Filter original data to remove rows with missing values for model fitting
d_filtered <- d %>% filter(!is.na(Interactions) & !is.na(Gaussian_niche))
d_filtered_Leipzig = d_filtered %>% 
  filter(Botanical_garden == "Jena")



model = lm(log(Interactions +1) ~ Gaussian_niche, data = d_filtered_Leipzig)
summary(model)
# Create predictions for the fitted line
d_filtered_Leipzig$Fitted <- predict(model, newdata = d_filtered_Leipzig)

# Plot the data and the fitted regression line
ggplot(d_filtered_Leipzig, aes(x = Gaussian_niche, y = log(Interactions +1))) +
  geom_point(alpha = 0.6) +  # Plot observed data points
  geom_line(aes(y = Fitted), color = "blue", size = 1) +  # Plot the fitted line
  labs(title = "Trend of Interactions vs. Gaussian Niche",
       x = "Gaussian Niche Values",
       y = "Number of Interactions") +
  theme_minimal()

