#Check distributios of plant and pollinator phenology
library(dplyr)
library(ggplot2)
library(scales)
#Read pollinator phenology 
poll_phen = readRDS("Data/Working_files/pollinator_phenology.rds")
#Count number of flying days for each pollinator
flying_days = poll_phen %>%
filter(Flying_period == "Yes") %>%
group_by(Species) %>%
summarise(Flying_days = n()) %>% 
arrange(-Flying_days) %>% 
rename(Pollinator = Species)
#plot
flying_days %>% 
ggplot(aes(Flying_days)) +
geom_histogram()

flying_days <- flying_days %>%
  mutate(Relative_Flying_days = Flying_days / 100)

#Read plant phenology for the 3 gardens
halle_phen = readRDS("Data/Working_files/halle_plant_phenology_predictions.rds")
jena_phen = readRDS("Data/Working_files/jena_plant_phenology_predictions.rds")
leipzig_phen = readRDS("Data/Working_files/leipzig_plant_phenology_predictions.rds")

plant_phen = bind_rows(halle_phen, jena_phen, leipzig_phen)

#Count number of flowering days for each pollinator
flowering_days = plant_phen %>%
filter(Flowering_period == "Yes") %>%
group_by(Species) %>%
summarise(Flowering_days = n()) %>% 
arrange(-Flowering_days) %>% 
rename(Plant = Species)
#plot
flowering_days %>% 
ggplot(aes(Flowering_days)) +
geom_histogram()

#Combine both graphs, maybe with a density one!
# Create a combined data frame
flying_days <- flying_days %>% mutate(Type = "Flying Days")
flowering_days <- flowering_days %>% mutate(Type = "Flowering Days")

# Combine the datasets
combined_days <- bind_rows(
  flying_days %>% rename(Days = Flying_days),
  flowering_days %>% rename(Days = Flowering_days)
)

# Plot the combined density plot
show_col(viridis_pal()(20))
# Create a combined data frame
flying_days = flying_days %>% mutate(Type = "Pollinator flying")
flowering_days = flowering_days %>% mutate(Type = "Plant flowering")
# Combine the datasets
combined_days = bind_rows(
  flying_days %>% rename(Days = Flying_days),
  flowering_days %>% rename(Days = Flowering_days))

#Get average values
#for poll
average_flying_days = flying_days %>% 
ungroup() %>% 
summarise(mean_flying_days = mean(Flying_days))
#for plants
average_flowering_days = flowering_days %>% 
ungroup() %>% 
summarise(mean_flowering_days = mean(Flowering_days))
#create tibble with averages
average_values = tibble(mean_value = c(average_flying_days$mean_flying_days, 
                      average_flowering_days$mean_flowering_days),
       Type = c("Pollinator flying", "Plant flowering"))
# Plot 
ggplot(combined_days, aes(x = Days, fill = Type)) +
geom_histogram(position = "identity", alpha = 0.9, binwidth = 5, color = "black") +
scale_fill_manual(name = "Phenology", values = c("#481568FF", "#56C667FF")) +
xlab("Activity period (Number of days)") +
ylab("Count") +
facet_grid(Type ~ .) +  # Facet grid to create separate panels for each type
theme_bw() +
theme(legend.position = c(0.85, 0.85),  # Position the legend inside the plot
      legend.background = element_rect(fill = "white", size = 4),
      legend.title = element_text(size = 10),
      legend.text = element_text(size = 9)) +
geom_vline(data = average_values, aes(xintercept = mean_value), linetype = "dashed") +
coord_cartesian(expand = FALSE) +
theme(
strip.background = element_blank(),
strip.text = element_blank()) +
scale_y_continuous(breaks = seq(0, max(combined_days$Days, na.rm = TRUE), by = 5)) 


#Now get number of pollinators flying through time
poll_phen1 = poll_phen %>% 
filter(Flying_period == "Yes") %>% 
group_by(Doy) %>% 
summarise(Active = length(Flying_period)) %>% 
select(Doy, Active) %>% 
mutate(Type = "Pollinators")

plant_phen1 = plant_phen %>% 
filter(Flowering_period == "Yes") %>% 
group_by(Doy) %>% 
summarise(Active = length(Flowering_period)) %>% 
select(Doy, Active) %>% 
mutate(Type = "Plants")

all1 = bind_rows(poll_phen1, plant_phen1)


ggplot(all1, aes(x = Doy, y = Active, fill = Type)) + 
geom_area(stat = "smooth", method = "gam", aes(group = Type),
          method.args = list(family = poisson),
          alpha = 0.75,
          color = NA) +
scale_fill_manual(name = "Type", values = c("Pollinators" = "#481568FF", "Plants" = "#56C667FF")) +
facet_grid(Type ~ .) +
xlab("Day of the year") +
ylab("Number of species") +
coord_cartesian(expand = FALSE) +
theme_minimal() +
theme(
strip.background = element_blank(),
strip.text = element_blank()) 


# Plot with smoothed lines and bars for both pollinators and plants
ggplot(all1, aes(x = Doy, y = Active, fill = Type)) + 
  geom_bar(stat = "identity", position = "identity", alpha = 0.5)  +
  scale_fill_manual(name = "Type", values = c("Pollinators" = "#481568FF", "Plants" = "#56C667FF")) +
  facet_grid(Type ~ ., scales = "free_y") +  # Facet by Type with free y-axis scales
  theme_minimal() +
  theme(strip.background = element_blank(),  # Remove facet strip background
        strip.text = element_blank()) +
  coord_cartesian(expand = FALSE)  +
xlab("Day of the year") +
ylab("Number of species") 


