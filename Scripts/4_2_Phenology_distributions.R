#Check distributios of plant and pollinator phenology

library(dplyr)
library(ggplot2)
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
library(scales)
show_col(viridis_pal()(20))
ggplot(combined_days, aes(x = Days, fill = Type, color = Type)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(name = "Phenology", values =  c("#481568FF", "#56C667FF")) +
  scale_color_manual(name = "Phenology",values = c("#481568FF", "#56C667FF")) +
xlab("Day of the year") +
ylab("Density") +
  theme_minimal()
