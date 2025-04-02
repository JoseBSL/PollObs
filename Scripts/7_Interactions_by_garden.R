##Calculate % of pollinators that are exclusive to PhenObs plants
library(dplyr)
library(readr)
library(stringr)
library(ggplot2)
#Load data
raw_data = readRDS("Data/Working_files/interaction_data.rds")
interaction_data = raw_data  %>% filter(!is.na(Interactions),
                                         !is.na(Floral_abundance),
                                         Pollinator != "None") %>% 
  select(Botanical_garden, Plant_accepted_name, 
         Plant,
         Pollinator_accepted_name, Date_time, 
         Interactions, Floral_abundance,
         Pollinator_rank) %>% 
  rename(Plant = Plant, Pollinator = Pollinator_accepted_name) %>% filter(!is.na(Pollinator))

phenobs_spp = read_csv("Data/Trait_data/Raw/ReproductiveTraits_Morphometrics.csv") %>% 
  select(Species) %>% 
  distinct() %>% 
  pull()
phenobs_spp = str_replace(phenobs_spp, "Persicaria bistorta", "Polygonum bistorta")
phenobs_spp = str_replace(phenobs_spp, "Aquilegia chrysantha", "Aquilegia vulgaris")


poll_total = interaction_data %>% 
  filter(Pollinator_rank == "SPECIES") %>% 
  distinct(Pollinator) %>% 
  pull(Pollinator)

poll_phenobs = interaction_data %>% 
  filter(Plant %in% phenobs_spp) %>% 
  filter(Pollinator_rank == "SPECIES") %>% 
  distinct(Pollinator) %>% 
  pull(Pollinator)

poll_garden = interaction_data %>% 
  filter(!Plant %in% phenobs_spp) %>% 
  filter(Pollinator_rank == "SPECIES") %>% 
  distinct(Pollinator) %>% 
  pull(Pollinator)

#Count the pollinators in each category
n_shared = sum(poll_phenobs %in% poll_garden)
n_phenobs_only = sum(!poll_phenobs %in% poll_garden)
n_garden_only = sum(!poll_garden %in% poll_phenobs)

#Create a tibble for plotting
pollinator_counts = tibble(
  Category = c("Phenobs Only", "Garden Only", "Shared"),
  Count = c(n_phenobs_only, n_garden_only, n_shared))

#Safety check
sum(pollinator_counts$Count) == length(poll_total)

# Plot the data
ggplot(pollinator_counts, aes(x = "Pollinators", y = Count, fill = Category)) +
  geom_bar(stat = "identity", width = 0.5) +
  theme_minimal() +
  labs(title = "Pollinator Overlap Between Phenobs and Garden",
       x = "",
       y = "Number of Pollinators") +
  scale_fill_manual(values = c("Phenobs Only" = "purple", "Garden Only" = "cyan4", "Shared" = "gray14")) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

#Group by Botanical Garden and compute pollinator counts

#Function to compute pollinator counts per garden
get_pollinator_counts <- function(garden) {
  
poll_total = interaction_data %>% 
  filter(Pollinator_rank == "SPECIES") %>% 
  filter(Botanical_garden == garden) %>% 
  distinct(Pollinator) %>% 
  pull(Pollinator)

poll_phenobs = interaction_data %>% 
  filter(Plant %in% phenobs_spp) %>% 
  filter(Pollinator_rank == "SPECIES") %>% 
  filter(Botanical_garden == garden) %>% 
  distinct(Pollinator) %>% 
  pull(Pollinator)

poll_garden = interaction_data %>% 
  filter(!Plant %in% phenobs_spp) %>% 
  filter(Pollinator_rank == "SPECIES") %>% 
  filter(Botanical_garden == garden) %>% 
  distinct(Pollinator) %>% 
  pull(Pollinator)


# Count the pollinators in each category
n_shared = sum(poll_phenobs %in% poll_garden)
n_phenobs_only = sum(!poll_phenobs %in% poll_garden)
n_garden_only = sum(!poll_garden %in% poll_phenobs)

#Create a tibble for plotting
pollinator_counts = tibble(
  Category = c("PhenObs", "Garden", "Shared"),
  Count = c(n_phenobs_only, n_garden_only, n_shared),
  Garden = garden
)
}

#Run function for each garden
h = get_pollinator_counts("Halle")
j = get_pollinator_counts("Jena")
l = get_pollinator_counts("Leipzig")
#Create unique tibble for plotting
d = bind_rows(h, j, l)

#Plot the data
ggplot(d, aes(x = Garden, y = Count, fill = Category), group = Garden) +
  geom_bar(stat = "identity", width = 0.5) +
  theme_minimal() +
  labs(title = "Pollinator richness by sampling groups",
       x = "",
       y = "Number of Pollinators") +
  scale_fill_manual(name = NULL, values = c("PhenObs" = "purple", "Garden" = "cyan4", "Shared" = "gray14")) +
  theme(axis.ticks.x = element_blank())






