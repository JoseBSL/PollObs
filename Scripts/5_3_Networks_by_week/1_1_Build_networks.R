# ======================================================
#Script: Prepare plant-poll networks (visits and visitation rate)
# ======================================================

# Load libraries
library(dplyr)
library(purrr)
library(tidyr)

# ======================================================
# Load data
# Interaction data
raw_data = readRDS("Data/Working_files/interaction_data.rds")
# Vector of phenobs spp
phenobs_spp = readRDS("Data/Working_files/phenobs_spp.rds") #To filter int data

# ======================================================
# Create tibble with unique dates 
dates = raw_data %>% 
  select(Botanical_garden, Date) %>% 
  group_by(Botanical_garden) %>% 
  distinct()

weeks_2023 = tibble(Doy = 1:365) %>%
  mutate(Date = as.Date(Doy - 0, origin = "2023-01-01"),
         Sampling_week = isoweek(Date))

dates = left_join(dates, weeks_2023)

# Prepare interaction data
# Filter by main poll orders
poll_order = c("Hymenoptera", "Diptera", "Coleoptera", "Lepidoptera")

interaction_data = raw_data %>%
  filter(!is.na(Interactions),
         !is.na(Floral_abundance),
         Pollinator != "None") %>%
  rename(Plants = Plant_accepted_name,
         Pollinators = Pollinator_accepted_name) %>%
  filter(!is.na(Pollinators)) %>% 
  filter(Pollinator_rank == "SPECIES") %>% 
  filter(Sampling == "Focal") %>% 
  # filter(!Pollinators == "Apis mellifera") %>% 
  filter(Pollinator_order %in% poll_order) %>% 
  filter(!Plants == "Iberis sempervirens") %>% 
  filter(Plant %in% phenobs_spp) 

#Add season category to the data (early-mid and late season)
interaction_data = left_join(interaction_data, dates)

# ======================================================
# 1) Convert to network (visitation networks)
to_network = function(data) {
  data %>%
    group_by(Plants, Pollinators) %>%
    summarise(Total_interactions = sum(Interactions),
              .groups = "drop") %>%
    pivot_wider(names_from = Pollinators, 
                values_from = Total_interactions, values_fill = 0) %>%
    column_to_rownames("Plants") %>%
    as.matrix()
}

#Prepare networks per garden
networks_by_garden_and_week = interaction_data %>%
  group_by(Botanical_garden, Sampling_week) %>%
  nest() %>%
  mutate(Interaction_network = map(data, to_network)) %>%
  select(!data) %>% 
  ungroup()


# ======================================================
# 2) Interaction frequency per garden and season
interaction_frequency = interaction_data %>% 
  group_by(Botanical_garden, Sampling_week, Plants, Pollinators) %>%
  summarise(Interaction_total = sum(Interactions), .groups = "drop")

# Total observation time per species per garden and season
total_time_species = interaction_data %>%
  select(Botanical_garden, Sampling_week, Plants, Date, Total_time_species) %>% 
  group_by(Botanical_garden, Sampling_week, Plants) %>%
  distinct() %>% 
  summarise(Total_time_species = sum(Total_time_species, na.rm = TRUE), .groups = "drop")

# Join total time into interaction frequency data
interaction_data_freq <- interaction_frequency %>%
  left_join(total_time_species, by = c("Botanical_garden", "Sampling_week", "Plants")) %>%
  mutate(Freq = Interaction_total / Total_time_species)

# Frequency network function (same as before)
to_freq_network <- function(data) {
  data %>%
    group_by(Plants, Pollinators) %>%
    summarise(Total_frequency = sum(Freq), .groups = "drop") %>%
    pivot_wider(names_from = Pollinators, 
                values_from = Total_frequency, values_fill = 0) %>%
    column_to_rownames("Plants") %>%
    as.matrix()
}

# Prepare frequency networks per garden and season
networks_by_garden_week_freq = interaction_data_freq %>%
  group_by(Botanical_garden, Sampling_week) %>%
  nest() %>%
  mutate(Int_frequency_network = map(data, to_freq_network)) %>%
  select(-data) %>%
  ungroup()

# Join interaction totals and frequencies into one tibble
net_by_garden_week <- left_join(
  networks_by_garden_and_week,  # from your earlier step
  networks_by_garden_week_freq,
  by = c("Botanical_garden", "Sampling_week")
)

# ======================================================
# Save result
saveRDS(net_by_garden_week, "Data/Working_files/networks_by_garden_week_only_phenobs.rds")

# Example distance correlation check for one network pair
cor(c(dist(net_by_garden_season$Interaction_network[[20]])),
    c(dist(net_by_garden_season$Int_frequency_network[[20]])))

