# ======================================================
#Script: Prepare plant-poll networks (visits and visitation rate)
# ======================================================

#1)Total interactions
#AND
#2)Interaction frequency

#Load libraries
library(readr)
library(tidyr)
library(dplyr)
library(purrr)
library(stringr)
library(tibble)
# ======================================================
# Load data
# Interaction data
raw_data = readRDS("Data/Working_files/interaction_data.rds")

# Morphometrics to get phenobs species vector
morphometrics = read_csv("Data/Trait_data/Raw/ReproductiveTraits_Morphometrics.csv")
colnames(morphometrics)
phenobs_spp = morphometrics %>% 
  select(Species) %>% 
  mutate(Species = str_replace(Species, "Persicaria bistorta", "Polygonum bistorta")) %>% 
  mutate(Species = str_replace(Species, "Aquilegia chrysantha", "Aquilegia vulgaris")) %>% 
  distinct() %>% 
  pull(Species) 

# ======================================================
# Create tibble with unique dates 
dates = raw_data %>% 
  select(Botanical_garden, Date) %>% 
  group_by(Botanical_garden) %>% 
  distinct()

# Convert to tibble
groupped_dates = dates %>%
  group_by(Botanical_garden) %>% 
  arrange(Date, .by_group = TRUE) %>% 
  mutate(Season_group = ntile(Date, 3)) %>%
  mutate(Week = lubridate::week(Date)) %>%
  mutate(Season = case_when(
    Season_group == 1 ~ "Early",
    Season_group == 2 ~ "Mid",
    Season_group == 3 ~ "Late"
  )) %>%
  select(!Season_group) %>% 
  ungroup()


# ======================================================
# Create vector of mail orders
bee_fam = c("Megachilidae", 
            "Apidae",
            "Colletidae",
            "Andrenidae",
            "Halictidae",
            "Mellittidae")

# Prepare interaction data
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
  filter(Pollinator_family %in% bee_fam) %>% 
  filter(!Plants == "Iberis sempervirens") 

interaction_data = interaction_data %>% 
  filter(Plant %in% phenobs_spp) 

#Add season category to the data (early-mid and late season)
interaction_data = left_join(interaction_data, groupped_dates)

# ======================================================
#1)Total interactions

#Convert to network
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
networks_by_garden_interactions = interaction_data %>%
  group_by(Botanical_garden,Week) %>%
  nest() %>%
  mutate(Interaction_network = map(data, to_network)) %>%
  select(!data) %>% 
  ungroup()

networks_Halle <- networks_by_garden_interactions %>% 
  dplyr::filter(Botanical_garden == "Halle") %>% 
  dplyr::filter(Week %in% c(19,25,27))

networks_Jena <- networks_by_garden_interactions %>% 
  dplyr::filter(Botanical_garden == "Jena") %>% 
  dplyr::filter(Week %in% c(20,26,29))

networks_Leipzig <- networks_by_garden_interactions %>% 
  dplyr::filter(Botanical_garden == "Leipzig") %>% 
  dplyr::filter(Week %in% c(15,26,28))

networks_week <- dplyr::bind_rows(networks_Halle,
                                  networks_Jena,
                                  networks_Leipzig)


# ======================================================
#2)Interaction frequency
#Overwrite interactions with interaction frequency 
#to minimise edits in code
interaction_frequency = interaction_data %>% 
  group_by(Botanical_garden, Week, Plants, Pollinators) %>%
  summarise(Interaction_total = sum(Interactions))

total_time_species = interaction_data %>%
  dplyr::select(Botanical_garden, Week, Plants, Date, Total_time_species) %>% 
  group_by(Botanical_garden, Week, Plants) %>%
  distinct() %>% 
  summarise(Total_time_species = sum(Total_time_species, na.rm = TRUE), .groups = "drop")

# Join total time back into interaction data
interaction_data_freq = interaction_frequency %>%
  left_join(total_time_species, by = c("Botanical_garden", "Week", "Plants"), suffix = c("", "_summed")) %>%
  mutate(Freq = Interaction_total / Total_time_species)


# Now convert to interaction frequency network
to_freq_network = function(data) {
  data %>%
    group_by(Plants, Pollinators) %>%
    summarise(Total_frequency = sum(Freq), .groups = "drop") %>%
    pivot_wider(names_from = Pollinators, 
                values_from = Total_frequency, values_fill = 0) %>%
    column_to_rownames("Plants") %>%
    as.matrix()
}

# Prepare networks per garden (frequencies)
networks_by_garden_int_frequency = interaction_data_freq %>%
  group_by(Botanical_garden, Week) %>%
  nest() %>%
  mutate(Int_frequency_network = map(data, to_freq_network)) %>%
  select(!data) %>%
  ungroup()


networks_int_frequency_Halle <- networks_by_garden_int_frequency %>% 
  dplyr::filter(Botanical_garden == "Halle") %>% 
  dplyr::filter(Week %in% c(19,25,27))

networks_int_frequency_Jena <- networks_by_garden_int_frequency %>% 
  dplyr::filter(Botanical_garden == "Jena") %>% 
  dplyr::filter(Week %in% c(20,26,29))

networks_int_frequency_Leipzig <- networks_by_garden_int_frequency %>% 
  dplyr::filter(Botanical_garden == "Leipzig") %>% 
  dplyr::filter(Week %in% c(15,26,28))

networks_int_frequency_week <- dplyr::bind_rows(networks_int_frequency_Halle,
                                                networks_int_frequency_Jena,
                                                networks_int_frequency_Leipzig)


# Now bind both and get a tibble with total int and int freq
net_by_garden_WEEK = left_join(networks_week,
                               networks_int_frequency_week)


# ======================================================
# Save data
saveRDS(net_by_garden_WEEK, "Data/Working_files/networks_by_garden_only_phenobs_bees_WEEK.rds")


#Safety check
cor(c(dist(net_by_garden_WEEK$Interaction_network[[1]])),
    c(dist(net_by_garden_WEEK$Int_frequency_network[[1]])))

