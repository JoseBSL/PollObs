library(dplyr)
library(tidyr)
library(lubridate)
library(bipartite)

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

poll_abundance_season <- interaction_data %>%
  group_by(Botanical_garden, Season, Pollinator) %>%
  count() %>% 
  rename(Total_pollinator_abundance = n) %>% ungroup()


data_floral_ab_sampling_time_by_sp <- 
  readr::read_csv("Data/Working_files/data_floral_ab_sampling_time_by_sp.csv") %>% 
  filter(Plant %in% phenobs_spp)  %>% arrange(Week)


data_floral_ab_sampling_time_by_sp <- data_floral_ab_sampling_time_by_sp %>%
  left_join(groupped_dates, by = c("Botanical_garden", "Week"))

data_floral_ab_sampling_time_by_sp$week_check <- lubridate::week(data_floral_ab_sampling_time_by_sp$Date)

data_floral_ab_sampling_time_by_sp %>% dplyr::filter(Week != week_check)
  
data_floral_ab_sampling_time_by_sp <- data_floral_ab_sampling_time_by_sp %>%
  group_by(Botanical_garden, Season, Plant) %>%
  summarise(
    Total_floral_abundance = sum(Total_floral_abundance),
  ) %>% ungroup()



readr::write_csv(data_floral_ab_sampling_time_by_sp,"Data/Working_files/total_floral_abundance_by_early_mid_late_SEASON_sp.csv")
readr::write_csv(poll_abundance_season,"Data/Working_files/total_poll_abundance_by_early_mid_late_SEASON_sp.csv")
