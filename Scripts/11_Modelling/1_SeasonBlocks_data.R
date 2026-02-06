############################################################
# SEASON-BLOCK DATA PREP (Early / Mid / Late)
# Parallel to weekly pipeline, but using Season instead of Week
# Output: model_data_season (one row per garden × plant × pollinator × Season)
############################################################

library(dplyr)
library(lubridate)

# Load data
raw_data <- readRDS("Data/Working_files/interaction_data.rds")

# ------------------------------------------------------------
# 0) Build Season labels per garden based on date terciles
# ------------------------------------------------------------
dates <- raw_data %>%
  mutate(Date = as.Date(Date_time)) %>%
  select(Botanical_garden, Date) %>%
  distinct()

groupped_dates <- dates %>%
  group_by(Botanical_garden) %>%
  arrange(Date, .by_group = TRUE) %>%
  mutate(
    Season_group = ntile(Date, 3),
    Season = case_when(
      Season_group == 1 ~ "Early",
      Season_group == 2 ~ "Mid",
      Season_group == 3 ~ "Late"
    )
  ) %>%
  select(-Season_group) %>%
  ungroup()

# Attach Season to every raw record
raw_season <- raw_data %>%
  mutate(Date = as.Date(Date_time)) %>%
  left_join(groupped_dates, by = c("Botanical_garden", "Date")) %>%
  filter(!is.na(Season))

# Optional: enforce Season order
raw_season <- raw_season %>%
  mutate(Season = factor(Season, levels = c("Early", "Mid", "Late")))

# ------------------------------------------------------------
# 1) Pair-season totals (counts)
# ------------------------------------------------------------
interaction_season <- raw_season %>%
  filter(!is.na(Interactions),
         !is.na(Floral_abundance),
         Pollinator != "None",
         !is.na(Pollinator_accepted_name)) %>%
  transmute(
    Botanical_garden,
    Plant      = Plant_accepted_name,
    Pollinator = Pollinator_accepted_name,
    Season,
    Interactions
  ) %>%
  group_by(Botanical_garden, Plant, Pollinator, Season) %>%
  summarise(
    Total_pair_interactions = sum(Interactions, na.rm = TRUE),
    .groups = "drop"
  )

# ------------------------------------------------------------
# 2) Plant-season effort (total observation time per plant per season)
# IMPORTANT: effort is plant-level, NOT pollinator-level
# ------------------------------------------------------------
plant_season_effort <- raw_season %>%
  filter(!is.na(Interactions),
         !is.na(Floral_abundance),
         Pollinator != "None") %>%
  transmute(
    Botanical_garden,
    Plant = Plant_accepted_name,
    Season,
    Total_time_species
  ) %>%
  distinct() %>%
  group_by(Botanical_garden, Plant, Season) %>%
  summarise(
    Total_time_plant_season = sum(Total_time_species, na.rm = TRUE),
    .groups = "drop"
  )

# ------------------------------------------------------------
# 3) Join + compute visitation rate
# ------------------------------------------------------------
interaction_season_data <- interaction_season %>%
  left_join(plant_season_effort,
            by = c("Botanical_garden", "Plant", "Season")) %>%
  mutate(
    VisitRate = if_else(Total_time_plant_season > 0,
                        Total_pair_interactions / Total_time_plant_season,
                        NA_real_),
    Pair = paste(Plant, Pollinator, sep = "_")
  )

# ------------------------------------------------------------
# 4) Environmental variables per garden-season
# ------------------------------------------------------------
environmental_season <- raw_season %>%
  filter(!is.na(Interactions),
         !is.na(Floral_abundance),
         Pollinator != "None") %>%
  transmute(
    Botanical_garden,
    Season,
    Temperature,
    Humidity,
    Rainfall
  ) %>%
  group_by(Botanical_garden, Season) %>%
  summarise(
    Mean_Temperature = mean(Temperature, na.rm = TRUE),
    Mean_Humidity    = mean(Humidity, na.rm = TRUE),
    Mean_Rainfall    = mean(Rainfall, na.rm = TRUE),
    .groups = "drop"
  )

# ------------------------------------------------------------
# 5) Pollinator abundance per garden-pollinator-season
# ------------------------------------------------------------
poll_abundance_season <- raw_season %>%
  filter(!is.na(Interactions),
         Pollinator != "None",
         !is.na(Pollinator_accepted_name)) %>%
  group_by(
    Botanical_garden,
    Pollinator = Pollinator_accepted_name,
    Season
  ) %>%
  summarise(
    Total_pollinator_abundance = n(),
    .groups = "drop"
  )

# ------------------------------------------------------------
# 6) Floral abundance per garden-plant-season
# ------------------------------------------------------------
floral_abundance_season <- raw_season %>%
  filter(!is.na(Floral_abundance)) %>%
  select(
    Botanical_garden,
    Plant = Plant_accepted_name,
    Season,
    Floral_abundance,
    Date_time
  ) %>%
  distinct() %>%
  group_by(Botanical_garden, Plant, Season) %>%
  summarise(
    Floral_abundance = sum(Floral_abundance, na.rm = TRUE),
    .groups = "drop"
  )

# ------------------------------------------------------------
# 7) FINAL JOIN: one row per (garden × plant × pollinator × Season)
# ------------------------------------------------------------
model_data_season <- interaction_season_data %>%
  left_join(environmental_season,  by = c("Botanical_garden", "Season")) %>%
  left_join(floral_abundance_season, by = c("Botanical_garden", "Plant", "Season")) %>%
  left_join(poll_abundance_season, by = c("Botanical_garden", "Pollinator", "Season"))

# ------------------------------------------------------------
# 8) Transforms ready for modelling
# ------------------------------------------------------------
model_data_season <- model_data_season %>%
  mutate(
    log_flower = log1p(Floral_abundance),
    log_poll   = log1p(Total_pollinator_abundance),
    log_poll_z = as.numeric(scale(log_poll))
  )
