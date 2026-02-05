############################################################ #
#Prepare data for modelling:
# Response variable - int_frequency
# Key predictors: - environmental_variables
#                 - floral_abundance_week
#                 - poll_abundance_week

############################################################ #
# Load libraries
library(dplyr)
library(lubridate)
library(ggplot2)
############################################################ #
# Load data
raw_data = readRDS("Data/Working_files/interaction_data.rds")
############################################################ #
# Interaction frequency
# 1) Pair-week totals (counts)
interaction_week = raw_data %>%
  filter(!is.na(Interactions),
         !is.na(Floral_abundance),
         Pollinator != "None") %>%
  transmute(
    Botanical_garden,
    Plant      = Plant_accepted_name,
    Pollinator = Pollinator_accepted_name,
    Week       = week(as.Date(Date_time)),
    Interactions,
    Total_time_species
  ) %>%
  filter(!is.na(Pollinator)) %>%
  group_by(Botanical_garden, Plant, Pollinator, Week) %>%
  summarise(
    Total_pair_interactions = sum(Interactions, na.rm = TRUE),
    .groups = "drop"
  )

# 2) Plant-week effort (total observation time per plant per week)
plant_week_effort = raw_data %>%
  filter(!is.na(Interactions),
         !is.na(Floral_abundance),
         Pollinator != "None") %>%
  transmute(
    Botanical_garden,
    Plant = Plant_accepted_name,
    Week  = week(as.Date(Date_time)),
    Total_time_species
  ) %>%
  distinct() %>%                       # avoid repeating within the same sampling record
  group_by(Botanical_garden, Plant, Week) %>%
  summarise(
    Total_time_plant_week = sum(Total_time_species, na.rm = TRUE),
    .groups = "drop"
  )

# 3) Join + compute visitation rate
interaction_data = interaction_week %>%
  left_join(plant_week_effort, by = c("Botanical_garden", "Plant", "Week")) %>%
  mutate(
    VisitRate = if_else(Total_time_plant_week > 0,
                        Total_pair_interactions / Total_time_plant_week,
                        NA_real_),
    Pair = paste(Plant, Pollinator, sep = "_"))

############################################################ #

# Environmental variables
environmental_variables = raw_data %>%
  filter(!is.na(Interactions),
         !is.na(Floral_abundance),
         Pollinator != "None") %>%
  transmute(
    Botanical_garden,
    Week = lubridate::week(as.Date(Date_time)),
    Temperature,
    Humidity,
    Rainfall
  ) %>%
  group_by(Botanical_garden, Week) %>%
  summarise(
    Mean_Temperature = mean(Temperature, na.rm = TRUE),
    Mean_Humidity    = mean(Humidity, na.rm = TRUE),
    Mean_Rainfall    = mean(Rainfall, na.rm = TRUE),
    .groups = "drop"
  )
############################################################ #
# Abundances per week
# Pollinator abundance
poll_abundance_week = raw_data %>%
  filter(!is.na(Interactions),
         Pollinator != "None") %>%
  mutate(Date = as.Date(Date_time),
         Week = lubridate::week(Date)) %>%
  group_by(Botanical_garden,
           Pollinator = Pollinator_accepted_name,
           Week) %>%
  summarise(
    Total_pollinator_abundance = n(),
    .groups = "drop"
  )
# Floral abundance
floral_abundance_week = raw_data %>%
  filter(!is.na(Floral_abundance)) %>%
  mutate(
    Date = as.Date(Date_time),
    Week = lubridate::week(Date)
  ) %>%
  select(Botanical_garden, Plant = Plant_accepted_name,
         Week, Floral_abundance, Date_time) %>%
  distinct() %>%                 # avoid repeated counts per interaction
  group_by(Botanical_garden, Plant, Week) %>%
  summarise(
    Floral_abundance = sum(Floral_abundance, na.rm = TRUE),
    .groups = "drop")
############################################################ #
# FINAL JOIN: one row per (garden × plant × pollinator × week)
model_data = interaction_data %>%
  left_join(environmental_variables, by = c("Botanical_garden", "Week")) %>%
  left_join(floral_abundance_week,   by = c("Botanical_garden", "Plant", "Week")) %>%
  left_join(poll_abundance_week,     by = c("Botanical_garden", "Pollinator", "Week")) 

############################################################ #
# Check distribution of response variable
ggplot(model_data, aes(x = VisitRate)) +
  geom_histogram(bins = 30, fill = "steelblue", colour = "black") +
  theme_minimal() +
  labs(x = "Visit rate", y = "Frequency")
# Check distribution of response variable by garden
ggplot(model_data, aes(x = VisitRate)) +
  geom_histogram(bins = 25, fill = "steelblue", colour = "black") +
  facet_wrap(~Botanical_garden) +
  theme_minimal()
############################################################ #
# Modelling

model_data <- model_data %>%
  mutate(
    log_flower = log1p(Floral_abundance),
    log_poll   = log1p(Total_pollinator_abundance),
    log_flower_z = as.numeric(scale(log_flower)),
    log_poll_z   = as.numeric(scale(log_poll))
  )

library(glmmTMB)
library(DHARMa)
model1 = glmmTMB(VisitRate ~  log_flower * 
                   log_poll_z +
                   Botanical_garden +
                   (1 | Week) +
                   (1 | Pair),
                 family = Gamma(link = "log"),
                 data = model_data)
summary(model1)
simulationOutput <- simulateResiduals(fittedModel = model1)
plot(simulationOutput)
library(ggeffects)

library(performance)
r2(model1)



eff_floral <- ggpredict(model1, terms = "log_flower")
ggplot(eff_floral, aes(x = x, y = predicted)) +
  geom_line(size = 1.2) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  labs(
    x = "Floral abundance (scaled)",
    y = "Predicted visitation rate"
  ) +
  theme_minimal()

eff_poll = ggpredict(model1, terms = "log_poll_z")
ggplot(eff_poll, aes(x = x, y = predicted)) +
  geom_line(size = 1.2) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  labs(
    x = "Pollinator abundance (scaled)",
    y = "Predicted visitation rate"
  ) +
  theme_minimal()

# Interactive abundance effect
eff_interaction <- ggpredict(
  model1,
  terms = c("log_flower", "log_poll_z")
)

ggplot(eff_interaction,
       aes(x = x, y = predicted, color = group)) +
  geom_line(size = 1.2) +
  geom_ribbon(
    aes(ymin = conf.low, ymax = conf.high, fill = group),
    alpha = 0.15,
    color = NA
  ) +
  labs(
    x = "Floral abundance (scaled)",
    y = "Predicted visitation rate",
    color = "Pollinator abundance",
    fill = "Pollinator abundance"
  ) +
  theme_minimal()
