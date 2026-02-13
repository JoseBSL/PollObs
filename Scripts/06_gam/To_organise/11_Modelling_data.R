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
library(glmmTMB)
library(DHARMa)
library(ggeffects)
library(performance)
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
#ggplot(model_data, aes(x = VisitRate)) +
#  geom_histogram(bins = 30, fill = "steelblue", colour = "black") +
#  theme_minimal() +
#  labs(x = "Visit rate", y = "Frequency")
## Check distribution of response variable by garden
#ggplot(model_data, aes(x = VisitRate)) +
#  geom_histogram(bins = 25, fill = "steelblue", colour = "black") +
#  facet_wrap(~Botanical_garden) +
#  theme_minimal()
############################################################ #
# Modelling

model_data <- model_data %>%
  mutate(
    log_flower = log1p(Floral_abundance),
    log_poll   = log1p(Total_pollinator_abundance),
    log_flower_z = as.numeric(scale(log_flower)),
    log_poll_z   = as.numeric(scale(log_poll))
  )

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

r2(model1)

# ============================================================
# Faceted interaction plot by Botanical_garden
# RAW axes + RAW legend values
# ============================================================

# 1) Choose pollinator abundance levels (RAW scale)
poll_levels_raw <- as.numeric(
  quantile(model_data$Total_pollinator_abundance,
           probs = c(0.5, 0.7, 0.895),
           na.rm = TRUE)
)

# 2) Convert RAW levels to model scale (log_poll_z)
poll_center <- attr(scale(model_data$log_poll), "scaled:center")
poll_scale  <- attr(scale(model_data$log_poll), "scaled:scale")

poll_levels_z <- (log1p(poll_levels_raw) - poll_center) / poll_scale

# 3) Predict interaction at those levels, BY GARDEN
eff_interaction <- ggpredict(
  model1,
  terms = c(
    "log_flower",
    paste0("log_poll_z [", paste(round(poll_levels_z, 3), collapse = ", "), "]"),
    "Botanical_garden"
  )
)

eff_interaction <- as.data.frame(eff_interaction)

# 4) Back-transform floral abundance to RAW scale
eff_interaction$Floral_abundance_raw <- exp(eff_interaction$x) - 1

# 5) Use RAW pollinator values directly in legend
eff_interaction$group <- factor(eff_interaction$group, levels = unique(eff_interaction$group))

eff_interaction$poll_group_label <- factor(
  eff_interaction$group,
  levels = levels(eff_interaction$group),
  labels = round(poll_levels_raw, 0)
)

# 6) Plot (facet by garden)
ggplot(eff_interaction,
       aes(x = Floral_abundance_raw, y = predicted,
           color = poll_group_label, fill = poll_group_label)) +
  geom_line(linewidth = 1.2) +
  scale_x_log10() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
              alpha = 0.15, color = NA) +
  scale_color_viridis_d(option = "D") +
  scale_fill_viridis_d(option = "D") +
  facet_wrap(~ facet) +
  labs(
    x = "Floral abundance",
    y = "Predicted visitation rate",
    color = "Pollinator abundance",
    fill  = "Pollinator abundance"
  ) +
  theme_minimal()
