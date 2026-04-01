library(dplyr)

# Read pollinator phenology
poll_data = readRDS("Data/Working_files/pollinator_phenology.rds")

#--------------------------------------------------
# 1. Calculate phenological center per species
#--------------------------------------------------

pollinator_centers <- poll_data %>%
  mutate(
    Probability = as.numeric(Probability),
    Abundances  = as.numeric(Abundances)
  ) %>%
  filter(
    !is.na(Doy),
    !is.na(Probability),
    Probability > 0,
    Flying_period == "Yes"
  ) %>%
  group_by(Species) %>%
  summarise(
    center = weighted.mean(Doy, Probability, na.rm = TRUE),
    n_obs = n(),
    .groups = "drop"
  ) %>%
  filter(n_obs >= 3)

pollinator_centers

#--------------------------------------------------
# 2. Simple synchronization metrics (percentile-based)
#--------------------------------------------------

pollinator_sync_simple = pollinator_centers %>%
  summarise(
    q05 = quantile(center, 0.05, na.rm = TRUE),
    q50 = quantile(center, 0.50, na.rm = TRUE),
    q95 = quantile(center, 0.95, na.rm = TRUE),
    duration = q95 - q05,
    sd_center = sd(center, na.rm = TRUE),
    sync = 1 - sd_center / duration,
    n_species = n()
  )

pollinator_sync_simple

#--------------------------------------------------
# 3. Save processed data
#--------------------------------------------------
saveRDS(pollinator_centers, "Data/Working_files/pollinator_pheno_centers.rds")
