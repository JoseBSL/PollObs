############################################################ #
# Prepare data with phenological overlap
############################################################ #
# Load libraries
library(dplyr)
############################################################ #
# Load data
# Plant phenology
jena_phen = readRDS("Data/Phenology_data/clean_plant_phenobs_jena.rds") %>% 
  mutate(Garden = "Jena")
halle_phen = readRDS("Data/Phenology_data/clean_plant_phenobs_halle.rds") %>% 
  mutate(Garden = "Halle")
leipzig_phen = readRDS("Data/Phenology_data/clean_plant_phenobs_leipzig.rds") %>% 
  mutate(Garden = "Leipzig")
# Pollinator phenology
poll_pheno = readRDS("Data/Working_files/pollinator_phenology.rds")
############################################################ #
# Bind all gardens
plant_pheno = bind_rows(jena_phen, halle_phen, leipzig_phen)
# Create dataset with min and max dates of pheno per spp and garden
plant_range = plant_pheno %>% 
  filter(Flowering_intensity > 0) %>% 
  mutate(
    Date = if_else(
      is.na(Date),
      ymd("2023-01-01") + days(Doy - 1),
      Date)) %>% 
  group_by(Species, Garden) %>%
  summarise(
    Min_date_plant = min(Date, na.rm = TRUE),
    Max_date_plant = max(Date, na.rm = TRUE),
    .groups = "drop") %>% 
  rename(Plant = Species)
############################################################ #
# Pollinators
# Convert to Date
poll_pheno = poll_pheno %>%
  mutate(
    Date = ymd("2023-01-01") + days(Doy - 1))
# Summarise into 3 cols
poll_range = poll_pheno %>% 
  filter(Probability > 0) %>% 
  group_by(Species) %>%
  summarise(
    Min_date_poll = min(Date, na.rm = TRUE),
    Max_date_poll = max(Date, na.rm = TRUE),
    .groups = "drop") %>%
  rename(Pollinator = Species)
############################################################ #
# Obtain range for all potential combinations
pheno_overlap = plant_range %>%
  crossing(poll_range) %>%
  mutate(
    start_overlap = pmax(Min_date_plant, Min_date_poll),
    end_overlap   = pmin(Max_date_plant, Max_date_poll),
    Overlap_days  = pmax(0, as.numeric(end_overlap - start_overlap + 1))) %>%
  select(Garden, Plant, Pollinator, Overlap_days) %>% 
  rename(Botanical_garden = Garden)

# Fix some spp names
pheno_overlap = pheno_overlap %>% 
  mutate(Plant = if_else(Plant == "Silene vulgaris", "Viscaria vulgaris", Plant)) %>% 
  mutate(Plant = if_else(Plant == "Penstemon grandiflorus", "Penstemon bradburyi", Plant)) %>% 
  mutate(Plant = if_else(Plant == "Anemone hupehensis", "Eriocapitella hupehensis", Plant)) 
############################################################ #
# Save data
saveRDS(pheno_overlap, "Data/Working_files/phenoloical_overlap.rds")


