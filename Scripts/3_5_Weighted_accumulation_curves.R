# ======================================================
# Compute standardize accumulation curve
# ======================================================

# Potential analytical methods
# This an alternative to accumulation curves with iNEXT
# Compute cumulative sum of poll species by time (y axis) across plant species
# Compare accumulation accumulation rates (slopes) and area under the curve

# ======================================================
# Load libraries
library(dplyr)
# ======================================================

# Load data
raw_data = readRDS("Data/Working_files/interaction_data.rds")
colnames(raw_data)

# ======================================================
# 1) Prepare data to compute accumulation curves across species
# Get total sampling time per species and garden
sampling_times = raw_data %>%
  filter(Plant_rank == "SPECIES") %>%
  select(Plant_accepted_name, Botanical_garden, Total_time_species, Date) %>%
  distinct() %>%
  rename(Plants = Plant_accepted_name,
         Sampling_time = Total_time_species) %>% 
  group_by(Plants, Botanical_garden) %>%
  summarise(Total_sampling_time = sum(Sampling_time), .groups = 'drop')

# Get unique interactions per garden
int_data = raw_data %>%
  filter(Plant_rank == "SPECIES", 
         Pollinator_rank == "SPECIES") %>%
  select(Plant_accepted_name, Pollinator_accepted_name, Botanical_garden) %>%
  rename(Plants = Plant_accepted_name,
         Pollinators = Pollinator_accepted_name) %>%
  distinct()

# Data structure
data = left_join(int_data, sampling_times)

# ======================================================
# Example for 1 garden
data = data %>% 
  filter(Botanical_garden == "Halle")



curves = function(data){
  
sampled_plant = sample(unique(data$Plants))

output = data %>% 
  filter(Plants %in% sampled_plant) %>%
  arrange(match(Plants, sampled_plant)) %>%
  group_by(Pollinators) %>%
  mutate(distinct = row_number() == 1) %>%
  ungroup() %>%
  group_by(Plants) %>%
  mutate(Unique_spp = sum(distinct)) %>%
  ungroup() %>%
  select(Plants, Unique_spp, Botanical_garden) %>%
  distinct() %>%
  mutate(Unique_spp_cumulative = cumsum(Unique_spp)) %>%
  mutate(Plant_sampled = row_number())
}

curves_by_iteration = map_dfr(1:100, ~ curves(data), .id="iteration") 





output = left_join(output, sampling_times)

ggplot(output, aes(x = Plant_sampled, y = Unique_spp_cumulative)) +
  geom_line(size = 0.8, color = "black") +
  labs(
    x = "Plants Sampled",
    y = "Pollinator Species") +
  theme_minimal(base_size = 14)



# ======================================================

  data %>% 
  filter(Plants %in% sampled_plant) %>%
  arrange(match(Plants, sampled_plant)) %>%
  group_by(Pollinators) %>%
  mutate(distinct = row_number() == 1) %>%
  ungroup() %>%
  group_by(Plants) %>%
  mutate(Unique_spp = sum(distinct)) %>%
  ungroup() %>%
  select(Plants, Unique_spp, Botanical_garden) %>%
  distinct() %>%
  mutate(Unique_spp_cumulative = cumsum(Unique_spp)) %>%
  mutate(Plant_sampled = row_number())
