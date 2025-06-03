# ======================================================
# Compute standardize accumulation curve
# ======================================================

# Hand coded accumulation curve by plant species and time

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
  select(Plant_accepted_name, Botanical_garden, Total_time_species, Date, Sampling) %>%
  distinct() %>%
  rename(Plants = Plant_accepted_name,
         Sampling_time = Total_time_species) %>% 
  group_by(Plants, Botanical_garden, Sampling) %>%
  summarise(Total_sampling_time = sum(Sampling_time), .groups = 'drop')

# Get unique interactions per garden
int_data = raw_data %>%
  filter(Plant_rank == "SPECIES", 
         Pollinator_rank == "SPECIES") %>%
  select(Plant_accepted_name, Pollinator_accepted_name, Botanical_garden, Sampling) %>%
  rename(Plants = Plant_accepted_name,
         Pollinators = Pollinator_accepted_name) %>%
  distinct()

# Data structure
data = left_join(int_data, sampling_times)

#Sort data by botanical garden
data = data %>%
  arrange(Botanical_garden)


curves = function(data) {
  
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
    select(Plants, Unique_spp, Botanical_garden, Sampling) %>%
    distinct() %>%
    mutate(Unique_spp_cumulative = cumsum(Unique_spp)) %>%
    mutate(Plant_sampled = row_number())
  
  output_time = data %>%
    filter(Plants %in% sampled_plant) %>%
    arrange(match(Plants, sampled_plant)) %>%
    select(Plants, Total_sampling_time, Botanical_garden, Sampling) %>%
    distinct() %>%  # to ensure one row per plant
    mutate(
      Cumulative_time = cumsum(Total_sampling_time),
      Plant_sampled = row_number())
  
  output_curve = left_join(output_time, output, by = c("Plants", "Plant_sampled",
                                                       "Botanical_garden","Sampling"))
}

results_by_garden_sampling = data %>%
  group_split(Botanical_garden, Sampling) %>%
  map(~ map_dfr(1:100, function(i) curves(.x), .id = "iteration"))

iterations_combined = bind_rows(results_by_garden_sampling)

# Summarise mean curve per Plant_sampled per garden and sampling type
summary_by_garden_sampling = results_by_garden_sampling %>%
  map(~ .x %>%
        group_by(Plant_sampled, Botanical_garden, Sampling) %>%
        summarise(
          Mean_cumulative_spp = mean(Unique_spp_cumulative),
          Mean_cumulative_time = mean(Cumulative_time),
          .groups = "drop"))

# Combine summaries
summary_for_plot = bind_rows(summary_by_garden_sampling)

# Save data
saveRDS(iterations_combined, "Data/Working_files/rarefied_curves.rds")
saveRDS(summary_for_plot, "Data/Working_files/mean_rarefied_curve.rds")

# Final plot
p1 = ggplot() +
  # Iteration lines
  geom_line(data = iterations_combined, 
            aes(x = Plant_sampled, y = Unique_spp_cumulative, 
                group = interaction(iteration, Botanical_garden, Sampling),
                color = Botanical_garden, linetype = Sampling),
            size = 0.3, alpha = 0.3) +
  
  # Mean curves
  geom_line(data = summary_for_plot, 
            aes(x = Plant_sampled, y = Mean_cumulative_spp, 
                color = Botanical_garden, linetype = Sampling),
            size = 1.2) +
  
  scale_colour_viridis_d() +
  xlab("Plants") +
  ylab("Pollinators") +
  theme_minimal(base_size = 14) +
  labs(color = "Botanical Garden", linetype = "Sampling Type")


p1
# Final plot
p2 = ggplot() +
  # Iteration lines
  geom_line(data = iterations_combined, 
            aes(x = Cumulative_time, y = Unique_spp_cumulative, 
                group = interaction(iteration, Botanical_garden, Sampling),
                color = Botanical_garden, linetype = Sampling),
            size = 0.3, alpha = 0.3) +
  
  # Mean curves
  geom_line(data = summary_for_plot, 
            aes(x = Mean_cumulative_time, y = Mean_cumulative_spp, 
                color = Botanical_garden, linetype = Sampling),
            size = 1.2) +
  
  scale_colour_viridis_d() +
  xlab("Time (mins)") +
  ylab("Pollinators") +
  theme_minimal(base_size = 14) +
  labs(color = "Botanical Garden", linetype = "Sampling Type")


library(patchwork)

p1 + p2 + plot_layout(guides = 'collect')




