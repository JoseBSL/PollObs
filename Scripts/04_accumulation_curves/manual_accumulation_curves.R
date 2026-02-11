# ======================================================
# Compute standardized accumulation curve
# ======================================================

# Load libraries
library(dplyr)
library(purrr)
library(ggplot2)
library(tibble)
library(minpack.lm)
library(patchwork)
library(ggnewscale)

# ======================================================
# Load data
raw_data <- readRDS("Data/Working_files/interaction_data.rds")
colnames(raw_data)

# ======================================================
# 1) Prepare data to compute accumulation curves across species

# Get total sampling time per species and garden
sampling_times <- raw_data %>%
  filter(Plant_rank == "SPECIES") %>%
  select(Plant_accepted_name, Botanical_garden, Total_time_species, Date, Sampling) %>%
  distinct() %>%
  rename(Plants = Plant_accepted_name,
         Sampling_time = Total_time_species) %>% 
  group_by(Plants, Botanical_garden, Sampling) %>%
  summarise(Total_sampling_time = sum(Sampling_time), .groups = 'drop')

# Get unique interactions per garden
int_data <- raw_data %>%
  filter(Plant_rank == "SPECIES", 
         Pollinator_rank == "SPECIES") %>%
  select(Plant_accepted_name, Pollinator_accepted_name, Botanical_garden, Sampling) %>%
  rename(Plants = Plant_accepted_name,
         Pollinators = Pollinator_accepted_name) %>%
  distinct()

# Join data
data <- left_join(int_data, sampling_times) %>%
  arrange(Botanical_garden)

# ======================================================
# Accumulation curve function
curves <- function(data) {
  
  # Random order of all plants
  sampled_plant <- sample(unique(data$Plants), length(unique(data$Plants)))
  
  output <- data %>% 
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
  
  output_time <- data %>%
    filter(Plants %in% sampled_plant) %>%
    arrange(match(Plants, sampled_plant)) %>%
    select(Plants, Total_sampling_time, Botanical_garden, Sampling) %>%
    distinct() %>%  # one row per plant
    mutate(
      Cumulative_time = cumsum(Total_sampling_time),
      Plant_sampled = row_number())
  
  output_curve <- left_join(output_time, output, by = c("Plants", "Plant_sampled",
                                                        "Botanical_garden","Sampling"))
  
  return(output_curve)
}

# ======================================================
# Run accumulation curves iteratively by garden and sampling type
results_by_garden_sampling <- data %>%
  group_split(Botanical_garden, Sampling) %>%
  map(~ map_dfr(1:100, function(i) curves(.x), .id = "iteration"))

iterations_combined <- bind_rows(results_by_garden_sampling)

# ======================================================
# Summarize mean curve per plant sampled, per garden and sampling type
summary_by_garden_sampling <- results_by_garden_sampling %>%
  map(~ .x %>%
        group_by(Plant_sampled, Botanical_garden, Sampling) %>%
        summarise(
          Mean_cumulative_spp = mean(Unique_spp_cumulative),
          Mean_cumulative_time = mean(Cumulative_time),
          .groups = "drop"))

# Combine summaries
summary_for_plot <- bind_rows(summary_by_garden_sampling)

# ======================================================
# Optional: Save data
# saveRDS(iterations_combined, "Data/Working_files/rarefied_curves.rds")
# saveRDS(summary_for_plot, "Data/Working_files/mean_rarefied_curve.rds")

# ======================================================
# Extrapolation to fixed max time
global_max_time <- 4000

fit_extrapolate_fixed_time <- function(df, max_time) {
  model <- nlsLM(Unique_spp_cumulative ~ a * (1 - exp(-(b * Cumulative_time)^c)),
                 data = df,
                 start = list(a = max(df$Unique_spp_cumulative), b = 0.001, c = 1))
  
  new_times <- data.frame(Cumulative_time = seq(0, max_time, length.out = 100))
  new_times$predicted <- predict(model, newdata = new_times)
  new_times$Botanical_garden <- unique(df$Botanical_garden)
  new_times$Sampling <- unique(df$Sampling)
  
  return(new_times)
}

# Apply extrapolation by group
extrapolated_data_fixed <- iterations_combined %>%
  group_by(Botanical_garden, Sampling) %>%
  group_split() %>%
  map_dfr(~ fit_extrapolate_fixed_time(.x, global_max_time))

# Max observed time per group
max_times_df <- iterations_combined %>%
  group_by(Botanical_garden, Sampling) %>%
  summarise(max_observed_time = max(Cumulative_time), .groups = 'drop')

# Add alpha group to extrapolated data
extrapolated_data_fixed <- extrapolated_data_fixed %>%
  left_join(max_times_df, by = c("Botanical_garden", "Sampling")) %>%
  mutate(alpha_group = ifelse(Cumulative_time <= max_observed_time, "observed", "extrapolated"))

# ======================================================

summary_for_plot_points = extrapolated_data_fixed %>% 
  filter(alpha_group == "observed") %>% 
  group_by(Botanical_garden, Sampling) %>% 
  summarise(Mean_cumulative_time = max(Cumulative_time),
            Mean_cumulative_spp = max(predicted))


# Final plot
ggplot() +
  # Iteration lines
  geom_path(data = iterations_combined, 
            aes(x = Cumulative_time, y = Unique_spp_cumulative, 
                group = interaction(iteration, Botanical_garden, Sampling),
                color = Plant_sampled),
            size = 0.3, alpha = 0.5) +
  scale_colour_gradient(low = "grey90", high = "grey10", guide = "colorbar") +
  labs(color = "Plants sampled") +
  new_scale_color() +
  
  # Mean extrapolated curves
  geom_line(data = extrapolated_data_fixed, 
            aes(x = Cumulative_time, y = predicted, 
                color = Botanical_garden, linetype = Sampling, alpha = alpha_group),
            size = 1.2) +
  scale_alpha_manual(values = c("observed" = 1, "extrapolated" = 0.5), guide = "none") +
  scale_colour_viridis_d() +
  scale_linetype_manual(
    values = c("Focal" = "solid", "Random_census" = 22),
    labels = c("Focal" = "Phenobs plants", "Random_census" = "Random plants")) +
    geom_point(data = summary_for_plot_points, 
             aes(x = Mean_cumulative_time, y = Mean_cumulative_spp, 
                 color = Botanical_garden, linetype = Sampling),
             size = 2.74) +
  scale_colour_viridis_d() +
  
  # Labels and theme
  xlab("Time (mins)") +
  ylab("Pollinators") +
  theme_minimal(base_size = 14) +
  labs(color = "Botanical Garden", linetype = "Sampling type", alpha = "Data Type")

#Read interaction daa
library(dplyr)
library(tidyr)
library(lubridate)
library(readr)
library(ggplot2)
library(viridis)
library(scales)
#Load data
raw_data = readRDS("Data/Working_files/interaction_data.rds")


poll_species = raw_data %>% 
  filter(Pollinator_rank == "SPECIES") %>% 
  filter(Pollinator_order == "Hymenoptera" | Pollinator_order == "Diptera"| 
           Pollinator_order == "Coleoptera" |  Pollinator_order == "Lepidoptera") %>% 
  select(Pollinator_accepted_name, Pollinator_order) %>% 
  group_by(Pollinator_order) %>% 
  summarise(Species = n_distinct(Pollinator_accepted_name)/n_distinct(.),
            Interactions = length(Pollinator_accepted_name)/ nrow(.))

# Combine Interactions and Species into a single column
data_long = pivot_longer(poll_species, cols = c(Interactions, Species), names_to = "Variable", values_to = "Value")

#Set order of levels for plotting
data_long$Pollinator_order = factor(data_long$Pollinator_order, levels = c("Coleoptera", "Lepidoptera", "Diptera", "Hymenoptera"))

data_long$Variable = factor(data_long$Variable, levels = c("Species", "Interactions"))

p1 = ggplot(data_long, aes(x = Variable, y = Value, fill = Pollinator_order)) + 
  geom_bar(stat = "identity", width = 0.7, alpha=0.85) +
  theme_classic() +
  scale_fill_viridis(discrete=TRUE, direction = -1, name="Pollinator order") +
  xlab(NULL) +
  ylab(NULL) + 
  scale_y_continuous(breaks=c(0,0.5,1),labels = percent_format(), expand = c(0, 0)) +
  theme(axis.text.y = element_text(size=14), 
        axis.text.x = element_text(size=14), 
        axis.title.y = element_text(face = "bold", size = 18, vjust = -2),  
        axis.title.x = element_text(face="bold", size = 18),
        legend.position = "right",
        legend.title = element_text(size = 14, face = "bold"),
        legend.text = element_text(size = 14),
        legend.key.size = unit(5, "mm"),
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-8,-5,-5,-5)) +
  guides(fill=guide_legend(nrow=2,byrow=TRUE))


p1


