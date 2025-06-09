# ======================================================
# Potential analyses

# Obtain phylogenetic signal
# Resample subset (x number of species) of phenobs and random species 
# and calculate mean phylogenetic distance across species
# then repeat this x times
# ======================================================

# Load libraries
library(dplyr)
library(rtrees)
library(ggtree)
library(ape)
library(purrr)
library(ggplot2)
library(DescTools) 
# ======================================================
# Load data
raw_data = readRDS("Data/Working_files/interaction_data.rds")
colnames(raw_data)
# Create vector of mail orders
poll_order = c("Hymenoptera", "Diptera", "Coleoptera", "Lepidoptera")
# ======================================================
# Prepare data
interaction_data = raw_data %>%
  filter(!is.na(Interactions),
         !is.na(Floral_abundance),
         Pollinator != "None") %>%
  rename(Plants = Plant_accepted_name,
         Pollinators = Pollinator_accepted_name) %>%
  filter(!is.na(Pollinators)) %>% 
  filter(Pollinator_rank == "SPECIES") %>% 
  filter(Plant_rank == "SPECIES") %>% 
  #filter(Sampling == "Focal") %>% 
  # filter(!Pollinators == "Apis mellifera") %>% 
  filter(Pollinator_order %in% poll_order) %>% 
  filter(!Plants == "Iberis sempervirens") 

# Get total sampling time per species and garden to correct if necessary
sampling_time = interaction_data %>% 
  select(Botanical_garden, Plants, Sampling, Date, Total_time_species) %>%
  distinct() %>% 
  group_by(Botanical_garden, Plants, Sampling) %>%
  summarise(Time = sum(Total_time_species)) %>% 
  select(Botanical_garden, Plants, Sampling, Time)
# Get pollinator richness per plant species, garden and sampling type
poll_richness_per_plant = interaction_data %>% 
  select(Botanical_garden, Plants, Pollinators, Sampling) %>%
  distinct() %>% 
  group_by(Botanical_garden, Plants, Sampling) %>%
  summarise(Poll_richness = n_distinct(Pollinators)) 

# Bind poll richness per plant and sampling time
richness_and_sampling_time = left_join(sampling_time, poll_richness_per_plant)
richness_and_sampling_time = richness_and_sampling_time %>% 
  mutate(Poll_richness_per_min = Poll_richness/Time) %>% 
  mutate(Plants = str_replace(Plants, " ", "_"))

# First, get phylogenetic information for plants
spp_list = interaction_data %>% 
  select(c(Plants, Plant_genus, Plant_family)) %>% 
  distinct() %>% 
  rename(species = Plants,
         genus = Plant_genus,
         family = Plant_family)

# Get phylo from megratree
plant_phylo = get_tree(sp_list = spp_list,  
                       taxon = "plant")

# Create vector of species of each garden
nested_plant_lists = interaction_data %>%
  group_by(Botanical_garden) %>%
  summarise(plant_vector = list(unique(Plants)), .groups = "drop") %>% 
  mutate(plant_vector = map(plant_vector, ~ gsub(" ", "_", .x)))

# ======================================================
# Function to prune tree for a given vector of species
prune_tree = function(tree, species_vector) {
  # Keep only the species that exist both in the tree and the vector
  species_to_keep = intersect(tree$tip.label, species_vector)
  
  # Drop tips not in the vector
  drop_tip_labels = setdiff(tree$tip.label, species_to_keep)
  
  pruned_tree = drop.tip(tree, drop_tip_labels)
  return(pruned_tree)
}

# Apply to each garden
garden_phylo_list = nested_plant_lists %>%
  mutate(pruned_tree = map(plant_vector, ~ prune_tree(plant_phylo, .x)))

# Check that it works
#plot(garden_phylo_list$pruned_tree[[1]])

# ======================================================
# Function to compute mean and median phylo distance
stats_tree = function(tree){

  # Compute pairwise distances
  dist_matrix <- cophenetic.phylo(tree)
  dist_values <- dist_matrix[upper.tri(dist_matrix)]
  
  # Return mean and median
  tibble(
    Mean_phylo_dist = mean(dist_values),
    Gini_phylo_dist = Gini(dist_values)
  )
  
}

# Get stats for pruned tree
garden_phylo_list1 = garden_phylo_list %>%
  mutate(stats = map(pruned_tree, stats_tree)) %>%
  unnest_wider(stats)
# ======================================================

# This function selects random plants from focal and sampling method
# Prunes the tree and computes the phylogenetic distance
# it returns a tibble with mean and median phylogenetic distance AND 
# provides poll richness for these set of species 
# Create function with cool names! :)

phylo_dist_randomizer = function(garden_name){
# Create vector of species of each garden
nested_plant_lists_by_sampling = interaction_data %>%
  group_by(Botanical_garden, Sampling) %>%
  summarise(plant_vector = list(unique(Plants)), .groups = "drop") %>% 
  mutate(plant_vector = map(plant_vector, ~ gsub(" ", "_", .x)))

# Filter by garden both trees and interaction data
tree_garden_x = nested_plant_lists_by_sampling %>% 
  filter(Botanical_garden == garden_name)
int_data_x = interaction_data %>% 
  filter(Botanical_garden == garden_name) %>% 
  select(Plants, Pollinators)

# Select 4 species from each sampling method
focal_x = tree_garden_x %>% 
  filter(Sampling == "Focal") %>% 
  pull(plant_vector) %>% 
  .[[1]]
random_census_x = tree_garden_x %>% 
  filter(Sampling == "Random_census")  %>% 
  pull(plant_vector) %>% 
  .[[1]]

# Create random pull of species from focal and random of each garden
focal_vector = sample(focal_x, 10)
random_vector = sample(random_census_x, 10)

# Prune tree with these random vectors
# Note prune_tree function was created before
pruned_focal_tree = prune_tree(plant_phylo, focal_vector)
pruned_random_tree = prune_tree(plant_phylo, random_vector)

# Compute metrics of pruned trees
focal_stats = stats_tree(pruned_focal_tree)
random_stats = stats_tree(pruned_random_tree)

# Get pollinator richnnes for these subset of species
focal_richness = int_data_x %>%  
  mutate(Plants = str_replace(Plants, " ", "_")) %>% 
  filter(Plants %in% focal_vector) %>% 
  summarise(n_poll = n_distinct(Pollinators))
random_richness = int_data_x %>%  
  mutate(Plants = str_replace(Plants, " ", "_")) %>% 
  filter(Plants %in% random_vector) %>% 
  summarise(n_poll = n_distinct(Pollinators))
# Get pollinator richnnes corrected by time
focal_richness_by_time = richness_and_sampling_time %>% 
  filter(Botanical_garden == garden_name, Sampling == "Focal") %>% 
  filter(Plants %in% focal_vector) %>% 
  ungroup() %>% 
  summarise(Poll_richness_per_min = mean(Poll_richness_per_min))

random_richness_by_time = richness_and_sampling_time %>% 
  filter(Botanical_garden == garden_name, Sampling == "Random_census") %>% 
  filter(Plants %in% focal_vector) %>% 
  ungroup() %>% 
  summarise(Poll_richness_per_min = mean(Poll_richness_per_min)) 

# Create final tibble 
focal_stats = focal_stats %>% 
  mutate(Sampling = "Focal") %>% 
  mutate(Poll_richness = focal_richness$n_poll) %>% 
  mutate(Poll_richness_per_min = focal_richness_by_time$Poll_richness_per_min)

random_stats = random_stats %>% 
  mutate(Sampling = "Random") %>% 
  mutate(Poll_richness = random_richness$n_poll) %>% 
  mutate(Poll_richness_per_min = focal_richness_by_time$Poll_richness_per_min)

# Bind both datasets
d = bind_rows(focal_stats, random_stats)

# Reorder cols 
d = d %>% 
  mutate(Botanical_garden = garden_name) %>% 
  select(Botanical_garden,
         Sampling, 
         Mean_phylo_dist, 
         Gini_phylo_dist, 
         Poll_richness,
         Poll_richness_per_min)
return(d)
}


# Define garden names
garden_names = c("Jena", "Halle", "Leipzig")

# Run the randomizer for each garden and iteration
results_100 = map_dfr(garden_names, function(garden) {
  map_dfr(1:100, function(i) {
    phylo_dist_randomizer(garden) %>%
      mutate(iteration = i)})
  })


results_100 %>% 
#  filter(Botanical_garden == "Halle") %>% 
  ggplot(aes(x = Mean_phylo_dist, y = Poll_richness, colour = Sampling)) +
  geom_point(alpha = 0.6, size = 2) +
  theme_minimal()

results_100 %>% 
  #  filter(Botanical_garden == "Halle") %>% 
  ggplot(aes(x = Mean_phylo_dist, y = Poll_richness_per_min, colour = Sampling)) +
  geom_point(alpha = 0.6, size = 2) +
  theme_minimal()


results_100 %>%
  ggplot(aes(x = Sampling, y = Mean_phylo_dist, fill = Sampling)) +
  geom_violin(alpha = 0.6) +
  geom_jitter(width = 0.1, alpha = 0.3, size = 1) +
  facet_wrap(~ Botanical_garden) +
  theme_minimal() +
  labs(title = "Distribution of Mean Phylogenetic Distance by Sampling Type")
