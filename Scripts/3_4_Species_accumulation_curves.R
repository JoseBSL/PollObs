# ======================================================
#Compute species accumulation curves by garden and sampling type
# ======================================================

# 1) Accumulation curve for all gardens
# 2) Accumulation curve for all gardens but split
# phenobs and random sampling
# 3) Accumulation for each botanical garden considering phenobs and random sampling

# ======================================================
# Load libraries
library(dplyr)
library(iNEXT)
library(ggplot2)
library(patchwork)

# ======================================================
# Load data
raw_data = readRDS("Data/Working_files/interaction_data.rds")
# Check cols
colnames(raw_data)

# ======================================================
# 1) Prepare accumulation curve for all gardens
# Prepare interaction data for ALL 3 GARDENS
# Pollinator species observed on the different plants
int_data = raw_data %>% 
  filter(Plant_rank == "SPECIES", 
         Pollinator_rank == "SPECIES") %>% 
  select(Plant_accepted_name, Pollinator_accepted_name) %>% 
  rename(Plants = Plant_accepted_name,
         Pollinators = Pollinator_accepted_name) %>% 
  distinct() 

# Checks
# This should match the graph that I produce with iNext below
# (the total observed values of plants and pollinators)
int_data %>% 
  summarise(N_plants = n_distinct(Plants)) %>% 
  pull()

int_data %>% 
  summarise(N_plants = n_distinct(Pollinators)) %>% 
  pull()

# Count occurrences of each species in each study
incidence_tibble = int_data %>%
  count(Plants, Pollinators) %>% 
  group_by(Pollinators) %>% 
  summarise(Incidence = sum(n))

# Generate incidence matrix with number of sampling units at the beginning
incidence_matrix = matrix(c(length(unique(int_data$Plants)) , 
                 incidence_tibble %>% pull(Incidence)),  
               ncol = 1)
row.names(incidence_matrix) = c("Plants", incidence_tibble %>% pull(Pollinators))
incidence_matrix = data.frame(incidence_matrix)
colnames(incidence_matrix) = "Pollinators"
# Calculate sampling coverage
output = iNEXT(incidence_matrix, datatype = 'incidence_freq')
# Plot
ggiNEXT(output, type=1) 
# Store data to plot it with ggplot
output_data = output$iNextEst$coverage_based

output_data1 = output_data %>%  filter(!Method == "Observed")
output_data2 = output_data %>%  filter(Method == "Observed")

# Plot now with ggplot
# Set the factor levels in the desired order
output_data1$Method <- factor(output_data1$Method, levels = c("Rarefaction", "Extrapolation", "Observed"))
output_data2$Method <- "Observed"  # if not already set

# Combine both datasets to make sure factor levels align
combined_methods <- c(levels(output_data1$Method), "Observed")
output_data2$Method <- factor(output_data2$Method, levels = c("Rarefaction", "Extrapolation", "Observed"))

# Build the plot
all_gardens_plot = ggplot(output_data1, aes(x = t, y = qD, group = Method, color = Method)) +
  geom_line() +
  geom_ribbon(aes(ymin = qD.LCL, ymax = qD.UCL, fill = Method), alpha = 0.2, colour = NA, show.legend = FALSE) +
  geom_point(data = output_data2, aes(x = t, y = qD, color = Method), shape = 16) +
  scale_colour_manual(
    values = c("black", "gray", "red"),
    labels = c("Rarefaction", "Extrapolation", "Observed")
  ) +
  scale_linetype_manual(
    values = c("solid", "solid", "blank"),
    labels = c("Rarefaction", "Extrapolation", "Observed")
  ) +
  scale_fill_manual(values = c("black", "gray")) +
  ylab("Pollinator species") +
  xlab("Plant species") +
  theme_bw() +
  coord_cartesian(expand = FALSE) +
  guides(
    linetype = guide_legend(override.aes = list(linetype = c("solid", "solid", "blank"), shape = c(NA, NA, 16))))
all_gardens_plot


# ======================================================
# 2) Prepare accumulation curve for all gardens but split
# phenobs and random sampling
# first phenobs
int_data_phenobs = raw_data %>% 
  filter(Plant_rank == "SPECIES", 
         Pollinator_rank == "SPECIES",
         Sampling == "Focal") %>% 
  select(Plant_accepted_name, Pollinator_accepted_name) %>% 
  rename(Plants = Plant_accepted_name,
         Pollinators = Pollinator_accepted_name) %>% 
  distinct() 

# Checks
# This should match the graph that I produce with iNext below
# (the total observed values of plants and pollinators)
int_data_phenobs %>% 
  summarise(N_plants = n_distinct(Plants)) %>% 
  pull()
int_data_phenobs %>% 
  summarise(N_plants = n_distinct(Pollinators)) %>% 
  pull()

# Count occurrences of each species in each study
incidence_tibble_phenobs = int_data_phenobs %>%
  count(Plants, Pollinators) %>% 
  group_by(Pollinators) %>% 
  summarise(Incidence = sum(n))
# Generate incidence matrix with number of sampling units at the beginning
incidence_matrix_phenobs = matrix(c(length(unique(int_data_phenobs$Plants)) , 
                            incidence_tibble_phenobs %>% pull(Incidence)),  
                          ncol = 1)
row.names(incidence_matrix_phenobs) = c("Plants", incidence_tibble_phenobs %>% pull(Pollinators))
incidence_matrix = data.frame(incidence_matrix_phenobs)
colnames(incidence_matrix_phenobs) = "Pollinators"
# Calculate sampling coverage
output_phenobs = iNEXT(incidence_matrix_phenobs, datatype = 'incidence_freq')

# secong random sampling
int_data_random = raw_data %>% 
  filter(Plant_rank == "SPECIES", 
         Pollinator_rank == "SPECIES",
         Sampling == "Random_census") %>% 
  select(Plant_accepted_name, Pollinator_accepted_name) %>% 
  rename(Plants = Plant_accepted_name,
         Pollinators = Pollinator_accepted_name) %>% 
  distinct() 

# Checks
# This should match the graph that I produce with iNext below
# (the total observed values of plants and pollinators)
int_data_random %>% 
  summarise(N_plants = n_distinct(Plants)) %>% 
  pull()
int_data_random %>% 
  summarise(N_plants = n_distinct(Pollinators)) %>% 
  pull()

# Count occurrences of each species in each study
incidence_tibble_random = int_data_random %>%
  count(Plants, Pollinators) %>% 
  group_by(Pollinators) %>% 
  summarise(Incidence = sum(n))

# Generate incidence matrix with number of sampling units at the beginning
incidence_matrix_random = matrix(c(length(unique(int_data_random$Plants)) , 
                                    incidence_tibble_random %>% pull(Incidence)),  
                                  ncol = 1)
row.names(incidence_matrix_random) = c("Plants", incidence_tibble_random %>% pull(Pollinators))
incidence_matrix = data.frame(incidence_matrix_random)
colnames(incidence_matrix_random) = "Pollinators"
# Calculate sampling coverage
output_random = iNEXT(incidence_matrix_random, datatype = 'incidence_freq')

# Convert iNEXT output to df
output_phenobs_df <- output_phenobs$iNextEst$size_based 
output_random_df <- output_random$iNextEst$size_based

# Filter out observed to plot point separately
output_phenobs_df1 = output_phenobs_df %>%  filter(!Method == "Observed")
output_phenobs_df2 = output_phenobs_df %>%  filter(Method == "Observed")
output_phenobs_df1$Method = factor(output_phenobs_df1$Method, levels = c("Rarefaction", "Extrapolation", "Observed"))
# Filter out observed to plot point separately
output_random_df1 = output_random_df %>%  filter(!Method == "Observed")
output_random_df2 = output_random_df %>%  filter(Method == "Observed")
output_random_df1$Method <- factor(output_random_df1$Method, levels = c("Rarefaction", "Extrapolation", "Observed"))

# Add Sampling_method as a factor to both data frames
output_phenobs_df1$Sampling_method = "Phenobs"
output_phenobs_df2$Sampling_method = "Phenobs"
output_random_df1$Sampling_method = "Random"
output_random_df2$Sampling_method = "Random"

# Combine data frames for lines and points
lines_df <- bind_rows(output_phenobs_df1, output_random_df1)
points_df <- bind_rows(output_phenobs_df2, output_random_df2)

# Plot
ggplot(lines_df, aes(x = t, y = qD, group = interaction(Method, Sampling_method), 
                     color = Sampling_method, linetype = Method)) +
  geom_line() +
  geom_ribbon(aes(ymin = qD.LCL, ymax = qD.UCL, fill = Sampling_method), alpha = 0.2, colour = NA, show.legend = FALSE) +
  geom_point(data = points_df, aes(x = t, y = qD, color = Sampling_method, shape = Sampling_method), size = 3) +
  scale_colour_manual(
    values = c("Phenobs" = "purple", "Random" = "gray57"),
    name = "Sampling Method"
  ) +
  scale_fill_manual(
    values = c("Phenobs" = "purple", "Random" = "gray57")
  ) +
  scale_linetype_manual(
    values = c("Rarefaction" = "solid", "Extrapolation" = "dashed"),
    name = "Method"
  ) +
  scale_shape_manual(
    values = c("Phenobs" = 16, "Random" = 17),
    name = "Sampling Method"
  ) +
  ylab("Pollinator species") +
  xlab("Plant species") +
  theme_bw() +
  coord_cartesian(expand = FALSE) +
  guides(
    color = guide_legend(order = 1),
    linetype = guide_legend(order = 2),
    shape = guide_legend(order = 1)
  )


# ======================================================
# 3) Accumulation for each botanical garden considering phenobs and random sampling

plot_each_garden = function(garden_name){
int_data_phenobs = raw_data %>% 
  filter(Plant_rank == "SPECIES", 
         Pollinator_rank == "SPECIES",
         Sampling == "Focal",
         Botanical_garden == garden_name) %>% 
  select(Plant_accepted_name, Pollinator_accepted_name) %>% 
  rename(Plants = Plant_accepted_name,
         Pollinators = Pollinator_accepted_name) %>% 
  distinct() 
# Count occurrences of each species in each study
incidence_tibble_phenobs = int_data_phenobs %>%
  count(Plants, Pollinators) %>% 
  group_by(Pollinators) %>% 
  summarise(Incidence = sum(n))
# Generate incidence matrix with number of sampling units at the beginning
incidence_matrix_phenobs = matrix(c(length(unique(int_data_phenobs$Plants)) , 
                                    incidence_tibble_phenobs %>% pull(Incidence)),  
                                  ncol = 1)
row.names(incidence_matrix_phenobs) = c("Plants", incidence_tibble_phenobs %>% pull(Pollinators))
incidence_matrix = data.frame(incidence_matrix_phenobs)
colnames(incidence_matrix_phenobs) = "Pollinators"
# Calculate sampling coverage
output_phenobs = iNEXT(incidence_matrix_phenobs, datatype = 'incidence_freq')

# secong random sampling
int_data_random = raw_data %>% 
  filter(Plant_rank == "SPECIES", 
         Pollinator_rank == "SPECIES",
         Sampling == "Random_census", 
         Botanical_garden == garden_name) %>% 
  select(Plant_accepted_name, Pollinator_accepted_name) %>% 
  rename(Plants = Plant_accepted_name,
         Pollinators = Pollinator_accepted_name) %>% 
  distinct() 

# Count occurrences of each species in each study
incidence_tibble_random = int_data_random %>%
  count(Plants, Pollinators) %>% 
  group_by(Pollinators) %>% 
  summarise(Incidence = sum(n))

# Generate incidence matrix with number of sampling units at the beginning
incidence_matrix_random = matrix(c(length(unique(int_data_random$Plants)) , 
                                   incidence_tibble_random %>% pull(Incidence)),  
                                 ncol = 1)
row.names(incidence_matrix_random) = c("Plants", incidence_tibble_random %>% pull(Pollinators))
incidence_matrix = data.frame(incidence_matrix_random)
colnames(incidence_matrix_random) = "Pollinators"
# Calculate sampling coverage
output_random = iNEXT(incidence_matrix_random, datatype = 'incidence_freq')

# Convert iNEXT output to df
output_phenobs_df <- output_phenobs$iNextEst$size_based 
output_random_df <- output_random$iNextEst$size_based

# Filter out observed to plot point separately
output_phenobs_df1 = output_phenobs_df %>%  filter(!Method == "Observed")
output_phenobs_df2 = output_phenobs_df %>%  filter(Method == "Observed")
output_phenobs_df1$Method = factor(output_phenobs_df1$Method, levels = c("Rarefaction", "Extrapolation", "Observed"))
# Filter out observed to plot point separately
output_random_df1 = output_random_df %>%  filter(!Method == "Observed")
output_random_df2 = output_random_df %>%  filter(Method == "Observed")
output_random_df1$Method <- factor(output_random_df1$Method, levels = c("Rarefaction", "Extrapolation", "Observed"))

# Add Sampling_method as a factor to both data frames
output_phenobs_df1$Sampling_method = "Phenobs"
output_phenobs_df2$Sampling_method = "Phenobs"
output_random_df1$Sampling_method = "Random"
output_random_df2$Sampling_method = "Random"

# Combine data frames for lines and points
lines_df <- bind_rows(output_phenobs_df1, output_random_df1)
points_df <- bind_rows(output_phenobs_df2, output_random_df2)

all_data <- bind_rows(points_df, lines_df) %>% 
  mutate(Garden = garden_name)


# Plot
p = ggplot(lines_df, aes(x = t, y = qD, group = interaction(Method, Sampling_method), 
                     color = Sampling_method, linetype = Method)) +
  geom_line() +
  geom_ribbon(aes(ymin = qD.LCL, ymax = qD.UCL, fill = Sampling_method), alpha = 0.2, colour = NA, show.legend = FALSE) +
  geom_point(data = points_df, aes(x = t, y = qD, color = Sampling_method, shape = Sampling_method), size = 3) +
  scale_colour_manual(
    values = c("Phenobs" = "purple", "Random" = "gray57"),
    name = "Sampling Method"
  ) +
  scale_fill_manual(
    values = c("Phenobs" = "purple", "Random" = "gray57")
  ) +
  scale_linetype_manual(
    values = c("Rarefaction" = "solid", "Extrapolation" = "dashed"),
    name = "Method"
  ) +
  scale_shape_manual(
    values = c("Phenobs" = 16, "Random" = 17),
    name = "Sampling Method"
  ) +
  ylab("Pollinator species") +
  xlab("Plant species") +
  theme_bw() +
  coord_cartesian(expand = FALSE) +
  guides(
    color = guide_legend(order = 1),
    linetype = guide_legend(order = 2),
    shape = guide_legend(order = 1)
  )

return(list(plot = p, data = all_data))

}

# Create vector of gardens
gardens_vectors = unique(raw_data$Botanical_garden)
# Run function for each garden
results = map(gardens_vectors, plot_each_garden)
all_garden_data = bind_rows(map(results, "data"))
# Plot everything
final_plot <- reduce(map(results, "plot"), `+`) + plot_layout(guides = 'collect')
final_plot


# Use the data to plot phenobs and random in different plots
# but all gardens together
d = all_garden_data %>% 
  filter(Sampling_method == "Phenobs")
  
d_lines = d %>% 
  filter(!Method == "Observed")

d_points = d %>% 
  filter(Method == "Observed")

max_y = max(d_lines$qD.UCL)

p1 = ggplot(d_lines, aes(x = t, y = qD, group = interaction(Method, Garden), 
                    color = Garden, linetype = Method)) +
  geom_line() +
  geom_ribbon(aes(ymin = qD.LCL, ymax = qD.UCL, fill = Garden), 
              alpha = 0.2, colour = NA, show.legend = FALSE) +
  geom_point(data = d_points, aes(x = t, y = qD, color = Garden, shape = Garden), size = 3) +
  scale_color_viridis_d(name = "Garden") +
  scale_fill_viridis_d() +
  scale_linetype_manual(
    values = c("Rarefaction" = "solid", "Extrapolation" = "dashed"),
    name = "Method"
  ) +
  ylab("Pollinator species") +
  xlab("Plant species") +
  theme_bw() +
  coord_cartesian(expand = FALSE) +
  guides(
    color = guide_legend(order = 1),
    linetype = guide_legend(order = 2),
    shape = guide_legend(order = 1)) +
  ggtitle("PhenObs") +
  ylim(0, max_y)

p1

# Now do it for the random sampling

d = all_garden_data %>% 
  filter(Sampling_method == "Random")

d_lines = d %>% 
  filter(!Method == "Observed")

d_points = d %>% 
  filter(Method == "Observed")

p2= ggplot(d_lines, aes(x = t, y = qD, group = interaction(Method, Garden), 
                         color = Garden, linetype = Method)) +
  geom_line() +
  geom_ribbon(aes(ymin = qD.LCL, ymax = qD.UCL, fill = Garden), 
              alpha = 0.2, colour = NA, show.legend = FALSE) +
  geom_point(data = d_points, aes(x = t, y = qD, color = Garden, shape = Garden), size = 3) +
  scale_color_viridis_d(name = "Garden") +
  scale_fill_viridis_d() +
  scale_linetype_manual(
    values = c("Rarefaction" = "solid", "Extrapolation" = "dashed"),
    name = "Method"
  ) +
  ylab("Pollinator species") +
  xlab("Plant species") +
  theme_bw() +
  coord_cartesian(expand = FALSE) +
  guides(
    color = guide_legend(order = 1),
    linetype = guide_legend(order = 2),
    shape = guide_legend(order = 1)) +
  ggtitle("Random") +
  ylim(0, max_y)

p2


p1 + p2 + plot_layout(guides = 'collect')
