# Set seed for reproducibility
set.seed(123)

# Number of plant and pollinator species
n_plants <- 10
n_pollinators <- 10

# Create a random binary matrix (1 = interaction, 0 = no interaction)
net <- matrix(sample(0:1, n_plants * n_pollinators, replace = TRUE, prob = c(0.7, 0.3)),
                             nrow = n_plants, ncol = n_pollinators)

# Name rows and columns
rownames(net) <- paste0("Plant_", 1:n_plants)
colnames(net) <- paste0("Pollinator_", 1:n_pollinators)


#Network 1
# Calculate row and column degrees
row_degrees <- rowSums(net > 0)
col_degrees <- colSums(net > 0)

# Order rows and columns
ordered_rows <- order(row_degrees, decreasing = TRUE)
ordered_columns <- order(col_degrees, decreasing = TRUE)

# Reorder matrix based on degrees
ordered_net <- net[ordered_rows, ordered_columns]
rownames(ordered_net) <- rownames(net)[ordered_rows]
colnames(ordered_net) <- colnames(net)[ordered_columns]

# Create weights tibble
weights <- tibble(
  weight = c(col_degrees[ordered_columns], row_degrees[ordered_rows]),
  name = c(colnames(ordered_net), rownames(ordered_net)),
  trophic = c(rep("2", length(col_degrees[ordered_columns])), rep("1", length(row_degrees[ordered_rows]))) # Trophic levels
)

# Create layout for the bipartite graph
layout <- create_layout(ordered_net, "bipartite") %>%
  mutate(position = ifelse(y > 0, "upper", "lower"))


layout <- left_join(layout, weights, by = "name")

layout_false = layout %>%
  filter(type=="FALSE")
layout_true = layout %>%
  filter(type=="TRUE")

x_ordered_false = arrange(layout_false,x)
x_ordered_true = arrange(layout_true, x)
n_polls=layout_false %>% nrow(.)
n_plants=layout_true %>% nrow(.)
max_counts = max(c(n_polls, n_plants))

result_sequence_polls <- seq(from = 1, to = max_counts, length.out = n_polls)
result_sequence_plants <- seq(from = 1, to = max_counts, length.out = n_plants)

layout_false$x = result_sequence_polls
layout_true$x = result_sequence_plants

layout = bind_rows(layout_false, layout_true)



spp_vector = c("Plant_9", "Plant_7", "Plant_8")

layout <- layout %>%
  mutate(node_color = case_when(
    name %in% spp_vector ~ "yellow",
   TRUE ~ "black"))


str(layout)
layout[4]
attributes(layout)
V(layout)$graph
# Construct the ggraph
p1 = ggraph(layout) +
  geom_edge_link2(aes(edge_width = weight, color= from)) +
  geom_node_point(aes(shape = trophic, size = weight, color= node_color)) +  # Use 'weight' for size
  geom_node_text(aes(label = name,
                     hjust = ifelse(position == "upper", 0, 1)),
                 size = 1.5, angle = 90, vjust = 0.4, colour = "black") +
  scale_colour_identity() +
  scale_shape_manual(values = c(19, 19)) +
  scale_size_continuous(range = c(0.5, 4)) +
  scale_edge_width(range = c(0.5, 2.5)) +
  theme(
    panel.grid = element_blank(),
    panel.background = element_blank(),
    panel.border = element_blank(),
    plot.background = element_blank(),
    legend.position = "none",
    plot.margin = margin(t = 80, r = 10, b = 80, l = 10),
    plot.title = element_text(vjust=25)
  ) +
  coord_cartesian(expand = FALSE, clip = "off") 
  
p1
