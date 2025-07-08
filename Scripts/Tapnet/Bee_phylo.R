# Phylogenetic data:
# Henríquez-Piskulich, P.; Hugall, A.F.; Stuart-Fox; D. 2024.
# A supermatrix phylogeny of the world’s bees (Hymenoptera: Anthophila). 
# Molecular Phylogenetics and Evolution 190, 107963. doi:10.1016/j.ympev.2023.107963.

# Load library
library(ape)
library(phytools)
# Read tree
ml_tree = readRDS("Data/Working_files/Supertree_bees.rds")

# Prepare bee species from data
# Load data
# Interaction data
raw_data = readRDS("Data/Working_files/interaction_data.rds")
# Phenobs spp
phenobs_spp = readRDS("Data/Working_files/phenobs_spp.rds")

# Create vector of main bee families
bee_fam = c("Megachilidae", 
            "Apidae",
            "Colletidae",
            "Andrenidae",
            "Halictidae",
            "Mellittidae")

# ======================================================
# Prepare interaction data
interaction_data = raw_data %>%
  filter(!is.na(Interactions),
         !is.na(Floral_abundance),
         Pollinator != "None") %>%
  rename(Plants = Plant_accepted_name,
         Pollinators = Pollinator_accepted_name) %>%
  filter(!is.na(Pollinators)) %>% 
  filter(Pollinator_rank == "SPECIES") %>% 
  filter(Sampling == "Focal") %>% 
  # filter(!Pollinators == "Apis mellifera") %>% 
  filter(Pollinator_family %in% bee_fam) %>% 
  filter(!Plants == "Iberis sempervirens") 

interaction_data = interaction_data %>% 
  filter(Plant %in% phenobs_spp) 

# Second, get phylogenetic information for pollinators
poll_taxonomic_info = interaction_data %>% 
  select(c(Pollinators, Pollinator_genus, Pollinator_family, Pollinator_order)) %>% 
  distinct() %>% 
  rename(species = Pollinators,
         genus = Pollinator_genus,
         family = Pollinator_family,
         order = Pollinator_order) %>% 
  select(order, family, genus, species)

# Prepare species with underscore to match phylo
poll_taxonomic_info$full_name = str_replace(poll_taxonomic_info$species, " ", "_")
# Get tip labels
tree_tips = ml_tree$tip.label
# Find which species from your list are in the tree
matching_species = poll_taxonomic_info$full_name[poll_taxonomic_info$full_name %in% tree_tips]
# Unmatched species
unmatching_species = poll_taxonomic_info$full_name[!poll_taxonomic_info$full_name %in% tree_tips]
# Keep tip
pruned_tree = keep.tip(ml_tree, matching_species)
# Plot
ggtree(pruned_tree, size=0.1, open.angle=5, alpha=0.5) +
  geom_tippoint(colour='cyan4') +
  geom_tiplab(linetype='dashed', linesize=.05, 
              size=1.75, color= "black", offset = 0.2, fontface=2) +
  theme(plot.margin = margin(5, 50, 5, 5)) +
  coord_cartesian(clip = "off")

# I need to add only a single species, at the moment is being added like this
## Function for adding a cherry to a tree where a single tip was before
add.cherry <- function(tree, tip, new.tips) {
  
  ## Find the edge leading to the tip
  tip_id <- match(tip, tree$tip.label)
  
  ## Create the new cherry
  tree_to_add <- ape::stree(length(c(tip, new.tips)))
  
  ## Naming the tips
  tree_to_add$tip.label <- c(tip, new.tips)
  
  ## Add 0 branch length
  tree_to_add$edge.length <- rep(0, Nedge(tree_to_add))
  
  ## Binding both trees
  return(bind.tree(tree, tree_to_add, where = tip_id))
}

## Adding a new sister taxon to t6 (with a 0 branch length)
new_tree = add.cherry(pruned_tree, tip = "Andrena_pilipes", new.tips = unmatching_species)
ggtree(new_tree, size=0.1, open.angle=5, alpha=0.5) +
  geom_tippoint(colour='cyan4') +
  geom_tiplab(linetype='dashed', linesize=.05, 
              size=1.75, color= "black", offset = 0.2, fontface=2) +
  theme(plot.margin = margin(5, 50, 5, 5)) +
  coord_cartesian(clip = "off")

saveRDS(new_tree, "Data/Working_files/bee_phylo.rds")


pruned_tree$tip.label
