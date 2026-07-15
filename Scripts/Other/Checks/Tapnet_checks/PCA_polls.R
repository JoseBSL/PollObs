
# Load libraries
library(dplyr)
library(stringr)
library(phytools)

# Load data
bee_tree = readRDS("Data/Working_files/bee_phylo.rds")
bee_species = str_replace(bee_tree$tip.label, "_", " ")
polltraits_mean = readRDS("Data/Trait_data/Processed/PollTraits_all.rds")

# Organise trait data
traits = polltraits_mean %>%
  dplyr::filter(Pollinator_accepted_name %in% bee_species) %>% 
  arrange(match(Pollinator_accepted_name, bee_species)) %>%  # This ensures correct order
  dplyr::select(IT_mean, Body_length_mean, Proboscis_length_mean) %>% 
  arrange()

# Run Phylogenetically corrected PCA
phyl_pca_forest = phyl.pca(bee_tree, traits,method="lambda",mode="cov")

# Store data in a tibble
trait_axes_polls = as_tibble(phyl_pca_forest$S)
trait_axes_polls$Species = rownames(phyl_pca_forest$S)

#Fix some unmatched species names
trait_axes_polls1 = trait_axes_polls %>% 
  mutate(Species = str_replace(Species ,"_", " "))  %>% 
  select(Species, everything())

# Save data
saveRDS(trait_axes_polls1, "Data/Working_files/poll_trait_axes_phenobs.rds")
