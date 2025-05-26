# Script to compute the percentage of missing links due to phenological mistmatches
# Load libraries
library(dplyr)
library(reshape2)

# Load data
pheno_prob_matrices_by_garden <- readRDS("Data/Working_files/phenology_networks_only_phenobs_pheno.rds")

# Function to compute forbidden links percentage
phenological_forbidden_links <- function(garden_name) {
  
  # Extract network matrix for the selected garden
  network <- pheno_prob_matrices_by_garden %>% 
    filter(Botanical_garden == garden_name) %>% 
    pull() %>% 
    .[[1]]
  
  # Convert matrix to long format
  network_long_format <- melt(network, varnames = c("Row", "Column"), value.name = "Value")
  
  # Count non-NA links
  rows <- network_long_format %>% 
    filter(!is.na(Value)) %>% 
    nrow()
  
  # Count forbidden (zero) links
  zero_rows <- network_long_format %>% 
    filter(!is.na(Value)) %>% 
    filter(Value == 0) %>% 
    nrow()
  
  # Compute percentage of forbidden links
  forbidden_percentage <- zero_rows / rows * 100
  
  return(forbidden_percentage)
}


phenological_forbidden_links("Jena")
phenological_forbidden_links("Halle")
phenological_forbidden_links("Leipzig")

