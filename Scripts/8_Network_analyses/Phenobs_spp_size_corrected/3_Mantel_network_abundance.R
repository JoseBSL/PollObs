################################################################
#Compute Mantel test between int and int freq and prob abundance matrix
################################################################

library(dplyr)
library(purrr)
library(tidyr)
library(ggplot2)
library(vegan) #for mantes and procrustes

#Read network data
prob_matrices_by_garden = readRDS("Data/Working_files/abundance_networks_only_phenobs_corrected_by_size.rds")

mantel_interaction_abundance = function(garden_name) {
  
  interaction_network = prob_matrices_by_garden %>% 
    filter(Botanical_garden == garden_name) %>% 
    select(Interaction_network) %>% 
    pull(Interaction_network) %>% 
    .[[1]]
  
  abundance_network = prob_matrices_by_garden %>% 
    filter(Botanical_garden == garden_name) %>% 
    select(Prob_matrix) %>% 
    pull(Prob_matrix) %>% 
    .[[1]]
  
  #Convert matrices to distance matrices (Euclidean distance here)
  dist1 = dist(interaction_network)
  dist2 = dist(abundance_network)
  #Perform the Mantel test
  mantel_result = mantel(dist1, dist2, method = "pearson", permutations = 999)
  mantel_result$statistic
  mantel_result$signif
  
  return(tibble(Botanical_garden = garden_name, 
                Test = "Visits-Abundance",
                Mantel_corr = mantel_result$statistic,
                Mantel_Pval = mantel_result$signif))
  
}


mantel_interactionFreq_abundance = function(garden_name) {
  
  interaction_network = prob_matrices_by_garden %>% 
    filter(Botanical_garden == garden_name) %>% 
    select(Int_frequency_network) %>% 
    pull(Int_frequency_network) %>% 
    .[[1]]
  
  abundance_network = prob_matrices_by_garden %>% 
    filter(Botanical_garden == garden_name) %>% 
    select(Prob_matrix) %>% 
    pull(Prob_matrix) %>% 
    .[[1]]
  
  #Convert matrices to distance matrices (Euclidean distance here)
  dist1 = dist(interaction_network)
  dist2 = dist(abundance_network)
  #Perform the Mantel test
  mantel_result = mantel(dist1, dist2, method = "pearson", permutations = 999)
  mantel_result$statistic
  mantel_result$signif
  
  return(tibble(Botanical_garden = garden_name, 
                Test = "VisitRate-Abundance",
                Mantel_corr = mantel_result$statistic,
                Mantel_Pval = mantel_result$signif))
  
}

#Run both functions now
#First create vector of gardens
gardens = unique(prob_matrices_by_garden$Botanical_garden)

#Run function int-abund on each garden
results_int_abund = map_dfr(gardens, mantel_interaction_abundance)
#Check results
results_int_abund
#Run function int freq-abundance on each garden
results_intFreq_abund = map_dfr(gardens, mantel_interactionFreq_abundance)
#Check results
results_intFreq_abund

#Bind rows
combined_results = bind_rows(results_int_abund, results_intFreq_abund)

ggplot(combined_results, aes(x = Botanical_garden, y = Mantel_corr, fill = Test)) +
  geom_bar(stat = "identity", position = "dodge") + 
  theme_minimal() +
  labs(x = "Botanical Garden", y = "Mantel Correlation",
       title = "Visits-Abund. vs VisitationRate-Abund.") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

