################################################################
#Compute Mantel test between int and int freq and prob abundance matrix
################################################################

library(dplyr)
library(purrr)
library(tidyr)
library(ggplot2)
library(vegan) #for mantes and procrustes

#Read network data
prob_matrices_by_garden = readRDS("Data/Working_files/abundance_networks_only_phenobs_by_season.rds")

mantel_interaction_abundance = function(garden_name, season_name) {
  
  interaction_network = prob_matrices_by_garden %>% 
    filter(Botanical_garden == garden_name, Season == season_name) %>% 
    select(Interaction_network) %>% 
    pull(Interaction_network) %>% 
    .[[1]]
  
  abundance_network = prob_matrices_by_garden %>% 
    filter(Botanical_garden == garden_name, Season == season_name) %>% 
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
                Season = season_name,
                Test = "Visits-Abundance",
                Mantel_corr = mantel_result$statistic,
                Mantel_Pval = mantel_result$signif))
  
}


mantel_interactionFreq_abundance = function(garden_name, season_name) {
  
  interaction_network = prob_matrices_by_garden %>% 
    filter(Botanical_garden == garden_name, Season == season_name) %>% 
    select(Int_frequency_network) %>% 
    pull(Int_frequency_network) %>% 
    .[[1]]
  
  abundance_network = prob_matrices_by_garden %>% 
    filter(Botanical_garden == garden_name, Season == season_name) %>% 
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
                Season = season_name,
                Test = "VisitRate-Abundance",
                Mantel_corr = mantel_result$statistic,
                Mantel_Pval = mantel_result$signif))
  
}

#Run both functions now
#First create vector of gardens
garden_season_combos = prob_matrices_by_garden %>% 
  select(Botanical_garden, Season) %>% 
  distinct()
#Run function int-abund on each garden
results_int_abund = pmap_dfr(
  list(
    garden_name = garden_season_combos$Botanical_garden,
    season_name = garden_season_combos$Season
  ),
  mantel_interaction_abundance
)

results_intFreq_abund = pmap_dfr(
  list(
    garden_name = garden_season_combos$Botanical_garden,
    season_name = garden_season_combos$Season
  ),
  mantel_interactionFreq_abundance
)
# Combine results
combined_results = bind_rows(results_int_abund, results_intFreq_abund)

combined_results$Mantel_corr = abs(combined_results$Mantel_corr)

combined_results = combined_results %>%
  mutate(Season = factor(Season, levels = c("Early", "Mid", "Late")))

# Plot
ggplot(combined_results, aes(x = Season, 
                             y = Mantel_corr, 
                             fill = Test)) +
  geom_bar(aes(group = interaction(Season, Test)), 
           stat = "identity", 
           position = position_dodge(width = 0.8)) +
  facet_wrap(~Botanical_garden, nrow = 1) +
  theme_minimal() +
  labs(x = "Botanical Garden", 
       y = "Mantel Correlation", 
       title = "Mantel correlation by Garden, Season and Test",
       fill = "Test Type") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylim(0, 1)






