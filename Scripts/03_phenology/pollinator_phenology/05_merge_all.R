#Bind pollinator phenology and explore it visually
#Load libraries
library(dplyr)
library(viridis)
library(tidyr)

#Load clean phenologies
#File 1
file1 = readRDS("Data/Working_files/poll_phenology_first_133_spp.rds")
#File 2
file2 = readRDS("Data/Working_files/poll_phenology_remaining_spp.rds")
#Bind together
all = bind_rows(file1, file2)
#Check number of species
unique(file1$Species)
#Select columns of interest
all = all %>% 
select(!c(k_value, m_value, prob_value))

checks = all %>% 
  filter(is.na(Probability)) %>% 
  distinct(Species) %>% 
  pull(Species)

checks

#Fix those species manually
#The day of appearance has been checked in the previous script
doy_spikes <- c(124, 222, 216, 235, 142)

spike_map <- tibble(
  Species = checks,
  Doy_spike = doy_spikes
)
# Apply: Probability = 1 only on that DOY, else 0 (for those species only)
all <- all %>%
  left_join(spike_map, by = "Species") %>%
  mutate(
    Probability = case_when(
      Species %in% checks & Doy == Doy_spike ~ 1,
      Species %in% checks                   ~ 0,
      TRUE                                  ~ Probability
    ),
    Abundances = case_when(
      Species %in% checks & Doy == Doy_spike ~ 1,
      Species %in% checks                   ~ 0,
      TRUE                                  ~ Abundances
    )
  ) %>%
  select(-Doy_spike)

#Save pollinator phenology
saveRDS(all, "Data/Working_files/pollinator_phenology.rds")

