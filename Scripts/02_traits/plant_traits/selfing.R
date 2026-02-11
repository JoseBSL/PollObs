#Prepare selfing data
#Load libraries
library(readr)
library(dplyr)
library(ggplot2)
#Load data
selfing = read_csv("Data/Trait_data/Raw/Selfing.csv")
#Aggregate data by single species
colnames(selfing)
average_selfing = selfing %>% 
group_by(Species, 
         Compatibility,
         Reference_compatibility_level,
         Aut_selfing_qualitative
         ) %>% 
summarise(Average_fruits = mean(Selfing_fruit_number),
          Average_seeds = mean(Selfing_seed_number))

#Filter out species with NA's and assign them manually
na_selfing = average_selfing %>% 
filter(is.na(Average_fruits))
#Assign those values based on qualitative data:
# 0%, 13%, 50.5% and 88%  (none, low, medium , high)
na_selfing1 = na_selfing %>% 
 mutate(Average_fruits = case_when(
   Aut_selfing_qualitative == "low" ~ 13,
   Aut_selfing_qualitative == "high" ~ 88,
 TRUE ~ Average_fruits))

#Check now non-na selfing
#Evaluate misleading fruit-sets that only produce abortive seeds
non_na_selfing = average_selfing %>% 
  filter(!is.na(Average_fruits))

#Check distribution
non_na_selfing %>% 
ggplot(aes(Average_fruits)) +
geom_histogram()
#Most species seem pollinator dependent 
#They produce little fruit set independently

#Bind NA and non_na back
selfing1 = bind_rows(na_selfing1, non_na_selfing)


#Save data
write_csv(selfing1, "Data/Trait_data/Processed/Selfing.csv")





