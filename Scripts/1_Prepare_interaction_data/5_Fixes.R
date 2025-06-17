# This is script is only to correct mistakes in raw data 

# Load library
library(dplyr)

# Load raw data
raw_data = readRDS("Data/Working_files/interaction_data.rds")

raw_data = raw_data %>% 
mutate(Pollinator_accepted_name = if_else(Pollinator_accepted_name == "Nona",
                                          NA_character_, Pollinator_accepted_name),
       Pollinator_genus = if_else(Pollinator_accepted_name == "Nona",
                                 NA_character_, Pollinator_genus),
       Pollinator_family = if_else(Pollinator_accepted_name == "Nona",
                                  NA_character_, Pollinator_family),
       Pollinator_order = if_else(Pollinator_accepted_name == "Nona",
                                   NA_character_, Pollinator_order),
       Pollinator_rank = if_else(Pollinator_accepted_name == "Nona",
                                  NA_character_, Pollinator_rank),
       Pollinator_status = if_else(Pollinator_accepted_name == "Nona",
                                   NA_character_, Pollinator_status),
       Pollinator_matchtype = if_else(Pollinator_accepted_name == "Nona",
                                   NA_character_, Pollinator_matchtype))

# Load raw data
saveRDS(raw_data, "Data/Working_files/interaction_data.rds")