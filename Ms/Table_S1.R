

# Load libraries
library(dplyr)
library(tidyr)

# Interaction data
raw_data = readRDS("Data/Working_files/interaction_data.rds")
# Load Phenobs species
phenobs_spp = readRDS("Data/Working_files/phenobs_spp.rds")

# Filter data 
plant_data = raw_data %>% 
  filter(Sampling =="Focal") %>% 
  filter(Plant_rank == "SPECIES") %>% 
  select(Botanical_garden, 
         Plant, 
         Plant_accepted_name,
         Plant_genus,
         Plant_family,
         Plant_order) %>% 
filter(Plant %in% phenobs_spp)

# Get total time per species and garden
sampling_time <- raw_data %>% 
  filter(Sampling == "Focal",
         Plant_rank == "SPECIES",
         Plant %in% phenobs_spp) %>%
  select(
    Botanical_garden, 
    Plant_accepted_name, 
    Total_time_species,
    Date
  ) %>% 
  distinct() %>% 
  group_by(Botanical_garden, Plant_accepted_name) %>% 
  summarise(
    Sampling_time = sum(Total_time_species, na.rm = TRUE),
    .groups = "drop"
  )

# Taxonomic information
tax_info <- raw_data %>% 
  filter(Sampling == "Focal",
         Plant_rank == "SPECIES",
         Plant %in% phenobs_spp) %>%
  distinct(
    Plant_accepted_name,
    Plant_genus,
    Plant_family,
    Plant_order
  )

readr::write_csv(tax_info, "Data/Working_files/tax_info.csv")


# Wide table with sampling time instead of 1/0
shared_wide <- sampling_time %>%
  pivot_wider(
    names_from = Botanical_garden,
    values_from = Sampling_time,
    values_fill = 0
  ) %>%
  left_join(tax_info, by = "Plant_accepted_name") %>%
  select(
    Plant_accepted_name,
    Plant_genus,
    Plant_family,
    Plant_order,
    Leipzig, Halle, Jena
  ) %>%
  rename(
    Species = Plant_accepted_name,
    Genus = Plant_genus,
    Family = Plant_family,
    Order = Plant_order
  ) %>%
  arrange(Species)

shared_wide <- shared_wide %>%
  mutate(Total_sampling_time = Leipzig + Halle + Jena)
shared_wide <- shared_wide %>%
  mutate(n_gardens = rowSums(across(c(Leipzig, Halle, Jena), ~ .x > 0)))
shared_wide <- shared_wide %>%
  mutate(
    Total_sampling_time = Leipzig + Halle + Jena,
    n_gardens = rowSums(across(c(Leipzig, Halle, Jena), ~ .x > 0))
  )
shared_wide

readr::write_csv(shared_wide, "Data/Working_files/shared_wide.csv")

# Check non-phenobs species
# Filter data 
plant_data = raw_data %>% 
  filter(Sampling =="Random_census") %>% 
  filter(Plant_rank == "SPECIES") %>% 
  select(Botanical_garden, 
         Plant, 
         Plant_accepted_name,
         Plant_genus,
         Plant_family,
         Plant_order) %>% 
  filter(!Plant %in% phenobs_spp)

plant_data %>% 
  summarise(n_distinct(Plant_accepted_name))

shared_wide %>% 
  select(Species, Leipzig) %>% 
  filter(Leipzig== 0)
78-31


raw_data = readRDS("Data/Working_files/interaction_data.rds")

random = raw_data %>% 
  filter(Sampling=="Random_census") %>% 
  group_by(Botanical_garden, Date, Random_census_stop) %>% 
  summarise(N = n_distinct(Plant_accepted_name))
mean(random$N)


# Taxonomic information
tax_info_non_phenobs <- raw_data %>% 
  filter(Sampling == "Random_census",
         Plant_rank == "SPECIES") %>% 
  filter(!Plant %in% phenobs_spp) %>%
  distinct(
    Botanical_garden,
    Plant_accepted_name,
    Plant_genus,
    Plant_family,
    Plant_order
  )

tax_info_wide <- tax_info_non_phenobs %>%
  mutate(present = 1) %>%
  pivot_wider(
    names_from = Botanical_garden,
    values_from = present,
    values_fill = 0
  )

non_phenobs_time <- raw_data %>% 
  filter(Sampling == "Random_census",
         Plant_rank == "SPECIES",
         Plant %in% phenobs_spp) %>%
  select(
    Botanical_garden,
    Plant_accepted_name,
    Plant_genus,
    Plant_family,
    Plant_order,
    Total_time_species,
    Date
  ) %>%
  distinct() %>%
  group_by(
    Botanical_garden,
    Plant_accepted_name,
    Plant_genus,
    Plant_family,
    Plant_order
  ) %>%
  summarise(
    Sampling_time = sum(Total_time_species, na.rm = TRUE),
    .groups = "drop"
  )

non_phenobs_wide <- non_phenobs_time %>%
  pivot_wider(
    names_from = Botanical_garden,
    values_from = Sampling_time,
    values_fill = 0
  ) %>%
  mutate(
    Total_sampling_time = Leipzig + Halle + Jena,
    n_gardens = rowSums(across(c(Leipzig, Halle, Jena), ~ .x > 0))
  ) %>%
  select(
    Plant_accepted_name,
    Plant_genus,
    Plant_family,
    Plant_order,
    Leipzig, Halle, Jena,
    Total_sampling_time )

sum(non_phenobs_wide$Total_sampling_time)

non_phenobs_time %>% 
  summarise(Species = n_distinct(Plant_accepted_name),
            Genus = n_distinct(Plant_genus),
            Family = n_distinct(Plant_family),
            Order = n_distinct(Plant_order))

readr::write_csv(non_phenobs_wide, "Data/Working_files/non_phenobs_wide.csv")



# Interaction data
raw_data = readRDS("Data/Working_files/interaction_data.rds")
main_ord = c("Hymenoptera", "Diptera", "Lepidoptera", "Coleoptera")

check = raw_data %>% 
  filter(Pollinator_rank == "SPECIES") %>% 
  #filter(Pollinator_order %in% main_ord) %>% 
  distinct(Pollinator_accepted_name) 

check = raw_data %>% 
  filter(Pollinator_rank == "SPECIES") %>% 
  #filter(Pollinator_order %in% main_ord) %>% 
 # summarise(N = n_distinct(Pollinator_accepted_name))
  summarise(Genus = n_distinct(Pollinator_genus),
            Family = n_distinct(Pollinator_family),
            Order = n_distinct(Pollinator_order))

raw_data %>% 
  select(Pollinator_order) %>% 
  distinct()

raw_data %>% 
  filter(Pollinator_rank == "SPECIES")
  group_by(Pollinator_order) %>% 
  summarise(n_distinct(Pollinator_accepted_name))

raw_data %>% 
  group_by(Pollinator_order) %>% 
  sampling_time
  summarise(n_distinct(Pollinator_accepted_name))



order_levels <- c("Hymenoptera", "Diptera", "Coleoptera", "Lepidoptera", "Hemiptera")

check <- raw_data %>% 
  filter(Pollinator_rank == "SPECIES") %>% 
  select(
    Pollinator_accepted_name,
    Pollinator_genus,
    Pollinator_family,
    Pollinator_order,
    Botanical_garden
  ) %>% 
  distinct() %>%
  mutate(
    garden_code = case_when(
      Botanical_garden == "Halle" ~ "H",
      Botanical_garden == "Jena" ~ "J",
      Botanical_garden == "Leipzig" ~ "L"
    )
  ) %>%
  group_by(
    Pollinator_accepted_name,
    Pollinator_genus,
    Pollinator_family,
    Pollinator_order
  ) %>%
  summarise(
    gardens = paste(sort(unique(garden_code)), collapse = ""),
    .groups = "drop"
  ) %>%
  mutate(
    Pollinator_order = factor(Pollinator_order, levels = order_levels)
  ) %>%
  arrange(
    Pollinator_order,
    Pollinator_family,
    Pollinator_genus,
    Pollinator_accepted_name
  )
check
readr::write_csv(check, "Data/Working_files/poll_table.csv")



raw_data %>% 
  filter(Pollinator_rank == "SPECIES") %>% 
  filter(Sampling == "Focal") %>% 
  select(
    Pollinator_accepted_name,
    Pollinator_genus,
    Pollinator_family,
    Pollinator_order,
    Botanical_garden
  ) %>% 
  distinct() %>% 
 # group_by(Botanical_garden) %>% 
  summarise(n =n_distinct(Pollinator_accepted_name))



focal <- raw_data %>% 
  filter(Pollinator_rank == "SPECIES",
         Sampling == "Focal") %>% 
  select(Botanical_garden, Pollinator_accepted_name) %>% 
  distinct()

random <- raw_data %>% 
  filter(Pollinator_rank == "SPECIES",
         Sampling == "Random_census") %>% 
  select(Botanical_garden, Pollinator_accepted_name) %>% 
  distinct()

random %>% 
  distinct(Pollinator_accepted_name)

additional <- anti_join(
  random,
  focal,
  by = c("Botanical_garden", "Pollinator_accepted_name")
)

additional %>%
  group_by(Botanical_garden) %>%
  summarise(n_additional = n_distinct(Pollinator_accepted_name))



raw_data %>% 
  filter(Pollinator_rank == "SPECIES",
         Plant_rank == "SPECIES") %>% 
  filter(Sampling == "Focal") %>% 
  group_by(Botanical_garden) %>% 
  summarise(n_interactions = n())


c = raw_data %>% 
  filter(Pollinator_rank == "SPECIES",
         Plant_rank == "SPECIES") %>% 
  group_by(Pollinator_order) %>% 
  summarise(n_interactions = n()/nrow(.)*100)

sum(c$n_interactions)
c

apis_int / nrow(raw_data) *100

apis_int = raw_data %>% 
  filter(Pollinator_rank == "SPECIES",
         Plant_rank == "SPECIES") %>% 
  filter(Pollinator_accepted_name == "Apis mellifera") %>% 
  nrow()


c <- raw_data %>% 
  filter(Pollinator_rank == "SPECIES",
         Plant_rank == "SPECIES") %>% 
  group_by(Botanical_garden, Pollinator_order) %>% 
  summarise(n_interactions = n(), .groups = "drop") %>% 
  group_by(Botanical_garden) %>% 
  mutate(perc = n_interactions / sum(n_interactions) * 100)
c
