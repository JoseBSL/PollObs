#Script to organize interaction dataset
#Add further taxonomic info and climatic data (temperature and humidity)

#Load libraries
library(dplyr) #To manipulate data
#Read data
interaction_data = read_csv("Data/PollObs_all.csv")
#Load taxonomy
#Load taxonomy
plants = readRDS("Data/Working_files/matched_gbif_plants.rds") 
polls = readRDS("Data/Working_files/matched_gbif_pollinators.rds") 

#Select coloumns of interest from interaction data
colnames(interaction_data)

#interaction_data = 
#interaction_data %>% 
#select(Botanical_garden, Plant, Pollinator,
#       Interactions, Floral_abundance, Capitulum,
#       Flowers_per_capitulum, Flowering_neighbours_intensity,
#       Time_start, Time_finish,
#       Total_time_species, Year, Month, Day,
#       Random_census_stop, Sampling)
#Add taxonomic info
interaction_data = left_join(interaction_data, plants)
interaction_data = left_join(interaction_data, polls)

#Add climatic data
#First organise data in POSIXct styles (date and time)
date_time = interaction_data %>% 
select(Year, Month, Day, Time_start) %>% 
mutate(Date = make_date(Year, Month, Day)) %>% 
mutate(Date1 = as.POSIXct(paste(Date, Time_start), format="%Y-%m-%d %H:%M")) %>% 
mutate(Date_time = round_date(Date1, "hour")) %>% 
select(Date_time)
interaction_data$Date_time = date_time$Date_time

#Convert date time to date
interaction_data = interaction_data %>% 
mutate(Date = as.Date(Date_time)) 

#Fix one record that is out of phenology
#Anthophora plumipes 2023-07-04 is likey Anthophora quadrimaculata
interaction_data = interaction_data %>% 
mutate(Pollinator = case_when(
  Pollinator == "Anthophora plumipes" & Date == "2023-07-04" ~ "Anthophora quadrimaculata",
  TRUE ~ Pollinator))
interaction_data = interaction_data %>% 
mutate(Pollinator_accepted_name = case_when(
  Pollinator_accepted_name == "Anthophora plumipes" & Date == "2023-07-04" ~ "Anthophora quadrimaculata",
  TRUE ~ Pollinator_accepted_name))

library(stringr)

strings = c("Like", "like", "red bee")

#Create label 1 with ID, date, location, plant and observer
label = interaction_data %>% 
select(Pollinator_id, Day, Month, Year, Botanical_garden, 
       Plant, Observer, Pollinator, ID_by, Pollinator_order) %>% 
filter(!is.na(Pollinator_id)) %>% 
filter(!str_detect(Pollinator_id, paste(strings, collapse = "|")))
#Format label 1
label_1_formatted = label %>% 
mutate(Month = as.roman(Month)) %>% 
mutate(Date = paste0(Day, "/", Month, "/", Year)) %>% 
select(!c(Day, Month, Year)) %>% 
mutate(Botanical_garden = paste(Botanical_garden, "BG")) %>% 
mutate(Observer = if_else(str_detect(Observer, "&"), "JB Lanuza", Observer)) %>% 
mutate(Observer = paste("leg.", Observer)) %>% 
mutate(Country = "Germany") %>% 
select(Pollinator_id, Date, Country, Botanical_garden, Plant, Observer)

#Create label 2 with ID, date, location, plant and observer
label_2_formatted = label %>% 
select(Pollinator, ID_by, Pollinator_id, Pollinator_order) %>% 
filter(!str_detect(Pollinator_id, "p")) %>% 
mutate(ID_by = case_when(
  is.na(ID_by) & Pollinator_order == "Hymenoptera" ~ "M Hoffmann",
  is.na(ID_by) & Pollinator_order == "Diptera" ~ "J Cobain",
  is.na(ID_by) & Pollinator_order == "Coleoptera" ~ "HF Morgenroth",
  is.na(ID_by) & Pollinator_order == "Lepidoptera" ~ "HF Morgenroth",
  TRUE ~ ID_by)) %>% 
select(Pollinator, ID_by) %>% 
mutate(ID_by = paste("det", ID_by, "2024"))

#
checks = label_2_formatted %>% 
distinct(Pollinator)
library(rgbif)

poll_spp = checks %>% 
mutate(Pollinator = str_replace(Pollinator, " sp", "")) %>% 
pull() 

#Check for futher taxonomic info
gbif_pollinators = name_backbone_checklist(name = poll_spp, kingdom='arthropoda')
#Select cols of interest
gbif_poll = gbif_pollinators %>% 
select(scientificName, rank, verbatim_name) 
#Divide into species and others
gbif_poll_species = gbif_poll %>% 
filter(rank == "SPECIES")
gbif_poll_other = gbif_poll %>% 
filter(!rank == "SPECIES")
#This needs to be also divided in the ones that have parenthesis and not
gbif_poll_species_parenthesis = gbif_poll_species %>% 
filter(str_detect(scientificName, "\\)")) %>% 
select(verbatim_name, scientificName)

gbif_poll_species_without_parenthesis = gbif_poll_species %>% 
filter(!str_detect(scientificName, "\\)")) %>% 
 tidyr::extract(scientificName, into = c("V1", "V2"), "^(\\S+\\s+\\S+)\\s+(.*)") %>% 
mutate(V2 = if_else(V2 == "E.Stöckhert, 1928", "Stöckhert, 1928", V2)) %>% 
mutate(V2 = if_else(V1 == "Nowickia ferox", "Panzer, 1809", V2)) %>% 
mutate(V2 = if_else(V1 == "Sarcophaga subvicina", "Baranov, 1937", V2)) %>% 
mutate(V2 = paste0("(", V2, ")")) %>% 
mutate(scientificName = paste(V1, V2)) %>% 
select(verbatim_name, scientificName)
  
gbif_species_clean = rbind(gbif_poll_species_parenthesis, 
                           gbif_poll_species_without_parenthesis)

#Now prepare the ones without species level
gbif_other_clean =  gbif_poll_other %>% 
mutate(scientificName = if_else(verbatim_name == "Lasioglossum", 
        "Lasioglossum sp. (Curtis, 1833)", scientificName)) %>% 
mutate(scientificName = if_else(verbatim_name == "Diptera", 
        "Diptera sp. (Linnaeus, 1758)", scientificName)) %>% 
mutate(scientificName = if_else(verbatim_name == "Hymenoptera", 
        "Hymenoptera sp. (Linnaeus, 1758)", scientificName)) %>% 
mutate(scientificName = if_else(verbatim_name == "Megachile", 
        "Megachile sp. (Latreille, 1802)", scientificName))  %>% 
mutate(scientificName = if_else(verbatim_name == "Oedemera", 
        "Oedemera sp. (Olivier, 1789)", scientificName))%>% 
mutate(scientificName = if_else(verbatim_name == "Tephritidae", 
        "Tephritidae sp. (Newman, 1834)", scientificName)) %>% 
mutate(scientificName = if_else(verbatim_name == "Hemiptera", 
        "Hemiptera sp. (Linnaeus, 1758)", scientificName)) %>% 
mutate(scientificName = if_else(verbatim_name == "Mordellidae", 
        "Mordellidae sp. (Latreille, 1802)", scientificName)) %>% 
mutate(scientificName = if_else(verbatim_name == "Angioneura", 
        "Angioneura sp. (Brauer & Bergenstamm, 1893)", scientificName)) %>% 
mutate(scientificName = if_else(verbatim_name == "Anthrenus", 
        "Anthrenus sp. (Geoffroy, 1762)", scientificName)) %>% 
mutate(scientificName = if_else(verbatim_name == "Sarcophaga", 
        "Sarcophaga sp. (Meigen, 1826)", scientificName))  %>% 
mutate(scientificName = if_else(verbatim_name == "Metopia", 
        "Metopia sp. (Meigen, 1803)", scientificName))  %>% 
mutate(scientificName = if_else(verbatim_name == "Hylaeus", 
        "Hylaeus sp. (Fabricius, 1793)", scientificName)) %>% 
mutate(scientificName = if_else(verbatim_name == "Bruchidae", 
        "Bruchidae sp. (Latreille, 1802)", scientificName)) %>% 
mutate(scientificName = if_else(verbatim_name == "Megachilidae", 
        "Megachilidae sp. (Latreille, 1802)", scientificName)) %>% 
mutate(scientificName = if_else(verbatim_name == "Pseudocoenosia", 
        "Pseudocoenosia sp. (Stein, 1916)", scientificName)) %>% 
mutate(scientificName = if_else(verbatim_name == "Melangyna", 
        "Melangyna sp. (Verrall, 1901)", scientificName))%>% 
mutate(scientificName = if_else(verbatim_name == "Cheilosia", 
        "Cheilosia sp. (Meigen, 1822)", scientificName)) %>% 
select(!rank)

gbif_clean = rbind(gbif_species_clean, gbif_other_clean)
gbif_clean = gbif_clean %>% 
rename(Pollinator = verbatim_name) %>% 
distinct()

label_2_formatted = label_2_formatted %>% 
mutate(Pollinator = str_replace(Pollinator, " sp", ""))

label_2_formatted_clean = left_join(label_2_formatted, gbif_clean, by = "Pollinator")

