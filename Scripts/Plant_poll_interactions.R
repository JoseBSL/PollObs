#Botanical garden plant-pollinator dataset
#Dataset description----
#Plant-pollinator interactions from 3 different cities Jena-Halle-Leipzig
#Single flowering season (Year 2023)
#Two methods of sampling: focal observations and random census
#We recorded interaction frequency and number of flowers of each species per unit of time
#In addition, focal species have the approximate number of surrounding flowers in a 1 meter radius


#Download plant-pollinator data
library(googlesheets4)
library(dplyr) #To process data
library(rgbif) #To extract taxonomic information
library(stringr) #To process data
library(ggplot2) #For plotting
library(lubridate) #To manipulate dates
library(readr)
#Read data/takes a while
#data <- read_sheet("https://docs.google.com/spreadsheets/d/1K9zR8M8hpCJEoiSx1oGjaB4PNSD062S5cVdojNjnFpU/edit#gid=2114344765", 
#                         sheet = "Plant_poll_interactions",  na = "NA")

data = read_csv("Data/PollObs_all.csv")

#Create list of plants
#plant_list <- read_sheet("https://docs.google.com/spreadsheets/d/1K9zR8M8hpCJEoiSx1oGjaB4PNSD062S5cVdojNjnFpU/edit#gid=2114344765", 
#                         sheet = "Plant_Species")

plant_list = read_csv("Data/PollObs_species.csv")


#Generacte vector with species names
spp_list = plant_list %>%  
select(Accepted_name) %>% 
pull()

#Check what is not matching as focal
data %>% 
filter(Sampling == "Focal") %>% 
summarise(n_distinct(Plant))

#Extract focal species that are not matching
spp_to_exclude = data %>% 
filter(Sampling == "Focal") %>% 
filter(!Plant %in% spp_list) %>% 
select(Plant) %>% 
pull()

#Exclude non-matching focals now
data = data %>% 
filter(!Plant %in% spp_to_exclude)
#Filter out species with 0 pollinators
data = data %>% 
filter(!Pollinator == "None")

#Check number of interactions from focal and random
data %>% 
group_by(Botanical_garden, Sampling) %>% 
count()

#Explore interactions across time
#1st create date col
data = data %>%
mutate(Date = make_date(Year, Month, Day))
#Now summarise interactions by dates and bot garden
data_dates_1 = data %>% 
select(Interactions, Botanical_garden, Date) %>% 
group_by(Botanical_garden, Date) %>% 
summarise(Interactions = n())

#Plot interactions
ggplot(data_dates_1) + 
geom_density(aes(Date, Interactions), stat = "identity") +
facet_wrap(vars(Botanical_garden))

#Prepare dataset with sampling method
data_dates_2 = data %>% 
select(Interactions, Botanical_garden, Date, Sampling) %>% 
group_by(Botanical_garden, Date, Sampling) %>% 
summarise(Interactions = n())
#Plot interactions
ggplot(data_dates_2) + 
geom_density(aes(Date, Interactions), stat = "identity") +
facet_grid(Sampling ~ Botanical_garden)

#We are still checking species names
#Process and explore provisional dataset 
#Only consider for now fields with
#Checked and to check
reviewed_spp = c("checked", "to_check")
data_filtered = data %>% 
filter(Status %in% reviewed_spp) %>% 
select(!Status)

#Select taxonomic information
poll_spp = data_filtered %>% 
select(Pollinator) %>% 
distinct() %>% 
filter(!Pollinator == "None") %>% 
pull()


#Check for futher taxonomic info
matched_gbif = name_backbone_checklist(name = poll_spp, kingdom='animals')

#Fix str of data from GBIF
change_str1 <- function(data) { 
data = data %>% 
select(!c(usageKey, confidence, kingdomKey,
          phylumKey, classKey, orderKey, familyKey,
         genusKey,  speciesKey,
         verbatim_index, verbatim_kingdom)) %>% 
rename(Fixed_name = verbatim_name,
       Scientific_name  = scientificName,
       Canonical_name  = canonicalName,
       Accepted_name = species) %>% 
select(Fixed_name, rank, status, matchType, 
       Scientific_name, Canonical_name,
       Accepted_name, kingdom, phylum, order, family,
       genus) %>% 
  rename_all(~str_to_title(.))
}

#Change data structure
matched_gbif1 = change_str1(matched_gbif)

#Fix higher rank records
source("Scripts/Pollinator_processing.R")

#Add Genus to accepted name
matched_gbif1 = matched_gbif1 %>% 
mutate(Accepted_name = 
if_else(is.na(Accepted_name) & Matchtype== "EXACT", 
Canonical_name, Accepted_name)) %>% 
rename(Pollinator = Fixed_name)


#Bind back with dataset
data_poll = left_join(data_filtered, matched_gbif1, by = "Pollinator")
#Dataset with pollinator names ready
#Select now just Hymenoptera interactions
hymenoptera = data_poll %>% 
filter(Order == "Hymenoptera")

hymenoptera_without_apis = data_poll %>% 
filter(Order == "Hymenoptera") %>% 
filter(!Pollinator == "Apis mellifera")

#Select Apis data 
apis = data_poll %>% 
filter(Order == "Hymenoptera") %>% 
filter(Pollinator == "Apis mellifera") %>% 
group_by(Botanical_garden) %>% 
count()

#Bar plot with apis data
ggplot(apis, aes(Botanical_garden, n)) +
geom_col() + ylab("Honey bees")


#Check number of hymenoptera species 
hymenoptera %>% 
filter(Rank == "SPECIES") %>% 
select(Accepted_name) %>% 
summarise(Total_spp = n_distinct(Accepted_name))
#Check number of bee species per garden
hymenoptera_garden = hymenoptera %>% 
filter(Rank == "SPECIES") %>% 
group_by(Botanical_garden) %>% 
select(Accepted_name, Botanical_garden) %>% 
summarise(Total_spp = n_distinct(Accepted_name))
#Bar plot
ggplot(hymenoptera_garden, aes(Botanical_garden, Total_spp)) +
geom_col() + ylab("Hymenoptera")

#Check number of species per garden and sampling method
hymenop_garden_sampling = hymenoptera %>% 
filter(Rank == "SPECIES") %>% 
group_by(Botanical_garden, Sampling) %>% 
select(Accepted_name, Botanical_garden, Sampling) %>% 
summarise(Total_spp = n_distinct(Accepted_name))
#We have recorded 130 species in the 3 gardens
#Bar plot
ggplot(hymenop_garden_sampling, aes(Botanical_garden, 
Total_spp)) +
geom_col(aes(fill = Sampling), position = "dodge") + 
ylab("Hymenoptera") +
scale_fill_manual(values=c("gray12", "#E69F00"))

#Show number of times that each species appears
hym_distribution = hymenoptera %>% 
filter(Rank == "SPECIES") %>% 
select(Accepted_name, Botanical_garden, Family) %>% 
group_by(Accepted_name, Botanical_garden, Family) %>% 
summarise(Counts = length(Accepted_name)) %>% 
arrange(desc(Counts)) %>% 
filter(!Accepted_name == "Apis mellifera") #filter out Apis

#Case when by bee family
hym_distribution = hym_distribution %>% 
mutate(Family_col = case_when(
  Family == "Apidae" ~ "Apidae",
  Family == "Megachilidae" ~ "Megachilidae",
  Family == "Halictidae" ~ "Halictidae",
  Family == "Colletidae" ~ "Colletidae",
  Family == "Andrenidae" ~ "Andrenidae",
 T ~ "Other"))

#Plot distributions by garden
ggplot(hym_distribution, aes(reorder(Accepted_name, -Counts), Counts)) +
geom_col(aes(fill = Family_col)) + 
ylab("Individuals") +
xlab("Pollinator species") +
theme(axis.text.x = element_blank(),
      axis.ticks.x=element_blank()) +
facet_wrap(vars(Botanical_garden), ncol = 1) +
scale_fill_manual(values = c("blue","darkorange","green3","maroon1","yellow","black","lightsalmon3","black"))


#Try to plot species cumulative curves
cumulative_by_date <- hymenoptera %>%                              # Applying group_by & summarise
  group_by(Date) %>%
  summarise(count = dist(Accepted_name))


  
cumulative_sum = hymenoptera %>%
filter(Rank == "SPECIES") %>% 
select(Date, Botanical_garden, Accepted_name) %>%   
mutate(cum_sum = cumsum(!duplicated(Accepted_name))) %>%
group_by(Date) %>%
slice(n()) %>%
select(-Accepted_name)
#Plot interactions
ggplot(cumulative_sum) + 
geom_density(aes(Date, cum_sum), stat = "identity") +
facet_wrap(vars(Botanical_garden)) +
ylab("Cumulative species curve")
