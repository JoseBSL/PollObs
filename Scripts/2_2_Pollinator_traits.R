#Script to prepare trait data of pollinators
#Load libraries
library(readr) #to read files
library(dplyr) #process data
library(ggplot2) #visualize data
library(stringr) #process strings
#Steps:
#1)Read Pollinator traits
#2)Read species names and prepare it with old id's
#3)Convert pixels to mm
#4)Visualize trait distributions
#5)Match with pollinator id's

#1)Read Pollinator traits
#Load pollinator trait data
poll_traits = read_csv("Data/Trait_data/Raw/PollTraits.csv")
#2)Read species names and prepare it with old id's
#Load unprocessed interaction data to recover old id's
data = read_csv("Data/PollObs_all.csv")
poll_spp_id = data %>% 
  select(Pollinator, Pollinator_id) %>% 
  filter(!is.na(Pollinator_id)) %>% 
  distinct() %>% 
  filter(!str_detect(Pollinator_id,"Like") &
         !str_detect(Pollinator_id,"like") & 
         !str_detect(Pollinator_id, "red bee") &
         !str_detect(Pollinator_id , "LP1"),
         !str_detect(Pollinator_id , "HP2")) 
#Load data with processed names
int_data = readRDS("Data/Working_files/interaction_data.rds")
poll_accepted_names = int_data %>% 
  select(Pollinator, Pollinator_accepted_name) %>% 
  distinct()
#Match old names with Id's with the aceepted ones
pollinator_species = left_join(poll_spp_id, poll_accepted_names)

#3)Convert pixels to mm
#Covert in trait data pixels to mm
poll_traits1 = poll_traits %>% 
  mutate(IT_mm = IT_pixels * 10 / Square_size_pixels) %>% 
  mutate(Length_mm = Length_pixels * 10 / Square_size_pixels) %>% 
  mutate(Tongue_mm = Tongue_pixels * 10 / Square_size_pixels) %>% 
  rename(Pollinator_id = ID)

#4)Visualize trait distributions
#Explore distribution
#IT
poll_traits1 %>% 
ggplot(aes(IT_mm)) +
geom_histogram(colour="black", fill="plum3") +
theme_bw()+
coord_cartesian(expand = FALSE) + 
xlab("IT distance (mm)") +
ylab("Counts")
#Body length
poll_traits1 %>% 
  ggplot(aes(Length_mm)) +
  geom_histogram(colour="black", fill="steelblue3") +
  theme_bw()+
  coord_cartesian(expand = FALSE) + 
  xlab("Body length (mm)") +
  ylab("Counts")
#They look relatively similar as expected
#Check their correlation
poll_traits1 %>% 
  ggplot(aes(IT_mm, Length_mm)) +
geom_point()
#Looks ok, highly correlated
#Keep an eye on species out of the trend

#5)Match with pollinator id's
#Now merge trait data with species names
poll_trait_data = left_join(pollinator_species, poll_traits1)
#Now select columns of interest
poll_trait_data1 = poll_trait_data %>% 
select(!c(Pollinator_id, Box, Number, Square_size_pixels, 
          IT_pixels, Length_pixels, Tongue_pixels))

saveRDS(poll_trait_data1, "Data/Trait_data/Processed/PollTraits.rds")

