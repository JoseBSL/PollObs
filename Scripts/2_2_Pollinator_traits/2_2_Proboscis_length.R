#Extrapolate tongue length for bees using pollimetry
#Use existing information and OUR images also to get the remaining proboscis

#Install packages
#devtools::install_github("liamkendall/pollimetry")
#devtools::install_github("liamkendall/pollimetrydata")
#Load library
library(pollimetry)
library(pollimetrydata)
library(dplyr)
#Load pollinator trait data
polltraits = readRDS("Data/Trait_data/Processed/PollTraits.rds")

#Read data
matched_gbif_pollinators = readRDS("Data/Working_files/matched_gbif_pollinators.rds")

#Join data
d = left_join(polltraits, matched_gbif_pollinators)
#Rename to run pollimetry function
d = d %>% 
  rename(IT = IT_mm) %>% 
  rename(Family = Pollinator_family)
#Select cols of interest and rename to extrapolate proboscis length
colnames(d)
#This is only implemented for these families
bee_fam = c("Andrenidae", "Apidae", "Colletidae", "Halictidae", "Megachilidae")
#Filter bee families that are available
d1 = d %>% 
select(c(IT, Family)) %>% 
filter(Family %in% bee_fam) %>% 
distinct()
#Extrapolate proboscis length
tongue_length = tonguelength(d1, mouthpart="all")
#Merge back extrapolated traits
d_extrapolated = left_join(d, tongue_length)

#Calculate mean proboscis length at species level
mean_proboscis_length = d_extrapolated %>% 
group_by(Pollinator) %>% 
summarise(Mean_proboscis_length = mean(Proboscis, na.rm = TRUE))

#Get mean proboscis length for non-bee species
non_bees = d %>% 
  filter(!Family %in% bee_fam) %>% 
  filter(!is.na(Tongue_mm)) %>% 
  select(c(Pollinator, Tongue_mm)) %>% 
  group_by(Pollinator) %>% 
  summarise(Mean_proboscis_length = mean(Tongue_mm, na.rm = T)) %>% 
  distinct()
#Create vector of non-bee species
non_bees_vector = non_bees %>% 
  pull(Pollinator)
#Filter out those and merge them back
mean_proboscis_length_filtered = mean_proboscis_length %>% 
  filter(!Pollinator %in% non_bees_vector) %>% 
  bind_rows(non_bees)
 
#Important! 
#For the missing species 
#we can try to provide an extrapolated value at genus or family level

#First create dataset with relevant taxonomic information
proboscis_data = left_join(mean_proboscis_length_filtered, matched_gbif_pollinators)

#Select taxonomic rank of species and genus
proboscis_data = proboscis_data %>% 
filter(Pollinator_rank == "SPECIES" | Pollinator_rank == "GENUS")


#Calliphoridae has only 1 value
#Consider that value to all Calliphoridae
calliphoridae_value = proboscis_data %>% 
  filter(Pollinator_family == "Calliphoridae") %>% 
  summarise(Mean_proboscis_length = 
              mean(Mean_proboscis_length, na.rm = TRUE)) %>% 
  pull()

anthomyiidae_value = proboscis_data %>% 
  filter(Pollinator_family == "Anthomyiidae") %>% 
  summarise(Mean_proboscis_length = 
              mean(Mean_proboscis_length, na.rm = TRUE)) %>% 
  pull()

sarcophagidae_value = proboscis_data %>% 
  filter(Pollinator_family == "Sarcophagidae") %>% 
  summarise(Mean_proboscis_length = 
              mean(Mean_proboscis_length, na.rm = TRUE)) %>% 
  pull()

eupeodes_value = proboscis_data %>% 
  filter(Pollinator_genus == "Eupeodes") %>% 
  summarise(Mean_proboscis_length = 
              mean(Mean_proboscis_length, na.rm = TRUE)) %>% 
  pull()

phasia_value = proboscis_data %>% 
  filter(Pollinator_genus == "Phasia") %>% 
  summarise(Mean_proboscis_length = 
              mean(Mean_proboscis_length, na.rm = TRUE)) %>% 
  pull()

sphaerophoria_value = proboscis_data %>% 
  filter(Pollinator_genus == "Sphaerophoria") %>% 
  summarise(Mean_proboscis_length = 
              mean(Mean_proboscis_length, na.rm = TRUE)) %>% 
  pull()

pollenia_value = proboscis_data %>% 
  filter(Pollinator_genus == "Pollenia") %>% 
  summarise(Mean_proboscis_length = 
              mean(Mean_proboscis_length, na.rm = TRUE)) %>% 
  pull()


#Eristalis tenax (male) length
#7.28 mm
#https://doi.org/10.1111/1365-2656.12828
#As I do trust more measurements from fresh specimens than from images
#I will consider Vollucella measuremnts of this article
#https://www.jstor.org/stable/36024
eristalis_value = 7.28
episyrphus_value = 2.89
syrphus_value = 2.98
myathropa_value = 5.44
volucella_value = 7.24
cheilosia_value = 2.95
meliscaeva_value = 2.73
#https://doi.org/10.1098/rspb.1985.0025
scaeva_value = 4.15
#Hemipenthes maurus (it is considered to have a short proboscis)
#A bit similar to Villa modesta for what it seems
villa_value = 2.22
#Hebia flavipes is quite similar in size and morphology to Prosena siberita
#Also a tachinidae fly
prosena_value = 2.28
gymnosoma_value = 1.55
#Minnetia is similar in size to Botanophila
#Let's assume a similar proboscis length
minettia_value = 1
#Miopa is another conopidae like Sicus
sicus_value = 2.678669
#Pseudocoenosia is a Muscidae
#Consider value of Graphomya
graphomya_value = 2.1460554
#Thecophora longirostris
#is another Conopidae provide also value of Sicus
sicus_value = 2.678669
#Tachina is anther tachinadae and very similar to Nowickia
nowickia_value = 2.9675223

#The remaining syrphidae are quite small
#Create a vector and consider episyrphus_value as their tongue length
#Add Entomophaga nigrohalterata as has also a similar size
remaing_syrphidae = c("Dasysyrphus albostriatus",
                      "Eristalinus aeneus",
                      "Paragus constrictus/tibialis",
                      "Trichopsomyia flavitarsis",
                      "Entomophaga nigrohalterata",
                      "Microphthalma europaea")

#From a local dataset of Mallorca (unpublished)
#We can obtain some average values for some wasps and beetles at genus level
#Polistes 1.7
polistes_value = 1.7
#Philanthus 2.84
philanthus_value = 2.84
#Scolia 3.63
scolia_value = 3.63
#Oedemera 0.4
oedemera_value = 0.4
#Cerceris, vespula and Ancistrocerus give same value as polistes (relatively similar)
#Other similar crabronidae as cerceris provide same value as polistes
#Tiphia, Dolicho, Sphex are large wasps provide same value as Scolia
#Metalic wasps, small proboscis ~1mm
hedychrum_value = 1
#Calculate average value of hylaeus
hylaeus_value = proboscis_data1 %>% 
  filter(Pollinator_genus == "Hylaeus") %>% 
  summarise(Mean_proboscis_length = 
              mean(Mean_proboscis_length, na.rm = TRUE)) %>% 
  pull()

#https://www.commanster.eu/Commanster/Insects/Butterflies/SpButterflies/Anthocharis.cardamines.html
#Anthocaris cardamines proboscis length 11.5
anthocharis_value = 11.5

#Average tongue length for Melittidae
#2,640909091
melittidae_value = 2.6

#Calculate average value of Lasioglossum
lasioglossum_value = proboscis_data1 %>% 
  filter(Pollinator_genus == "Lasioglossum") %>% 
  summarise(Mean_proboscis_length = 
              mean(Mean_proboscis_length, na.rm = TRUE)) %>% 
  pull()

proboscis_data1 = proboscis_data %>% 
  mutate(Mean_proboscis_length = 
           if_else(Pollinator_family == "Calliphoridae", calliphoridae_value, Mean_proboscis_length)) %>%
  mutate(Mean_proboscis_length = 
           if_else(Pollinator_genus == "Lapposyrphus", eupeodes_value, Mean_proboscis_length)) %>%
  mutate(Mean_proboscis_length = 
           if_else(Pollinator_family == "Sarcophagidae", sarcophagidae_value, Mean_proboscis_length)) %>%
  mutate(Mean_proboscis_length = 
           if_else(Pollinator_family == "Anthomyiidae" & is.na(Mean_proboscis_length), anthomyiidae_value, Mean_proboscis_length)) %>%
  mutate(Mean_proboscis_length = 
           if_else(Pollinator_genus == "Phasia", phasia_value, Mean_proboscis_length)) %>% 
  mutate(Mean_proboscis_length = 
           if_else(Pollinator_genus == "Litophasia", phasia_value, Mean_proboscis_length)) %>% 
  mutate(Mean_proboscis_length = 
           if_else(Pollinator_genus == "Sphaerophoria", sphaerophoria_value, Mean_proboscis_length)) %>% 
  mutate(Mean_proboscis_length = 
           if_else(Pollinator_genus == "Eristalis", eristalis_value, Mean_proboscis_length)) %>% 
  mutate(Mean_proboscis_length = 
           if_else(Pollinator_genus == "Episyrphus", episyrphus_value, Mean_proboscis_length)) %>% 
  mutate(Mean_proboscis_length = 
           if_else(Pollinator_genus == "Syrphus", syrphus_value, Mean_proboscis_length)) %>% 
  mutate(Mean_proboscis_length = 
           if_else(Pollinator_genus == "Myathropa", myathropa_value, Mean_proboscis_length)) %>% 
  mutate(Mean_proboscis_length = 
           if_else(Pollinator_genus == "Volucella", volucella_value, Mean_proboscis_length)) %>% 
  mutate(Mean_proboscis_length = 
           if_else(Pollinator_genus == "Cheilosia", cheilosia_value, Mean_proboscis_length)) %>% 
  mutate(Mean_proboscis_length = 
           if_else(Pollinator_genus == "Meliscaeva", meliscaeva_value, Mean_proboscis_length)) %>% 
  mutate(Mean_proboscis_length = 
           if_else(Pollinator_genus == "Scaeva", scaeva_value, Mean_proboscis_length)) %>% 
  mutate(Mean_proboscis_length = 
           if_else(Pollinator %in% remaing_syrphidae, episyrphus_value, Mean_proboscis_length)) %>% 
  mutate(Mean_proboscis_length = 
           if_else(Pollinator_genus == "Pollenia", pollenia_value, Mean_proboscis_length)) %>% 
mutate(Mean_proboscis_length = 
         if_else(Pollinator_genus == "Hemipenthes", villa_value, Mean_proboscis_length)) %>% 
  mutate(Mean_proboscis_length = 
           if_else(Pollinator_genus == "Hebia", prosena_value, Mean_proboscis_length)) %>% 
  mutate(Mean_proboscis_length = 
           if_else(Pollinator_genus == "Gymnosoma", gymnosoma_value, Mean_proboscis_length)) %>% 
  mutate(Mean_proboscis_length = 
           if_else(Pollinator_genus == "Minettia", minettia_value, Mean_proboscis_length)) %>% 
  mutate(Mean_proboscis_length = 
           if_else(Pollinator_genus == "Myopa", sicus_value, Mean_proboscis_length)) %>% 
  mutate(Mean_proboscis_length = 
           if_else(Pollinator_genus == "Pseudocoenosia", graphomya_value, Mean_proboscis_length)) %>% 
  mutate(Mean_proboscis_length = 
           if_else(Pollinator_genus == "Merziella", sicus_value, Mean_proboscis_length)) %>% 
mutate(Mean_proboscis_length = 
         if_else(Pollinator_genus == "Tachina", nowickia_value, Mean_proboscis_length)) %>% 
mutate(Mean_proboscis_length = 
           if_else(Pollinator_genus == "Polistes", polistes_value, Mean_proboscis_length)) %>% 
mutate(Mean_proboscis_length = 
           if_else(Pollinator_genus == "Philanthus", philanthus_value, Mean_proboscis_length)) %>% 
mutate(Mean_proboscis_length = 
           if_else(Pollinator_genus == "Scolia", scolia_value, Mean_proboscis_length)) %>% 
  mutate(Mean_proboscis_length = 
           if_else(Pollinator_genus == "Oedemera", oedemera_value, Mean_proboscis_length)) %>% 
mutate(Mean_proboscis_length = 
         if_else(Pollinator_genus == "Hylaeus" & is.na(Mean_proboscis_length), 
                 hylaeus_value, Mean_proboscis_length)) %>% 
  mutate(Mean_proboscis_length = 
           if_else(Pollinator_genus == "Cerceris", polistes_value, Mean_proboscis_length)) %>% 
  mutate(Mean_proboscis_length = 
           if_else(Pollinator_genus == "Ancistrocerus", polistes_value, Mean_proboscis_length)) %>% 
  mutate(Mean_proboscis_length = 
           if_else(Pollinator_genus == "Vespula", polistes_value, Mean_proboscis_length)) %>% 
  mutate(Mean_proboscis_length = 
           if_else(Pollinator_genus == "Gymnomerus", polistes_value, Mean_proboscis_length)) %>% 
  mutate(Mean_proboscis_length = 
           if_else(Pollinator_genus == "Sapygina", polistes_value, Mean_proboscis_length)) %>% 
  mutate(Mean_proboscis_length = 
           if_else(Pollinator_genus == "Tiphia", scolia_value, Mean_proboscis_length)) %>% 
  mutate(Mean_proboscis_length = 
           if_else(Pollinator_genus == "Sphex", scolia_value, Mean_proboscis_length)) %>% 
  mutate(Mean_proboscis_length = 
           if_else(Pollinator_genus == "Philanthus", scolia_value, Mean_proboscis_length)) %>% 
  mutate(Mean_proboscis_length = 
           if_else(Pollinator_genus == "Dolichovespula", scolia_value, Mean_proboscis_length)) %>% 
  mutate(Mean_proboscis_length = 
           if_else(Pollinator_genus == "Crossocerus", scolia_value, Mean_proboscis_length)) %>% 
  mutate(Mean_proboscis_length = 
           if_else(Pollinator_genus == "Dinetus", scolia_value, Mean_proboscis_length)) %>% 
  mutate(Mean_proboscis_length = 
           if_else(Pollinator_genus == "Ectemnius", scolia_value, Mean_proboscis_length)) %>% 
  mutate(Mean_proboscis_length = 
           if_else(Pollinator_genus == "Nysson", scolia_value, Mean_proboscis_length)) %>% 
  mutate(Mean_proboscis_length = 
           if_else(Pollinator_genus == "Hedychrum", hedychrum_value, Mean_proboscis_length)) %>% 
  mutate(Mean_proboscis_length = 
         if_else(Pollinator_genus == "Holopyga", hedychrum_value, Mean_proboscis_length)) %>% 
  mutate(Mean_proboscis_length = 
           if_else(Pollinator_genus == "Pseudovadonia", oedemera_value, Mean_proboscis_length)) %>% 
  mutate(Mean_proboscis_length = 
           if_else(Pollinator_genus == "Anthocharis", anthocharis_value, Mean_proboscis_length)) %>% 
mutate(Mean_proboscis_length = 
         if_else(Pollinator == "Macropis fulvipes", melittidae_value, Mean_proboscis_length)) %>% 
mutate(Mean_proboscis_length = 
           if_else(Pollinator == "Melitta haemorrhoidalis", melittidae_value, Mean_proboscis_length)) %>% 
mutate(Mean_proboscis_length = 
           if_else(Pollinator_genus == "Anthrenus", oedemera_value, Mean_proboscis_length)) %>% 
  mutate(Mean_proboscis_length = 
           if_else(Pollinator_order== "Coleoptera" & is.na(Mean_proboscis_length), 
                   oedemera_value, Mean_proboscis_length)) %>% 
  mutate(Mean_proboscis_length = 
           if_else(Pollinator_order == "Hemiptera", oedemera_value, Mean_proboscis_length)) %>% 
  mutate(Mean_proboscis_length = 
           if_else(Pollinator_genus== "Lasioglossum" & is.na(Mean_proboscis_length), 
                   lasioglossum_value, Mean_proboscis_length))



#Now select pollinator and proboscis length and merge it back with trait data
main_proboscis_col = proboscis_data1 %>% 
select(Pollinator, Mean_proboscis_length)

#Bind with trait data
polltraits1 = left_join(polltraits, main_proboscis_col)

polltraits_clean = polltraits1 %>% 
rename(IT = IT_mm) %>% 
rename(Body_length = Length_mm) %>% 
rename(Proboscis_length = Mean_proboscis_length) %>% 
select(!Tongue_mm)

saveRDS(polltraits_clean, "Data/Trait_data/Processed/PollTraits_with_proboscis.rds")






























