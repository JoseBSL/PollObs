#Script to generate pollinator phenology
#Consider the three gardens together to reduce the possibilities of under-sampling
#It is important to note that:
#We finish sampling before the full pollinator phenology ended
#We finish because the plants of the projects were all gone but pollinators were still
#active on other species
#Septermber-october still have poll activity in Germany

#Even using the three gardens does not seem enough for an accurate phenology
#We are going to use GBIF occurrences from that year to complement our records
#Yet not decided if do it at Country level, given the likely lack of records for some species
#Or focus only on records close to Jena-Halle and Leipzig

#Load libraries
library(dplyr) #To manipulate data
#Load interaction data
int_data = readRDS("Data/Working_files/interaction_data.rds")
#Select columns of interest
#and filter by species level
int_data_polls = int_data %>% 
filter(Pollinator_rank=="SPECIES") %>% 
select(Pollinator,
       Date)
#Filter out "none" in pollinator column
int_data_polls = int_data_polls %>% 
filter(!Pollinator == "None")
#Convert Date to day of the year (Doy)
int_data_polls = int_data_polls %>% 
mutate(Doy = lubridate::yday(Date)) %>% 
select(!Date)

#Things to do:
#Find minimum date
#Find maximum date
#Create a column of flying period
#Count number of individuals per date
#Find minimum date for all poll. species
min_date = int_data_polls %>% 
group_by(Pollinator) %>% 
slice_min(Doy) %>% 
distinct() %>% 
rename(min_Doy = Doy)
#Find maximum date
max_date = int_data_polls %>% 
group_by(Pollinator) %>% 
slice_max(Doy) %>% 
distinct() %>% 
rename(max_Doy = Doy)

#Bind together
poll_phenology = left_join(min_date,max_date, by = "Pollinator")
#Create a column that is the difference between both dates
poll_phenology = poll_phenology %>% 
mutate(Flying_period_days = max_Doy-min_Doy)

#Now create a tibble with counts of individuals per date
#and bind the poll_phenology one
poll_phenology_counts = int_data %>% 
filter(Pollinator_rank=="SPECIES") %>% 
select(Pollinator,
       Date) %>% 
group_by(Pollinator, Date) %>% 
summarise(Individuals = n()) %>% 
mutate(Doy = lubridate::yday(Date))

#Bind to this dataset the one with info of min, max dates and flying period
poll_phen = left_join(poll_phenology, poll_phenology_counts)

#Explore how the different phenologies look like (main ones)
#Plot
spp = poll_phen %>% distinct(Pollinator) %>%  pull()
poll_phen %>% 
filter(Pollinator==spp[11]) %>% 
ggplot(aes(x = Doy, y = Individuals)) + 
geom_line()+
theme_minimal()+
theme(legend.position = "none", 
      strip.background = element_blank(),
      strip.text.x = element_blank(),
      panel.spacing = unit(-1.5,'lines')) +
coord_cartesian(expand = FALSE) +
scale_y_continuous(expand = c(0,0), breaks = c(NULL), labels = c(NULL))
#It doesn't seem we can do the same thing as we did for plants
#The number are quite low in general



