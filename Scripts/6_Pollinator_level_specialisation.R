#Calculate overall species level specialisation
#Obtain also number of interacting partners per pollinator and see how they correlate

#Load libraries
library(dplyr)
library(bipartite)
#Plot it a bit nicer
library(ggplot2)
library(ggExtra)
library(RColorBrewer)
library(cowplot)
library(ggpubr)
#Load data
raw_data = readRDS("Data/Working_files/interaction_data.rds")

#Organise data
interaction_data = raw_data  %>% filter(!is.na(Interactions),
                                         !is.na(Floral_abundance),
                                         Pollinator != "None") %>% 
select(Botanical_garden, Plant_accepted_name, 
       Pollinator_accepted_name, Date_time, 
       Interactions, Floral_abundance) %>% 
rename(Plant = Plant_accepted_name, Pollinator = Pollinator_accepted_name) %>% 
mutate(Date = as.Date(Date_time)) %>% 
mutate(Week = lubridate::week(Date)) %>% 
select(-Date_time, -Date) %>% filter(!is.na(Pollinator)) %>% ungroup()

#Bind everything together and aggregate interactions by species
int = interaction_data %>% 
select(Plant, Pollinator, Interactions) %>% 
group_by(Plant, Pollinator) %>% 
summarise(Interactions1 = sum(Interactions)) %>% 
pivot_wider(names_from = Pollinator, 
                      values_from = Interactions1,
                      values_fill = 0) %>% 
tibble::column_to_rownames("Plant")

#Calculate species level specialisation
poll_specialisation = specieslevel(int, index="d", level="higher")
#Alternative way to account for abundances
#https://rdrr.io/cran/bipartite/man/dfun.html
#poll_specialisation = dfun(t(int), abuns=NULL)

#Get the overall approximated abundances 
poll_abundance = interaction_data %>%
group_by(Pollinator) %>%
count() %>% 
rename(Total_pollinator_abundance = n) %>% ungroup()

#Provide format before left join
poll_specialisation1 = poll_specialisation %>% tibble::rownames_to_column("Pollinator")

all =left_join(poll_specialisation1, poll_abundance)

#
p = all %>% 
ggplot(aes(Total_pollinator_abundance, d, fill= "black")) +
geom_point() +
scale_x_log10() +
theme_bw() +
theme(legend.position = "none") +
xlab("log(Pollinator abundance)") +
ylab("Specialisation (d')")
p

p_marginal = ggMarginal(p, type = "density", groupFill = TRUE)

#Find now the number of interacting partners 
poll_interacting_partners = interaction_data %>% 
select(Plant, Pollinator) %>% 
group_by(Pollinator) %>% 
summarise(Visiting_species = n_distinct(Plant))

all1 =left_join(poll_abundance, poll_interacting_partners)

p1 = all1 %>% 
ggplot(aes(Total_pollinator_abundance, Visiting_species, fill= "black")) +
geom_point() +
scale_y_log10() +
scale_x_log10() +
theme_bw() +
theme(legend.position = "none") +
xlab("log(Pollinator abundance)") +
ylab("log(Pollinator degree)")
p1
p1_marginal = ggMarginal(p1, type = "density", groupFill = TRUE)

library(patchwork)
p + p1

library(ggExtra)
library(gridExtra)
grid.arrange(p_marginal, p1_marginal, ncol = 2)


