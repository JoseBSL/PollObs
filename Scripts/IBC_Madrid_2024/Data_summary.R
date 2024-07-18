
#Basic descriptors
#Read interaction daa
library(dplyr)
library(tidyr)
library(lubridate)
library(readr)
library(ggplot2)
library(viridis)
library(scales)
#Load data
raw_data = readRDS("Data/Working_files/interaction_data.rds")

poll_species = raw_data %>% 
filter(Pollinator_rank == "SPECIES") %>% 
filter(Pollinator_order == "Hymenoptera" | Pollinator_order == "Diptera"| 
      Pollinator_order == "Coleoptera" |  Pollinator_order == "Lepidoptera") %>% 
select(Pollinator_accepted_name, Pollinator_order) %>% 
group_by(Pollinator_order) %>% 
summarise(Species = n_distinct(Pollinator_accepted_name)/n_distinct(.),
          Interactions = length(Pollinator_accepted_name)/ nrow(.))

# Combine Interactions and Species into a single column
data_long = pivot_longer(poll_species, cols = c(Interactions, Species), names_to = "Variable", values_to = "Value")


#Set order of levels for plotting
data_long$Pollinator_order = factor(data_long$Pollinator_order, levels = c("Coleoptera", "Lepidoptera", "Diptera", "Hymenoptera"))

ggplot(data_long, aes(x = Variable, y = Value, fill = Pollinator_order)) + 
geom_bar(stat = "identity", width = 0.7) +
theme_classic() +
scale_fill_viridis(discrete=TRUE, direction = -1, name="Pollinator order") +
xlab(NULL) +
ylab("Percentage") + 
scale_y_continuous(labels = percent_format(), expand = c(0, 0)) +
theme(axis.text = element_text(size=6), 
     axis.title.y = element_text(face = "bold", size = 7, vjust = -2),  
     axis.title.x = element_text(face="bold", size = 7),
     legend.position = "top",
     legend.title = element_text(size = 5, face = "bold"),
     legend.text = element_text(size = 5),
     legend.key.size = unit(3, "mm"),
     legend.margin=margin(0,0,0,0),
     legend.box.margin=margin(-8,-5,-5,-5)) +
guides(fill=guide_legend(nrow=2,byrow=TRUE))+
ggtitle("(c)") +
theme(plot.title = element_text(size = 10,hjust = -0.3, vjust = -6)) +
coord_flip()


poll_species = raw_data %>% 
filter(Pollinator_rank == "SPECIES") %>% 
filter(Pollinator_order == "Hymenoptera" | Pollinator_order == "Diptera"| 
      Pollinator_order == "Coleoptera" |  Pollinator_order == "Lepidoptera") %>% 
select(Pollinator_accepted_name, Pollinator_order) %>% 
summarise(n_distinct(Pollinator_accepted_name))

plant_species = raw_data %>% 
filter(Plant_rank == "SPECIES") %>% 
select(Plant_accepted_name, Plant_order) %>% 
summarise(n_distinct(Plant_accepted_name))


unique_interactions = raw_data %>% 
filter(Plant_rank == "SPECIES" & Pollinator_rank == "SPECIES") %>% 
filter(Pollinator_order == "Hymenoptera" | Pollinator_order == "Diptera"| 
      Pollinator_order == "Coleoptera" |  Pollinator_order == "Lepidoptera") %>% 
mutate(Unique_int = paste0(Plant_accepted_name, Pollinator_accepted_name)) %>% 
distinct(Unique_int) %>% 
nrow(.)
