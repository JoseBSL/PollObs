#Bind pollinator phenology and explore it visually
#Load libraries
library(dplyr)
library(viridis)
library(tidyr)

#Load clean phenologies
#File 1
file1 = readRDS("Data/Working_files/poll_phenology_first_133_spp.rds")
#File 2
file2 = readRDS("Data/Working_files/poll_phenology_remaining_spp.rds")
#Bind together
all = bind_rows(file1, file2)
#Check number of species
unique(file1$Species)
#Select columns of interest
all = all %>% 
select(!c(k_value, m_value, prob_value))
#Save pollinator phenology
saveRDS(all, "Data/Working_files/pollinator_phenology.rds")

#Plot phenologies
#Create order of species for plotting
plot_order = all %>% 
filter(Flying_period == "Yes") %>% 
group_by(Species) %>% 
slice_min(Doy) %>% 
arrange(-Doy) %>% 
select(Doy, Species) %>% 
rename(Order = Doy) %>% 
ungroup() %>% 
mutate(Order_number = row_number()  * 50) #To add some species across bars

#Arrange dataframe by this order
all_ordered = left_join(all, plot_order)

all_ordered = all_ordered %>% 
group_by(Species) %>% 
arrange(Order_number)
#Add NA's so when there is probability 0 nothing is plotted
all_ordered = all_ordered %>% 
mutate(Order_number = if_else(Flying_period== "No", NA, Order_number))

highlight_species <- "Sphecodes albilabris"
highlighted_species_data = all_ordered %>% filter(Species == highlight_species) %>% 
filter(Flying_period == "Yes") %>% 
slice_min(Doy) %>% 
mutate(Doy = 80)



#plot everything
p1 = all_ordered %>%
  ggplot(aes(x=Doy, y=Order_number, group = Species, color= Probability)) +
  geom_line(linetype=1, size=0.8) +
  scale_color_viridis_c(name = "Flying \n probability") +
theme_bw() +
coord_cartesian(clip = "off") +
scale_y_continuous(expand = c(0.01,0)) +
xlab("Day of the year") +
ylab("Pollinator species") +
theme(axis.ticks.y = element_blank(), axis.text.y = element_blank()) +
  geom_point(data = highlighted_species_data, 
             aes(x=Doy, y=Order_number), color="black", 
             shape =8) +
geom_text(data = highlighted_species_data, 
          aes(label = Species), size = 2, 
          hjust = 1.2, fontface = "italic")

#Plot a unique example (to help people understand this figure)
oc2 = readRDS("Data/Working_files/poll_occurrences_over_60_records.rds")

#Prepare all tibbles for plotting
#Curves
example_sp = all_ordered %>% 
filter(Species == highlight_species)
#Raw points
sp_data = oc2 %>% 
filter(Species == highlight_species)
sp_data = left_join(sp_data, example_sp)
#Vertical lines
sp_data1 = oc2 %>% 
filter(Species == highlight_species) %>% 
filter(!is.na(PollObs))
#Plot
p2 = ggplot(example_sp, aes(x = Doy, y = Probability)) +
geom_line(color = "black") +
labs(title = highlight_species,
       x = "Day of Year",
       y = "Flying probability") +
theme_minimal() +
geom_point(data = sp_data, aes(x = Doy, y = n_individuals/(max(n_individuals)), color = Probability), alpha= 0.5) +
geom_vline(data = sp_data1, aes(xintercept = Doy), linetype = "dashed", color = "black") +
scale_color_viridis_c(name = "Flying \n probability") +
guides(colour="none") 

#Now plot horizontal bar for the species
p3 = all_ordered %>%
filter(Species == highlight_species) %>% 
  ggplot(aes(x=Doy, y=Order_number, group = Species, color= Probability)) +
  geom_line(linetype=1, size=3) +
  scale_color_viridis_c(name = "Flying \n probability") +
coord_cartesian(clip = "off") +
scale_y_continuous(expand = c(0.01,0)) +
xlab("Day of the year") +
ylab("Pollinator species") +
theme(axis.ticks.y = element_blank(), axis.text.y = element_blank(), 
      legend.position = "none", plot.margin=unit(c(-20,0,0,0), "cm")) +
guides(colour="none") +
theme_void() 



library(patchwork)

panel_2 = p2 / p3

panel_2 = plot_spacer() /  p2 / p3 + plot_spacer()+  plot_layout(widths = c(2.1, -0.485, 0.8, 2.1), 
                                                               heights = c(0.5, 1.2, -0.4, 0.5)) 
p1 + panel_2
