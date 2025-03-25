#Script to check how the pollinator community changes throughout the season
#Ordination analyses-->NMDS

#Load libraries
library(dplyr) #data organization
library(vegan) #nmds
library(tidyr) #pivot wider
library(ggplot2) #plotting
library(viridis) #plotting
library(patchwork) #plotting
library(tibble) #cols to rows

#Load data
data = readRDS("Data/Working_files/interaction_data.rds")
#Select columns of interest
d = data %>% 
select(Botanical_garden, Pollinator_rank, Pollinator_accepted_name,
       Pollinator_family, Pollinator_genus, Date_time) %>% 
mutate(Date = as.Date(Date_time)) %>% 
mutate(Date = format(as.POSIXct(Date, format="%Y%m%d"),format="%d%b%y")) %>% 
select(!Date_time) %>% 
rename(Family = Pollinator_family) %>% 
rename(Genus = Pollinator_genus) %>% 
rename(Species = Pollinator_accepted_name)

#Create function to run for each garden and taxonomic level
nmds_plot = function(data, garden, taxanomic_level){
#1)Pollinator family level
d1 = d %>% 
filter(Botanical_garden == garden) %>% 
select(!!sym(taxanomic_level), Date) %>% 
filter(!is.na(!!sym(taxanomic_level))) %>% 
group_by(Date, !!sym(taxanomic_level)) %>% 
count(name = "Number")
#Convert to wide and then to matrix
matrix = d1 %>% 
pivot_wider(names_from = !!sym(taxanomic_level), 
            values_from = Number, 
            values_fill = 0) %>%
remove_rownames %>% 
column_to_rownames(var="Date") %>%
as.matrix()
#Ordination analysis
nmds_output = metaMDS(matrix, 
                     k=2) # The number of reduced dimensions
#Visualize with ggplot
nmds_output_data = nmds_output$points %>%
as_tibble(rownames = "Date") %>%
mutate(Date = as.Date(format(as.POSIXct(Date, format="%d%b%y"), format="%Y-%m-%d"))) 
#Plot it
plot_nmds = ggplot(nmds_output_data, aes(MDS1,MDS2)) +
geom_point(stroke=1, aes(color=Date),size=2) + 
scale_colour_viridis(name = "Flowering",
                     labels=c("Start", "End"),
                    # labels=function(x)as.Date(x, origin="1970-01-01"),
                     breaks = c(min(nmds_output_data$Date), max(nmds_output_data$Date))) +
theme_bw() +
ggtitle(paste(garden, "\n", taxanomic_level)) 

plot_nmds
}

#Leipzig
l1 = nmds_plot(d, "Leipzig", "Family")
l2 = nmds_plot(d, "Leipzig", "Genus")
l3 = nmds_plot(d, "Leipzig", "Species")
#Halle
h1 = nmds_plot(d, "Halle", "Family")
h2 = nmds_plot(d, "Halle", "Genus")
h3 = nmds_plot(d, "Halle", "Species")
#Jena
j1 = nmds_plot(d, "Jena", "Family")
j2 = nmds_plot(d, "Jena", "Genus")
j3 = nmds_plot(d, "Jena", "Species")

#Family level
family_panel = l1 + h1 + j1 & theme(legend.position = "none")
#Genus level
genus_panel =l2 + h2 + j2 + plot_layout(guides = "collect")
#Species level
species_panel = l3 + h3 + j3 & theme(legend.position = "none")

family_panel /
genus_panel /
species_panel 


#Prepare one graph for a power point presentation
species_panel

