#Script to prepare all plant traits from PhenObs
#1)Prepare traits used in Lanuza 2023
#Those are: 
#Selfing
#Plant height
#Flower size
#Flower number
#Style length
#Ovule number

#2)Select other traits of interest and format accordingly
#Flowering length
#Flower lifespan
#Nectar

#Load libraries
library(dplyr)
library(readr)
library(conflicted)
library(lubridate)
library(rgbif)
library(stringr)
library(phytools)
library(rtrees)
library(ggplot2)

conflict_prefer("select", # the function
                "dplyr")
#Load morphometrics and select/rename column of interest
morphometrics = read_csv("Data/Trait_data/Raw/ReproductiveTraits_Morphometrics.csv")
colnames(morphometrics)

#Load network descriptors of PhenObs plants
phenobs_metrics = readRDS("Data/Working_files/phenobs_network_metrics.rds") %>% 
rename(Species_all = Plant)


#1)Prepare traits used in Lanuza 2023
lanuza_2023_traits = morphometrics %>% 
 select(
   Species,
   Flower_number, 
   Flower_width,
   Style_length, 
   `Ovule_number/flower`,
   Plant_height_mm) %>% 
 rename(Flowers_per_plant = Flower_number) %>% 
 rename(Corolla_diameter_mean = Flower_width) %>% 
 rename(Ovule_number = `Ovule_number/flower`) %>% 
 rename(Plant_height_mean_m = Plant_height_mm) %>% 
 mutate(Plant_height_mean_m = Plant_height_mean_m/1000) %>% 
 mutate(Style_length = as.numeric(Style_length)) %>% 
 group_by(Species) %>%
 summarise(
   Flowers_per_plant = mean(Flowers_per_plant, na.rm = TRUE),
   Corolla_diameter_mean = mean(Corolla_diameter_mean, na.rm = TRUE),
   Style_length = mean(Style_length, na.rm = TRUE),
   Ovule_number = mean(Ovule_number, na.rm = TRUE),
   Plant_height_mean_m = mean(Plant_height_mean_m, na.rm = TRUE)
  )

#Add now selfing that is missing here
selfing = read_csv("Data/Trait_data/Processed/Selfing.csv")
colnames(selfing)
selfing = selfing %>% 
  select(c(Species, Average_fruits)) %>% 
  rename(Autonomous_selfing_level_fruit_set = Average_fruits)
#Add selfing to morphometircs
lanuza_2023_traits = left_join(selfing, lanuza_2023_traits)

#Lanuza traits are ready, jump onto the next ones

#For the phylo we need taxonomic info
#Retrieve it quickly from GBIF
plant_spp = lanuza_2023_traits %>% pull(Species)
#Check for futher taxonomic info
matched_gbif_plants = name_backbone_checklist(name = plant_spp)
#Select columns of interest and rename accordingly
matched_gbif_plants1 = matched_gbif_plants %>% 
  dplyr::select(c(family, verbatim_name)) %>% 
  rename(Family_all = family) %>% 
  rename(Species_all = verbatim_name) %>% 
  mutate(Genus_all = word(Species_all, 1))
#Bind cols
lanuza_2023_traits1 = bind_cols(lanuza_2023_traits, matched_gbif_plants1)

#2)Select other traits of interest and format accordingly
colnames(morphometrics)

#Select cols of interest and convert to date format
date_columns = c("Flowering_start", 
                  "Flowering_end", 
                  "Flower_life_start", 
                  "Flower_life_end")

flowering_dates = morphometrics %>% 
  select(Species, 
       Flowering_start, 
       Flowering_end,
       Flower_life_start,
       Flower_life_end) %>% 
  mutate(across(all_of(date_columns), ~ dmy(.)))

#Check that it works
str(flowering_dates)
#Now substract dates to get number of flowering days and flower life span
flowering_dates1 = flowering_dates %>% 
  mutate(Flowering_length = as.numeric(Flowering_end - Flowering_start)) %>% 
  mutate(Flower_lifespan = as.numeric(Flower_life_end - Flower_life_start)) %>% 
  select(Species, 
       Flowering_length,
       Flower_lifespan) %>% 
  group_by(Species) %>% 
  summarise(Mean_flowering_length = mean(Flowering_length, na.rm = TRUE),
          Mean_flower_lifespan = round(mean(Flower_lifespan, na.rm = TRUE)))


#Bind both datasets so I can compute a PCA with all traits for now
all_traits = left_join(lanuza_2023_traits1, flowering_dates1)

#Load nectar data 
nectar = read_csv("Data/Trait_data/Raw/Nectar_volume.csv")
#Select columns of interest
#Note the microcaps were 1ul and 32 mm length
nectar1 = nectar %>% 
select(Species, Nectar_length_microcap) %>% 
mutate(Nectar_volume = Nectar_length_microcap * 1 / 32) %>% 
group_by(Species) %>% 
summarise(Mean_nectar_volume = mean(Nectar_volume))
#Add nectar to trait dataset
all_traits1 = left_join(all_traits, nectar1)

#Load seed weight data 
seed_weight = read_csv("Data/Trait_data/Raw/Seed_weight.csv")

seed_weight1 = seed_weight %>% 
rename(Species = Accepted_name) %>% 
group_by(Species) %>% 
mutate(Seed_mass = Total_mass/Sample_size) %>% 
select(Species, Seed_mass)

all_traits2 = left_join(all_traits1, seed_weight1)

#Load pollen size data 
pollen_size = read_csv("Data/Trait_data/Raw/Pollen_size.csv")

pollen_size1 = pollen_size %>% 
group_by(Species) %>% 
select(Species, Pollen_size)

all_traits3 = left_join(all_traits2, pollen_size1)

#Load pollen count data 
pollen_counts = read_csv("Data/Trait_data/Raw/Pollen_counts.csv")

#Obtain total pollen per anther
pollen_counts1 = pollen_counts %>% 
group_by(Species) %>% 
mutate(Pollen_per_anther = Pollen_per_sample/Anthers_per_sample) %>% 
select(Species, Pollen_per_anther) %>% 
summarise(Mean_pollen_per_anther = mean(Pollen_per_anther))
#Calculate total pollen per flower
colnames(morphometrics)
stamens = morphometrics %>% 
group_by(Species) %>% 
select(Species, Stamen_number) %>% 
summarise(Mean_stamen_number = mean(Stamen_number, na.rm = TRUE)) 
#Bind with pollen counts
pollen_counts2 = left_join(pollen_counts1, stamens)
#Calculate total pollen per flower
pollen_counts3 = pollen_counts2 %>% 
group_by(Species) %>% 
mutate(Pollen_per_flower = Mean_pollen_per_anther * Mean_stamen_number) %>% 
select(Species, Pollen_per_flower)
#Bind with other trait data to conduct PCA
all_traits4 = left_join(all_traits3, pollen_counts3)

colnames(all_traits4)
all_traits4 = all_traits4 %>% 
mutate(Species_all = str_replace(Species_all, "Persicaria bistorta", "Polygonum bistorta")) %>% 
mutate(Species_all = str_replace(Species_all, "Aquilegia chrysantha", "Aquilegia vulgaris"))


#Check colnames
colnames(all_traits4)
all_traits4 = all_traits4 %>%
  left_join(phenobs_metrics, by = "Species_all") 
colnames(all_traits4)

# Define the columns to transform
cols_to_transform = c("Autonomous_selfing_level_fruit_set", 
                      "Flowers_per_plant",
                      "Corolla_diameter_mean", 
                      "Style_length", 
                      "Ovule_number", 
                      "Plant_height_mean_m", 
                      "Mean_flowering_length",
                      "Mean_flower_lifespan",
                      "Mean_nectar_volume",
                      "Seed_mass",
                      "Pollen_size",
                      "Pollen_per_flower",
                      "Mean_interaction_frequency")

#Save trait data 
saveRDS(all_traits4, "Data/Working_files/plant_trait_data.rds")


#Conduct log transformation and scaling using mutate across selected columns
final_d = all_traits4 %>%
  mutate(across(all_of(cols_to_transform), ~ log(. + 1))) %>%
  mutate(across(all_of(cols_to_transform), ~ scale(.) %>% as.vector())) %>% # Convert to vector to avoid grouping issues
  dplyr::select(all_of(cols_to_transform))

#4) Get phylo
#calculate phylo 
phylo <- as.data.frame(cbind(all_traits4$Family_all, all_traits4$Genus_all, all_traits4$Species_all))
colnames(phylo) <-  c("family", "genus", "species")
#Select unique cases
#phylo_2 <- phylo[!duplicated(phylo$species),]
phylo_2 <- tibble(phylo)
#get phylo
phylo_output <- get_tree(sp_list = phylo_2, taxon = "plant")
str(phylo_output)
#Convert phylogenetic tree into matrix
A_5 <- vcv.phylo(phylo_output)
#Standardize to max value 1
A_5 <- A_5/max(A_5)
#Unify column names; remove underscore and remove asterik
rownames(A_5) <- gsub("\\*", "", rownames(A_5))
colnames(A_5) <- gsub("\\*", "", colnames(A_5))
colnames(A_5) <- gsub("_", " ", colnames(A_5))
rownames(A_5) <- gsub("_", " ", rownames(A_5))
#4) Calculate PPCA
#Standardize species names
rownames(final_d) = all_traits4$Species_all
rownames(final_d) = gsub(" ", "_", rownames(final_d))
#Run PCA
phyl_pca_forest <- phyl.pca(phylo_output, final_d,method="lambda",mode="cov")
#Save output
#saveRDS(phyl_pca_forest, "Data/Working_files/local_pca_phenobs_species_output.rds")
#saveRDS(final_d, "Data/Working_files/local_pca_phenobs_species_data.rds")


#CALL the output PC for simplicity
PC <- phyl_pca_forest
#CHECK CONTENT
#EIGENVALUES
PC$Eval
#PC score (POINTS)
PC$S
#PC loadings (ARROWS)
PC$L
s = tibble(sp = rownames(PC$S),
           PC1 = PC$S[,1],
           PC2 = PC$S[,2])

percentage = round(diag(PC$Eval) / sum(PC$Eval) * 100, 2) #calculate percentage

########################################################################################################################################################
#4) PLOT PPCA
########################################################################################################################################################
# Theme for publication
theme_ms <- function(base_size=12, base_family="Helvetica") {
  (theme_bw(base_size = base_size, base_family = base_family)+
     theme(text=element_text(color="black"),
           axis.title=element_text( size = rel(1.6)),
           axis.text=element_text(size = rel(1.6), color = "black"),
           legend.title=element_text(face="bold"),
           legend.text=element_text(size = rel(1.1)),
           legend.background=element_rect(fill="transparent"),
           legend.key.size = unit(0.4, 'lines'),
           panel.border=element_rect(color="black",size=1),
           panel.grid.minor.x =element_blank(),
           panel.grid.minor.y= element_blank(),
           panel.grid.major= element_blank(),
           plot.title = element_text(size = rel(2.5), face="bold")
     ))
}
#####
#MASTER FUNCTION TO PLOT 
#####
#seems massive but in reality is a scatterplot with arrows
#it has inside how to compute kernel density but do not think I'll use it
#point density seems to do the job


# Load plots
local_phenobs_pca <- function(PC, x = "PC1", y = "PC2") {
  # PC being a prcomp object
  data <- data.frame(PC$S)
  plot <- ggplot(data, aes_string(x = x, y = y)) 
  
  dat <- data.frame(x = data[, x], y = data[, y])
  
  plot <- plot + geom_point(data = dat, aes(-x, -y), 
                            size = 2.25, alpha = 0.85,
                            colour = "#cc67ff") 
  
  ########
  # ADD ARROWS 
  ########
  datapc <- data.frame(PC$L) 
  mult <- min(
    (max(data[, y]) - min(data[, y])) / (max(datapc[, y]) - min(datapc[, y])),
    (max(data[, x]) - min(data[, x])) / (max(datapc[, x]) - min(datapc[, x]))
  )
  
  datapc <- transform(datapc,
                      v1 = 0.65 * mult * get(x),
                      v2 = 0.65 * mult * get(y)
  )
  
  plot <- plot + geom_segment(data = datapc, linejoin = "round", lineend = "round", 
                              aes(x = 0, y = 0, xend = -v1, yend = -v2), size = 1.2, 
                              arrow = arrow(length = unit(0.5, "cm")), alpha = 1, color = "black")
  
  # Percentage for the axes
  percentage <- round(diag(PC$Eval) / sum(PC$Eval) * 100, 2) 
  
  plot <- plot + geom_segment(data = datapc, aes(x = 0, y = 0, xend = -v1, yend = -v2), 
                              size = 0.8, arrow = arrow(length = unit(0, "cm")), 
                              linetype = 2, alpha = 0.8, color = "black")
  
  rownames(PC$L) <- c("S", "FN", "FS", "SL", "ON", "PH", "FL", "FLS", "N", "SM", "PS", "PN", "MIF")
  PCAloadings <- data.frame(Variables = rownames(PC$L), PC$L)
  
  # Position labels at the end of segments
  label_positions <- -datapc[, c("v1", "v2")]
  
  plot <- plot + annotate("text", 
                          x = label_positions[[1]]*c(1.25,1.2,1.25,1.2,1,1.2,1.2,1.45,1.2,1,1,1.2,1), 
                          y = label_positions[[2]]*c(0,1.3,0.7,-0.8,1,1.2,1.8,1,2,1.15,1.5,1,1), 
                          label = PCAloadings$Variables, color = "black", 
                          size = 6, fontface = 2, vjust = 1.5)  # Adjust vjust if needed
  
  # CHANGE THEME 
  plot <- plot + theme_ms() + 
    ylim(-4, 4) + 
    xlim(-4, 4) + 
    scale_y_reverse() +  # This reverses the y-axis
    theme(legend.position = c(0.220, 0.130)) + 
    xlab("PC1") + 
    ylab("PC2")
  
  plot + theme(legend.position = "top", legend.justification = c(0, 1))
}
# Call the function
local_phenobs_pca(PC)



#Check pair correlations among traits
# You need both ggplot2 and GGally packages loaded to use ggpairs()
#library(GGally)
#ggpairs(final_d)



phyl_pca_forest1 = readRDS("Data/Working_files/local_pca_phenobs_species_output.rds")

s = tibble(Species_all = rownames(phyl_pca_forest1$S),
           PC1 = phyl_pca_forest1$S[,1],
           PC2 = phyl_pca_forest1$S[,2])

s = s %>% 
mutate(Species_all = str_replace(Species_all, "_", " "))

s1 = left_join(all_traits4, s)

cor.test(s1$Mean_interaction_frequency, s$PC1)
cor.test(s1$Mean_interaction_frequency, s$PC2)
cor.test(s1$Mean_degree, s$PC1)
cor.test(s1$Mean_degree, s$PC2)

s1 = s1 %>% 
mutate(Mean_interaction_frequency = log(Mean_interaction_frequency +1))

library(ggplot2)
library(ggpubr)  # For correlation coefficients

plot_correlation <- function(x, y, xlab, ylab) {
  ggplot(data = s1, aes(x = !!sym(x), y = !!sym(y))) +
    geom_point(alpha = 0.7, color = "cyan4") +  # Scatter points
    geom_smooth(method = "lm", color = "black", se = TRUE) +  # Regression line
    stat_cor(method = "pearson", 
             label.x = min(s1[[x]], na.rm = TRUE),
             label.y = max(s1[[y]], na.rm = TRUE), 
             size = 5) +  # Correlation coefficient
    labs(x = xlab, y = ylab) +  # Add axis labels
    theme_bw() +
    ylim(-3.5, 5)
}

# Create individual plots
p1 <- plot_correlation("Mean_interaction_frequency", "PC1", "Mean Interaction Frequency (log)", "PC1")
p2 <- plot_correlation("Mean_interaction_frequency", "PC2", "Mean Interaction Frequency (log)", "PC2")
p3 <- plot_correlation("Mean_degree", "PC1", "Mean Degree", "PC1")
p4 <- plot_correlation("Mean_degree", "PC2", "Mean Degree", "PC2")

# Arrange plots in a grid
ggarrange(p1, p2, p3, p4, ncol = 2, nrow = 2)

