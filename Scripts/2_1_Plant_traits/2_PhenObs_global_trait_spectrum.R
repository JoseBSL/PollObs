#Load Lanuza et al., 2023
#Replicate graph and add phenObs(PollObs) species
#This provide an idea of the location of the specie sin the spectrum
library(dplyr)
library(rgbif)
library(stringr)
library(phytools) 
library(ape) 
library(rtrees) 
library(reshape2) 
#Integrate PollObs with the existing data
#Load selfing and trait morphometrics dataset
#I need to standardize to these column names:
#[1] "Autonomous_selfing_level_fruit_set"
#[2] "Flowers_per_plant"                 
#[3] "Corolla_diameter_mean"             
#[4] "Style_length"                      
#[5] "Ovule_number"                      
#[6] "Plant_height_mean_m"
#Firt load selfing and select/rename column of interest
selfing = read_csv("Data/Trait_data/Processed/Selfing.csv")
colnames(selfing)
selfing = selfing %>% 
dplyr::select(c(Species, Average_fruits)) %>% 
rename(Autonomous_selfing_level_fruit_set = Average_fruits)
#Load morphometrics and select/rename column of interest
morphometrics = read_csv("Data/Trait_data/Raw/ReproductiveTraits_Morphometrics.csv")
colnames(morphometrics)
#Prepare reproductive data from PollObs matching Lanuza 2023 format
morphometrics = morphometrics %>% 
dplyr::select(
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

#Add selfing to morphometircs
pollobs_data = left_join(selfing, morphometrics)
#Convert species columns to rownames
pollobs_data = pollobs_data %>% 
  tibble::column_to_rownames("Species")

#For the phylo we need taxonomic info
#Retrieve it quickly from GBIF
plant_spp = rownames(pollobs_data)
#Check for futher taxonomic info
matched_gbif_plants = name_backbone_checklist(name = plant_spp, kingdom='plants')
#Select columns of interest and rename accordingly
matched_gbif_plants1 = matched_gbif_plants %>% 
dplyr::select(c(family, verbatim_name)) %>% 
rename(Family_all = family) %>% 
rename(Species_all = verbatim_name) %>% 
mutate(Genus_all = word(Species_all, 1))
#All species are equal and have same order
matched_gbif_plants1$Species_all == rownames(pollobs_data)
pollobs_data1 = bind_cols(pollobs_data, matched_gbif_plants1)

#From here, script from Lanuza 2023
#1) Load data
#read data with missing values filled by data imputation
dat = read.csv("Data/Trait_data/Func_ecol_2023/all_species_imputed_trait_data_forest_data.csv", row.names = "X")
#2) Tidy up data to get phylo distance and conduct PCA
#remove not found species, cannot do PCA with unequal numbers of rows
cols.num = c("Family_all","Genus_all","Species_all")
dat[cols.num] = sapply(dat[cols.num],as.character)
dat$Species_all = gsub("Species_all_", "", dat$Species_all)
#dat <- dat[!dat$Species_all == "Diospyros seychellarum", ]
#dat <- dat[!dat$Species_all == "Memecylon eleagni", ]
#dat <- dat[!dat$Species_all == "Ocotea laevigata", ]
#dat <- dat[!dat$Species_all == "Soulamea terminaloides", ]
#3) REMOVE OUTLIERS, OUT OF 2.5-97.5 RANGE
dat_cleaning = dat[,c("Family_all", "Genus_all", "Species_all", "Breeding_system", "Compatibility_system", "Autonomous_selfing_level",
                       "Autonomous_selfing_level_fruit_set", "Flower_morphology", "Flower_symmetry", "Flowers_per_plant","Corolla_diameter_mean",
                       "Style_length","Ovule_number","life_form", "lifespan", "Plant_height_mean_m", "Nectar_presence_absence")]
#Do it trait by trait
dat_cleaning_1 = dat_cleaning %>%
  filter(between(Flowers_per_plant, quantile(Flowers_per_plant, 0.025), quantile(Flowers_per_plant, 0.975)))

dat_cleaning_2 = dat_cleaning_1 %>%
  filter(between(Corolla_diameter_mean, quantile(Corolla_diameter_mean, 0.025), quantile(Corolla_diameter_mean, 0.975)))

dat_cleaning_3 = dat_cleaning_2 %>%
  filter(between(Style_length, quantile(Style_length, 0.025), quantile(Style_length, 0.975)))

dat_cleaning_4 = dat_cleaning_3 %>%
  filter(between(Ovule_number, quantile(Ovule_number, 0.025), quantile(Ovule_number, 0.975)))

dat_cleaning_5 = dat_cleaning_4 %>%
  filter(between(Plant_height_mean_m, quantile(Plant_height_mean_m, 0.025), quantile(Plant_height_mean_m, 0.975)))

#Store only columns of interest
dat_cleaning_5 = dat_cleaning_5 %>% 
dplyr::select(c("Autonomous_selfing_level_fruit_set","Flowers_per_plant",
         "Corolla_diameter_mean","Style_length","Ovule_number",
         "Plant_height_mean_m", "Family_all", "Genus_all", "Species_all"))
#Check data structure before including PollObs species in the dataset
str(dat_cleaning_5)
colnames(pollobs_data)
#Add PollObs set of species
#Before check if there are duplicates and drop species from Lanuza 2023
spp_to_exclude = pollobs_data1$Species_all
dat_cleaning_5 = dat_cleaning_5 %>% 
filter(!Species_all %in% spp_to_exclude)
#Now we are safe for the PCA as there are no duplicated rows
dat_cleaning_5 = bind_rows(dat_cleaning_5, pollobs_data1)

dat_cleaning_5[,c("Autonomous_selfing_level_fruit_set","Flowers_per_plant",
                  "Corolla_diameter_mean","Style_length","Ovule_number","Plant_height_mean_m")] <- log(dat_cleaning_5[,c("Autonomous_selfing_level_fruit_set","Flowers_per_plant",
                                                                                                                         "Corolla_diameter_mean","Style_length","Ovule_number","Plant_height_mean_m" )]+1)
dat_cleaning_5[,c("Autonomous_selfing_level_fruit_set","Flowers_per_plant",
                  "Corolla_diameter_mean","Style_length","Ovule_number","Plant_height_mean_m")] <- scale(dat_cleaning_5[,c("Autonomous_selfing_level_fruit_set","Flowers_per_plant",
                                                                                                                           "Corolla_diameter_mean","Style_length","Ovule_number","Plant_height_mean_m")], center = T, scale = T)
final_d <- dat_cleaning_5[,c("Autonomous_selfing_level_fruit_set","Flowers_per_plant",
                             "Corolla_diameter_mean","Style_length","Ovule_number","Plant_height_mean_m")]
#4) Get phylo
#calculate phylo 
phylo <- as.data.frame(cbind(dat_cleaning_5$Family_all, dat_cleaning_5$Genus_all, dat_cleaning_5$Species_all))
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
rownames(final_d) = dat_cleaning_5$Species_all
rownames(final_d) = gsub(" ", "_", rownames(final_d))
#Run PCA
#phyl_pca_forest <- phyl.pca(phylo_output, final_d,method="lambda",mode="cov")
#Save output
#saveRDS(phyl_pca_forest, "Data/Working_files/global_pca_phenobs_species_output.rds")

#Prepare species with a column that indicate when it belongs to PollObs
all_species = tibble(Species_all = dat_cleaning_5$Species_all)
pollobs_species = tibble(Species_all = pollobs_data1$Species_all,
                         Pollobs = "Yes")

all_species1 = left_join(all_species,pollobs_species)
all_species1 = all_species1 %>% 
mutate(Pollobs = if_else(is.na(Pollobs), "No", Pollobs))

saveRDS(all_species1, "Data/Working_files/global_pca_phenobs_species_data.rds")

####
#READ DATA
####
phyl_pca_forest <- readRDS("Data/Working_files/global_pca_phenobs_species_output.rds")
dat_cleaning_5 = readRDS("Data/Working_files/global_pca_phenobs_species_data.rds")

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

#4) PLOT PPCA
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




#Load plots
global_phenobs_pca <- function(PC, x="PC1", y="PC2") {
  # PC being a prcomp object
  data <- data.frame(PC$S)
  plot <- ggplot(data, aes_string(x=x, y=y)) 
  
  dat <- data.frame(x = data[,x], y = data[,y])
  
  #######
  #DENSITY FUNCTION
  #######
  get_density <- function(x, y, ...) {
    dens <- MASS::kde2d(x, y, ...)
    ix <- findInterval(x, dens$x)
    iy <- findInterval(y, dens$y)
    ii <- cbind(ix, iy)
    return(dens$z[ii])
  }
  
  dat$density <- get_density(dat$x, dat$y, h = c(2, 2), n = 1000) 
  
  dat$PhenObs <- as.factor(all_species1$Pollobs)
  
  plot <- plot + geom_point(data=dat, aes(-x, -y,colour=PhenObs), size=2.25,  alpha = 0.85) +
     scale_colour_manual(values=c("#1a78ab", "#cc67ff"))
  
  ########
  #ADD ARROWS 
  ########
  datapc <- data.frame(PC$L) 
  mult <- min(
    (max(data[,y]) - min(data[,y])/(max(datapc[,y])-min(datapc[,y]))),
    (max(data[,x]) - min(data[,x])/(max(datapc[,x])-min(datapc[,x])))
  )
  datapc <- transform(datapc,
                      v1 = .65 * mult * (get(x)),
                      v2 = .65 * mult * (get(y))
  )
  
  plot <- plot + geom_segment(data=datapc,linejoin="round", lineend="round",aes(x=0, y=0, xend=-v1, yend=-v2),size=1.2, arrow=arrow(length=unit(0.5,"cm")), alpha=1, color="black")
  
  percentage <- round(diag(PC$Eval) / sum(PC$Eval) * 100, 2) 
  
  plot <- plot + geom_segment(data=datapc, aes(x=0, y=0, xend=-v1, yend=-v2),size=0.8, arrow=arrow(length=unit(0,"cm")),linetype=2, alpha=0.8, color="black")
  
  rownames(PC$L) <- c("S", "FN", "FS", "SL", "ON", "PH" )
  
  PCAloadings <- data.frame(Variables = rownames(PC$L), PC$L)
  plot <- plot + annotate("text", x = -(PCAloadings$PC1*c(4.6,5.2,5.25,7.9,7.1,6)), y = -(PCAloadings$PC2*c(4.45,4.2,5.4,7.7,6,6.5)+c(0,0,0,0,-0.1,0)),
                          label = PCAloadings$Variables, color="black",size=6, fontface=2)
  
  # CHANGE THEME and reverse the y-axis if selfing should be low at the bottom
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
global_phenobs_pca(PC)

