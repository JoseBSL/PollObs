# ======================================================
#Script: Script to test tapnet
# ======================================================

# Load libraries
library(tapnet)
library(ape)
library(dplyr)
library(stringi)

# ======================================================
# Load data
# Load philo--------------------------------------------
plant_phylo <- readRDS("Data/Working_files/plant_phylo.rds")
poll_phylo <- readRDS("Data/Working_files/bee_phylo.rds")

poll_phylo <- ape::root(poll_phylo, outgroup = poll_phylo$tip.label[1], resolve.root = TRUE)

# Sanity check (ape objects)
ape::is.rooted(plant_phylo)
ape::is.rooted(poll_phylo)

is.null(plant_phylo$edge.length)
is.null(poll_phylo$edge.length)


# Load networks---------------------------------------- 
net_by_garden_full_season <- readRDS("Data/Working_files/networks_by_garden_only_phenobs_bees.rds")

# Load cuasi-independent abundances--------------------
floral_ab_full_season_by_sp <- readr::read_csv("Data/Working_files/total_floral_abundance__by_SEASON_sp.csv")
poll_ab_full_season_by_sp <- readr::read_csv("Data/Working_files/total_poll_abundance_by_SEASON_sp.csv")


######################################################
# TAPNET
######################################################

gardens <- net_by_garden_full_season$Botanical_garden

# Leipzing---------------------------------------------
garden_number <- 1
garden_name <- gardens[garden_number]

# Interaction network
garden_full_season_net <- net_by_garden_full_season$Interaction_network[garden_number]
poll_garden_full_season_net_names <- colnames(garden_full_season_net[[1]])
plant_garden_full_season_net_names <- rownames(garden_full_season_net[[1]])

# Phylo
poll_philo_names <- poll_phylo$tip.label
plant_philo_names <- plant_phylo$tip.label

# Sanity check
poll_garden_full_season_net_names[!poll_garden_full_season_net_names %in% poll_philo_names]
plant_garden_full_season_net_names[!plant_garden_full_season_net_names %in% plant_philo_names] # OK
# pollinatir sp names need an underscore

# Modify poll names
colnames(garden_full_season_net[[1]]) <- gsub(" ", "_",poll_garden_full_season_net_names)
# Sanity check
poll_garden_full_season_net_names <- colnames(garden_full_season_net[[1]])
poll_garden_full_season_net_names[!poll_garden_full_season_net_names %in% poll_philo_names] # OK

# Abundances
floral_ab_garden_full_season <- floral_ab_full_season_by_sp %>% 
  filter(Botanical_garden == garden_name)
poll_ab_garden_full_season <- poll_ab_full_season_by_sp  %>% 
  filter(Botanical_garden == garden_name)

poll_ab_garden_full_season_names <-  gsub(" ", "_",poll_ab_garden_full_season$Pollinator)

# Sanity check
plant_garden_full_season_net_names[!plant_garden_full_season_net_names %in%  
                                     floral_ab_garden_full_season$Plant]

poll_garden_full_season_net_names[!poll_garden_full_season_net_names %in%  
                                    poll_ab_garden_full_season_names]

plant_abun_garden_full_season_vector <- setNames(
  floral_ab_garden_full_season$Total_floral_abundance,
  gsub(" ", "_",floral_ab_garden_full_season$Plant)
)

poll_abun_garden_full_season_vector <- setNames(
  poll_ab_garden_full_season$Total_pollinator_abundance,
  gsub(" ", "_",poll_ab_garden_full_season$Pollinator)
)


tapnet_web1 <- make_tapnet(tree_low = plant_phylo, tree_high = poll_phylo,
                           networks = garden_full_season_net[[1]], traits_low = NULL,
                           traits_high = NULL, abun_low = plant_abun_garden_full_season_vector,
                           abun_high = poll_abun_garden_full_season_vector, npems_lat = 4)

str(tapnet_web1) # show tapnet structure

colnames(tapnet_web1$networks[[1]]$pems$low) # names of fitted PEMs                                                                                                                                                                                            are required for prediction from web1 to web 2). Let’s check:
colnames(tapnet_web1$networks[[1]]$pems$high)

# check for correlation between the phylogenetic eigenvectors and the observed traits:
cor(cbind(tapnet_web1$networks[[1]]$pems$low, tapnet_web1$networks[[1]]$traits$low))
cor(cbind(tapnet_web1$networks[[1]]$pems$high, tapnet_web1$networks[[1]]$traits$high))

# We here assume that all trait matches are best described using a normal distribution. Alternatively, we could
# use the shifted log-normal.

fit_web1 <- fit_tapnet(tapnet = tapnet_web1, method="SANN")

# goodness of fit
gof_web1_norm <- gof_tapnet(fit_web1)
gof_web1_norm
