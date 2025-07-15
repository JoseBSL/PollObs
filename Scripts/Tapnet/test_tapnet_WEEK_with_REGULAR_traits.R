# ======================================================
#Script: Script to test tapnet
# ======================================================

# Load libraries
library(tapnet)
library(ape)
library(dplyr)
library(stringi)
library(readxl)
# ======================================================
# Load data
# Load philo--------------------------------------------
plant_phylo_full <- readRDS("Data/Working_files/plant_phylo.rds")
poll_phylo_full <- readRDS("Data/Working_files/bee_phylo.rds")

poll_phylo_full <- ape::root(poll_phylo_full, outgroup = poll_phylo_full$tip.label[1], resolve.root = TRUE)

# Sanity check (ape objects)
ape::is.rooted(plant_phylo_full)
ape::is.rooted(poll_phylo_full)

is.null(plant_phylo_full$edge.length)
is.null(poll_phylo_full$edge.length)


# Load networks---------------------------------------- 
net_by_garden_WEEK <- readRDS("Data/Working_files/networks_by_garden_only_phenobs_bees_WEEK.rds")

# Load cuasi-independent abundances--------------------
floral_ab_WEEK_by_sp <- readr::read_csv("Data/Working_files/total_floral_abundance_by_WEEK_sp.csv")
poll_ab_WEEK_by_sp <- readr::read_csv("Data/Working_files/total_poll_abundance_by_WEEK_sp.csv")

# Load PCA traits--------------------------------------
trait_axes_polls <- readRDS("Data/Trait_data/Processed/PollTraits_all.rds") %>% 
  dplyr::select(Pollinator_accepted_name, IT_mean) %>% rename(Pollinator = Pollinator_accepted_name)
trait_axes_plants <- readRDS("Data/Working_files/plants_REAL_trait_phenobs.rds") %>% 
  dplyr::select(Species,Floral_tube_width) %>% rename(Plant = Species)

######################################################
# TAPNET
######################################################

gardens <- net_by_garden_WEEK$Botanical_garden %>% unique()

for (garden_number in 1:length(gardens)) {
  
  # Garden---------------------------------------------
  garden_name <- gardens[garden_number]
  garden_name
  
  week <- net_by_garden_WEEK %>%
    dplyr::filter(Botanical_garden == garden_name) %>% 
    dplyr::select(Week) %>% pull() %>% sort()
  
  for (week_number in 1:length(week)) {

    # Week --------------------------------------------
    week_name <- week[week_number]
    week_name
    
    # Interaction network
    
    row_4_int_network_garden_week <- which(net_by_garden_WEEK$Botanical_garden == garden_name &
                                         net_by_garden_WEEK$Week == week_name)
    
    garden_WEEK_net <- net_by_garden_WEEK$Interaction_network[row_4_int_network_garden_week]
    poll_garden_WEEK_net_names <- colnames(garden_WEEK_net[[1]])
    plant_garden_WEEK_net_names <- rownames(garden_WEEK_net[[1]])
    
    # Phylo
    
    plant_phylo <- ape::keep.tip(plant_phylo_full, plant_garden_WEEK_net_names)
    poll_phylo <- ape::keep.tip(poll_phylo_full, gsub(" ","_",poll_garden_WEEK_net_names))
    
    poll_philo_names <- poll_phylo$tip.label
    plant_philo_names <- plant_phylo$tip.label
    
    # Sanity check
    poll_garden_WEEK_net_names[!poll_garden_WEEK_net_names %in% poll_philo_names]
    plant_garden_WEEK_net_names[!plant_garden_WEEK_net_names %in% plant_philo_names] # OK
    # pollinatir sp names need an underscore
    
    # Modify poll names
    colnames(garden_WEEK_net[[1]]) <- gsub(" ", "_",poll_garden_WEEK_net_names)
    # Sanity check
    poll_garden_WEEK_net_names <- colnames(garden_WEEK_net[[1]])
    poll_garden_WEEK_net_names[!poll_garden_WEEK_net_names %in% poll_philo_names] # OK
    
    # Abundances
    floral_ab_garden_WEEK_raw <- floral_ab_WEEK_by_sp %>% 
      dplyr::filter(Botanical_garden == garden_name)
    poll_ab_garden_WEEK_raw <- poll_ab_WEEK_by_sp  %>% 
      dplyr::filter(Botanical_garden == garden_name)
    
    # Replace sp name by accepted sp name
    plant_names_raw_data <-  readRDS("Data/Working_files/interaction_data.rds") %>%
      dplyr::select(Plant,Plant_accepted_name) %>% unique() %>% sort()
    poll_names_raw_data <-  readRDS("Data/Working_files/interaction_data.rds") %>%
      dplyr::select(Pollinator, Pollinator_accepted_name) %>% unique() %>% sort()
    
    floral_ab_garden_WEEK <- floral_ab_garden_WEEK_raw %>%
      left_join(plant_names_raw_data, by = "Plant") %>% 
      rename(Old_Plant = Plant, 
             Plant = Plant_accepted_name) %>%
      dplyr::filter(Plant %in% rownames(garden_WEEK_net[[1]]), Week == week_name)
    
    poll_ab_garden_WEEK <- poll_ab_garden_WEEK_raw %>%
      left_join(poll_names_raw_data, by = "Pollinator") %>% 
      rename(Old_Pollinator = Pollinator, Pollinator = Pollinator_accepted_name) %>%
      mutate(Pollinator = gsub(" ", "_", Pollinator)) %>%
      dplyr::filter(Pollinator %in% colnames(garden_WEEK_net[[1]]), Week == week_name)
    
    # Sanity check
    floral_ab_garden_WEEK %>% dplyr::filter(is.na(Plant))
    poll_ab_garden_WEEK %>% dplyr::filter(is.na(Pollinator))
    
    # Fix pollinator names
    poll_ab_garden_partial_week_names <-  gsub(" ", "_",poll_ab_garden_WEEK$Pollinator)
    
    # Sanity check
    plant_garden_WEEK_net_names[!plant_garden_WEEK_net_names %in%  
                                         floral_ab_garden_WEEK$Plant]
    
    poll_garden_WEEK_net_names[!poll_garden_WEEK_net_names %in%  
                                        poll_ab_garden_partial_week_names]
    
    plant_abun_garden_partial_season_vector <- setNames(
      floral_ab_garden_WEEK$Total_floral_abundance,
      floral_ab_garden_WEEK$Plant
    )
    
    poll_abun_garden_partial_season_vector <- setNames(
      poll_ab_garden_WEEK$Total_pollinator_abundance,
      gsub(" ", "_",poll_ab_garden_WEEK$Pollinator)
    )
    
    # Sp names in trait information-----------------------------------------------
    
    # Sanity check----------------------------------------------------------------
    trait_axes_plants$Plant[!trait_axes_plants$Plant %in% plant_names_raw_data$Plant]
    trait_axes_polls$Pollinator[!trait_axes_polls$Pollinator %in% poll_names_raw_data$Pollinator]
    
    # Fix names
    trait_axes_plants$Plant[trait_axes_plants$Plant == "Penstemon bradburyi"] <- "Penstemon grandiflorus"
    trait_axes_plants$Plant[trait_axes_plants$Plant == "Anemone sylvestris"] <- "Anemonoides sylvestris"
    trait_axes_plants$Plant[trait_axes_plants$Plant == "Aquilegia chrysantha"] <- "Aquilegia vulgaris"
    trait_axes_plants$Plant[trait_axes_plants$Plant == "Anemone nemorosa"] <- "Anemonoides nemorosa"
    
    trait_axes_polls$Pollinator[trait_axes_polls$Pollinator == "Andrena gelriae"] <- "Andrena cf. gelriae"
    trait_axes_polls$Pollinator[trait_axes_polls$Pollinator == "Coelioxys elongatus"] <- "Coelioxys elongata"
    trait_axes_polls$Pollinator[trait_axes_polls$Pollinator == "Megachile alpicola"] <- "Megachila alpicola"
    
    # Recheck
    trait_axes_plants$Plant[!trait_axes_plants$Plant %in% plant_names_raw_data$Plant]
    trait_axes_polls$Pollinator[!trait_axes_polls$Pollinator %in% poll_names_raw_data$Pollinator]
    
    floral_trait_axes_WEEK <- trait_axes_plants %>%
      left_join(plant_names_raw_data, by = "Plant") %>% 
      rename(Old_Plant = Plant, 
             Plant = Plant_accepted_name) %>%
      dplyr::filter(Plant %in% rownames(garden_WEEK_net[[1]]))
    
    poll_trait_axes_WEEK <- trait_axes_polls %>%
      left_join(poll_names_raw_data, by = "Pollinator") %>% 
      rename(Old_Pollinator = Pollinator, Pollinator = Pollinator_accepted_name) %>%
      mutate(Pollinator = gsub(" ", "_", Pollinator)) %>%
      dplyr::filter(Pollinator %in% colnames(garden_WEEK_net[[1]]))
    
    
    # Sanity check
    floral_trait_axes_WEEK %>% dplyr::filter(is.na(Plant)) # No NAs OK
    poll_trait_axes_WEEK %>% dplyr::filter(is.na(Pollinator)) # No NAs OK
    
    # Sanity check
    plant_garden_WEEK_net_names[!plant_garden_WEEK_net_names %in%  
                                         floral_trait_axes_WEEK$Plant]
    
    poll_garden_WEEK_net_names[!poll_garden_WEEK_net_names %in%  
                                        poll_trait_axes_WEEK$Pollinator]
    
    tapnet_floral_trait_axes_WEEK <- floral_trait_axes_WEEK %>%
      dplyr::select(Floral_tube_width)
    row.names(tapnet_floral_trait_axes_WEEK) <- floral_trait_axes_WEEK$Plant
    
    tapnet_poll_trait_axes_WEEK <- poll_trait_axes_WEEK %>%
      dplyr::select(IT_mean)
    row.names(tapnet_poll_trait_axes_WEEK) <- poll_trait_axes_WEEK$Pollinator
    
    # Create tapnet object--------------------------------------------------------
    
    tapnet_web1 <- make_tapnet(tree_low = plant_phylo, tree_high = poll_phylo,
                               networks = garden_WEEK_net[[1]],
                               traits_low = tapnet_floral_trait_axes_WEEK %>% as.matrix(),
                               traits_high = tapnet_poll_trait_axes_WEEK %>% as.matrix(), 
                               abun_low = plant_abun_garden_partial_season_vector,
                               abun_high = poll_abun_garden_partial_season_vector, npems_lat = 60)
    
    
    # str(tapnet_web1) # show tapnet structure
    # 
    # colnames(tapnet_web1$networks[[1]]$pems$low) # names of fitted PEMs                                                                                                                                                                                            are required for prediction from web1 to web 2). Let’s check:
    # colnames(tapnet_web1$networks[[1]]$pems$high)
    # 
    # # check for correlation between the phylogenetic eigenvectors and the observed traits:
    # cor(cbind(tapnet_web1$networks[[1]]$pems$low, tapnet_web1$networks[[1]]$traits$low))
    # cor(cbind(tapnet_web1$networks[[1]]$pems$high, tapnet_web1$networks[[1]]$traits$high))
    
    # We here assume that all trait matches are best described using a normal distribution. Alternatively, we could
    # use the shifted log-normal.
    
    fit_web1 <- fit_tapnet(tapnet = tapnet_web1, method="SANN")
    
    # goodness of fit
    gof_web1_norm <- gof_tapnet(fit_web1)
    
    # save info
    name_tapnet_obj <- paste0("Data/Working_files/",garden_name,"_obj_TAPNET_WEEK_",week_name,"_WITH_REGULAR_traits.rds")
    name_fit <- paste0("Data/Working_files/",garden_name,"_fit_TAPNET_WEEK_",week_name,"_WITH_REGULAR_traits.rds")
    name_gof <- paste0("Data/Working_files/",garden_name,"_gof_TAPNET_WEEK_",week_name,"_WITH_REGULAR_traits.rds")
    
    saveRDS(tapnet_web1, name_tapnet_obj)
    saveRDS(fit_web1, name_fit)
    saveRDS(gof_web1_norm, name_gof)
    
  }
  
}
  
  

