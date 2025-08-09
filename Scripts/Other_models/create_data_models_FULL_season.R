library(tapnet)
library(tidyverse)

# Define aux. functions to extract latent traits ------------------------------
extract_latent_traits_low <- function(tapnet_fit, tapnet_web){
  
  fitted_lin_low <- tapnet_fit$par_opt$lat_low[which(names(tapnet_fit$par_opt$lat_low) %in%
                                                       colnames(tapnet_web$networks[[1]]$pems$low))]
  fitted_lat_low <- as.vector(scale(rowSums(matrix(fitted_lin_low,
                                                   nrow = nrow (tapnet_web$networks[[1]]$pems$low),
                                                   ncol = ncol(tapnet_web$networks[[1]]$pems$low), byrow = TRUE) *
                                              tapnet_web$networks[[1]]$pems$low)))
  
  return(fitted_lat_low)
}

extract_latent_traits_high <- function(tapnet_fit, tapnet_web){
  
  fitted_lin_high <- tapnet_fit$par_opt$lat_high[which(names(tapnet_fit$par_opt$lat_high) %in%
                                                         colnames(tapnet_web$networks[[1]]$pems$high))]
  fitted_lat_high <- as.vector(scale(
    rowSums(matrix(fitted_lin_high,nrow = nrow (tapnet_web$networks[[1]]$pems$high),
                   ncol = ncol(tapnet_web$networks[[1]]$pems$high), byrow = TRUE) *
              tapnet_web$networks[[1]]$pems$high)))
  
  return(fitted_lat_high)
}

# Create a variable to storage all the information
data_model_full <- NULL

# Load networks---------------------------------------- 
net_by_garden_full_season <- readRDS("Data/Working_files/networks_by_garden_only_phenobs_bees.rds")

# Extract gardens' names
gardens <- net_by_garden_full_season$Botanical_garden

for (garden_number in 1:length(gardens)) {
  
  # Garden---------------------------------------------
  garden_name <- gardens[garden_number]
  garden_name
  
  name_tapnet_obj <- paste0("Data/Working_files/",garden_name,"_obj_TAPNET_full_season_WITH_REGULAR_traits.rds")
  name_fit <- paste0("Data/Working_files/",garden_name,"_fit_TAPNET_full_season_WITH_REGULAR_traits.rds")
  
  # Load tapnet info for that garden
  tapnet_web <- readRDS(name_tapnet_obj)
  tapnet_fit <- readRDS(name_fit)
  
  # Extract interactions -----------------------------------------------
  int_mat <- tapnet_web[["networks"]][[1]]$web
  
  # Convert to data frame
  int_df_aux <- as.data.frame(as.table(int_mat))
  
  # Rename columns
  colnames(int_df_aux) <- c("Plant", "Pollinator", "Interactions")
  
  # Remove non-interacting pairs
  int_df <- int_df_aux #%>% dplyr::filter(Interactions > 0)
  
  # Extract traits------------------------------------------------------------
  
  plant_traits <- tapnet_web[["traits_all"]][["low"]] %>% as.data.frame() %>%
    mutate(Plant = rownames(tapnet_web[["traits_all"]][["low"]])) %>%
    arrange(Plant) %>%
    mutate(Plant_latent_trait = extract_latent_traits_low(tapnet_fit, tapnet_web))
  pollinator_traits <- tapnet_web[["traits_all"]][["high"]] %>% as.data.frame() %>%
    mutate(Pollinator = rownames(tapnet_web[["traits_all"]][["high"]])) %>%
    arrange(Pollinator) %>%
    mutate(Pollinator_latent_trait = extract_latent_traits_high(tapnet_fit, tapnet_web))
  
  # Extract abundances---------------------------------------------------------
  
  plant_abundances <- tapnet_web[["networks"]][[1]]$abuns$low %>% as.data.frame() %>%
    mutate(Plant = sort(rownames(tapnet_web[["traits_all"]][["low"]])))
  colnames(plant_abundances) <- c("Plant_abundance", "Plant")
  
  pollinator_abundances <- tapnet_web[["networks"]][[1]]$abuns$high %>% as.data.frame() %>%
    mutate(Pollinator = sort(rownames(tapnet_web[["traits_all"]][["high"]])))
  colnames(pollinator_abundances) <- c("Pollinator_abundance", "Pollinator")
  
  # Create data for model ---------------------------------------------------
  data_model_garden <- int_df %>%
    left_join(plant_abundances, by = "Plant") %>%
    left_join(pollinator_abundances, by = "Pollinator") %>%
    left_join(plant_traits, by = "Plant") %>%
    left_join(pollinator_traits, by = "Pollinator") %>%
    mutate(diff_real_traits = Floral_tube_width - IT_mean,
           diff_latent_traits = Plant_latent_trait - Pollinator_latent_trait,
           Botanical_garden = garden_name)
  
  
  data_model_full <- bind_rows(data_model_full, data_model_garden)

}

# save info
name_file <- paste0("Data/Working_files/data_models_full_season_WITH_REGULAR_traits.rds")
saveRDS(data_model_full, name_file)
