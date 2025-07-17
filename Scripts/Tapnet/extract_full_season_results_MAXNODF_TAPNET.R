
library(tidyverse)
library(tapnet)
library(maxnodf)

# Load network info for full season
net_by_garden_full_season <- readRDS("Data/Working_files/networks_by_garden_only_phenobs_bees.rds")
gardens <- net_by_garden_full_season$Botanical_garden %>% unique()

# Create variable to store the information
maxnodf_results <- NULL

for (garden_number in 1:length(gardens)) {
  
  # Garden---------------------------------------------
  garden_name <- gardens[garden_number]
  garden_name
  
  result <- try({
    
    # Load info
    name_tapnet_obj <- paste0("Data/Working_files/",garden_name,"_obj_TAPNET_full_season_WITH_REGULAR_traits.rds")
    name_fit <- paste0("Data/Working_files/",garden_name,"_fit_TAPNET_full_season_WITH_REGULAR_traits.rds")
    name_gof <- paste0("Data/Working_files/",garden_name,"_gof_TAPNET_full_season_WITH_REGULAR_traits.rds")
    
    tapnet_web1 <- readRDS(name_tapnet_obj)
    fit_web1 <- readRDS(name_fit)
    gof_web1_norm <- readRDS(name_gof)
    
    # Generate 100 simulated networks ---
    nrep <- 100
    
    # Extract and reshape the simulations ---
    # Get the dimension of the original interaction matrix
    true_web <- tapnet_web1$networks[[1]]$web
    web_dim <- dim(true_web)
    
    # Extract multinomial samples (flattened matrices)
    fitted_sim <- rmultinom(nrep, sum(true_web), gof_web1_norm$fitted_I_mat[[1]])
    
    # Reconstruct as array of 2D networks
    simulated_networks <- array(
      data = fitted_sim,
      dim = c(web_dim[1], web_dim[2], nrep),
      dimnames = list(
        rownames(true_web),
        colnames(true_web),
        paste0("sim_", 1:nrep)
      )
    )
    
    # Estimate maxnodf for 2D networks  
    maxnodf_web1 <- NULL
    
    for(j in 1:nrep){
      
      result_j <- try({
        maxnodf_web1 <- c(maxnodf_web1,
                        maxnodf::NODFc(simulated_networks[,,j])
      )}, silent = TRUE)
      
      if (inherits(result_j, "try-error")) {
        message("Error on iteration ", garden_name, j, ". Skipping.")
      }
      
    }
    
    # Compute observed maxNODF
    observed_val <- maxnodf::NODFc(true_web)
    
    # Compute mean
    mean_val <- mean(maxnodf_web1, na.rm = TRUE)
    
    # Compute median
    median_val <- median(maxnodf_web1, na.rm = TRUE)
    
    # Compute 2.5% and 97.5% quantiles (for 95% interval)
    quantiles <- quantile(maxnodf_web1, probs = c(0.025, 0.975), na.rm = TRUE)
    
    # Combine in a named list or data frame (optional)
    summary_stats <- dplyr::tibble(
      Botanical_garden = garden_name,
      Variable = c("Observed maxNODF", "Mean maxNODF", 
                   "Median maxNODF", "q2.5 maxNODF", "q97.5 maxNODF"),
      Value = c(observed_val, mean_val, median_val, 
                as.numeric(quantiles[1]), as.numeric(quantiles[2])),
      Season = "Full",
      Week = NA,
      Type = "Full"
      )
    
    
    # Storage results for the corresponding botanical garden
    maxnodf_results <- bind_rows(maxnodf_results, summary_stats)

    
  }, silent = TRUE)
  
  if (inherits(result, "try-error")) {
    message("Error on iteration ", garden_name, ". Skipping.")
  }
  
  
}

# Save results
readr::write_csv(maxnodf_results, "Data/Working_files/results_maxnodf_TAPNET_FULL_SEASON_traits.csv")
