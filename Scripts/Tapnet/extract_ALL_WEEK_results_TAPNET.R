
library(tidyverse)


# ======================================================
# Create Season partitions------------------------------
# Interaction data
raw_data = readRDS("Data/Working_files/interaction_data.rds")

# Create tibble with unique dates 
dates = raw_data %>% 
  select(Botanical_garden, Date) %>% 
  group_by(Botanical_garden) %>% 
  distinct()

# Convert to tibble
groupped_dates = dates %>%
  group_by(Botanical_garden) %>% 
  arrange(Date, .by_group = TRUE) %>% 
  mutate(Season_group = ntile(Date, 3)) %>%
  mutate(Week = lubridate::week(Date)) %>%
  mutate(Season = case_when(
    Season_group == 1 ~ "Early",
    Season_group == 2 ~ "Mid",
    Season_group == 3 ~ "Late"
  )) %>%
  select(!Season_group) %>% 
  ungroup()

# ======================================================
# Load networks----------------------------------------- 
net_by_garden_WEEK <- readRDS("Data/Working_files/networks_by_garden_only_phenobs_bees_WEEK_ALL.rds")

gardens <- net_by_garden_WEEK$Botanical_garden %>% unique()

# Variable to store all the info
result_tapnet_ALL_WEEKS <- NULL

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
    
    # Load info
    name_tapnet_obj <- paste0("Data/Working_files/",garden_name,"_obj_TAPNET_WEEK_",week_name,"_WITH_REGULAR_traits.rds")
    name_fit <- paste0("Data/Working_files/",garden_name,"_fit_TAPNET_WEEK_",week_name,"_WITH_REGULAR_traits.rds")
    name_gof <- paste0("Data/Working_files/",garden_name,"_gof_TAPNET_WEEK_",week_name,"_WITH_REGULAR_traits.rds")
    
    tapnet_web1 <- readRDS(name_tapnet_obj)
    fit_web1 <- readRDS(name_fit)
    gof_web1_norm <- readRDS(name_gof)
    
    delta <- fit_web1$par_opt$delta
    tmatch_width_pem <- fit_web1$par_opt$tmatch_width_pem
    tmatch_width_obs <- fit_web1$par_opt$tmatch_width_obs
    
    # check for correlation between the latent trait and the (independent) abundance of the species
    fitted_lin_low <- fit_web1$par_opt$lat_low[which(names(fit_web1$par_opt$lat_low) %in%
                                                                     colnames(tapnet_web1$networks[[1]]$pems$low))]
    fitted_lat_low <- as.vector(scale(rowSums(matrix(fitted_lin_low,
                                                     nrow = nrow (tapnet_web1$networks[[1]]$pems$low),
                                                     ncol = ncol(tapnet_web1$networks[[1]]$pems$low), byrow = TRUE) *
                                                      tapnet_web1$networks[[1]]$pems$low)))
    
    fitted_lin_high <- fit_web1$par_opt$lat_high[which(names(fit_web1$par_opt$lat_high) %in%
                                                                       colnames(tapnet_web1$networks[[1]]$pems$high))]
    fitted_lat_high <- as.vector(scale(
      rowSums(matrix(fitted_lin_high,nrow = nrow (tapnet_web1$networks[[1]]$pems$high),
                     ncol = ncol(tapnet_web1$networks[[1]]$pems$high), byrow = TRUE) *
                tapnet_web1$networks[[1]]$pems$high)))
    
    # Check for correlation between the latent trait and the (independent) abundance 
    # of the species
    PEM_abundance_plant <- cor.test(fitted_lat_low, tapnet_web1$networks[[1]]$abuns$low, method = "spearman")
    rho_PEM_abundance_plant <- PEM_abundance_plant[["estimate"]] %>% as.numeric()
    rho_pvalue_PEM_abundance_plant <- PEM_abundance_plant[["p.value"]]
    
    PEM_abundance_poll <- cor.test(fitted_lat_high, tapnet_web1$networks[[1]]$abuns$high, method = "spearman")
    rho_PEM_abundance_poll <- PEM_abundance_poll[["estimate"]] %>% as.numeric()
    rho_pvalue_PEM_abundance_poll <- PEM_abundance_poll[["p.value"]]
    
    # Check for correlation between the latent trait and the REGULAR traits
    PEM_traits_plant <- cor.test(fitted_lat_low, tapnet_web1$networks[[1]]$traits$low, method = "spearman")
    rho_PEM_traits_plant <- PEM_traits_plant[["estimate"]] %>% as.numeric()
    rho_pvalue_PEM_traits_plant <- PEM_traits_plant[["p.value"]]
    
    PEM_traits_poll <- cor.test(fitted_lat_high, tapnet_web1$networks[[1]]$traits$high, method = "spearman")
    rho_PEM_traits_poll <- PEM_traits_poll[["estimate"]] %>% as.numeric()
    rho_pvalue_PEM_traits_poll <- PEM_traits_poll[["p.value"]]
    
    # Goodness of fit info-------------------------------------------------------
    # Similarity between fitted and observed network expressed as Bray-Curtis 
    bc_sim_web <- gof_web1_norm$bc_sim_web # Small?
    
    # Correlation between fitted and observed number of interactions, 
    # expressed as Spearman correlation
    cor_web <- gof_web1_norm$cor_web
    
    # Network indices computed for the observed and repeated draws from
    # the fitted multinomial distribution.
    tapnet_gof_df <- as.data.frame(gof_web1_norm$net_indices[[1]])
    
    # Pivot to long format, then combine variable and index
    reshaped_df <- tapnet_gof_df %>%
      pivot_longer(cols = -Index, names_to = "stat", values_to = "value") %>%
      unite("variable", stat, Index, sep = " ") %>%
      select(variable, value)
    
    aux_result_tapnet_ALL_WEEKS <- as.data.frame(
      variable = c()
    )
    aux_names <- c("delta", "tmatch_width_pem", "tmatch_width_obs", 
                  "rho_PEM_abundance_plant", "rho_pvalue_PEM_abundance_plant", 
                     "rho_PEM_abundance_poll", "rho_pvalue_PEM_abundance_poll", 
                     "rho_PEM_traits_plant", "rho_pvalue_PEM_traits_plant", 
                     "rho_PEM_traits_poll", "rho_pvalue_PEM_traits_poll")
    
    aux_values <- c( delta, tmatch_width_pem, tmatch_width_obs, 
                     rho_PEM_abundance_plant, rho_pvalue_PEM_abundance_plant, 
                     rho_PEM_abundance_poll, rho_pvalue_PEM_abundance_poll, 
                     rho_PEM_traits_plant, rho_pvalue_PEM_traits_plant, 
                     rho_PEM_traits_poll, rho_pvalue_PEM_traits_poll)
    
   
    
    
  }
}
    