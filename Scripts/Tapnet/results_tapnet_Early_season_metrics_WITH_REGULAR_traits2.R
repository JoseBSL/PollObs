
library(tidyverse)
Halle_tapnet_web <- readRDS("Data/Working_files/Halle_obj_TAPNET_Early_season_WITH_REGULAR_traits2.rds")
Halle_tapnet_fit <- readRDS("Data/Working_files/Halle_fit_TAPNET_Early_season_WITH_REGULAR_traits2.rds")
Halle_tapnet_gof <- readRDS("Data/Working_files/Halle_gof_TAPNET_Early_season_WITH_REGULAR_traits2.rds")

Jena_tapnet_web <- readRDS("Data/Working_files/Jena_obj_TAPNET_Early_season_WITH_REGULAR_traits2.rds")
Jena_tapnet_fit <- readRDS("Data/Working_files/Jena_fit_TAPNET_Early_season_WITH_REGULAR_traits2.rds")
Jena_tapnet_gof <- readRDS("Data/Working_files/Jena_gof_TAPNET_Early_season_WITH_REGULAR_traits2.rds")

Leipzig_tapnet_web <- readRDS("Data/Working_files/Leipzig_obj_TAPNET_Early_season_WITH_REGULAR_traits2.rds")
Leipzig_tapnet_fit <-readRDS("Data/Working_files/Leipzig_fit_TAPNET_Early_season_WITH_REGULAR_traits2.rds")
Leipzig_tapnet_gof <- readRDS("Data/Working_files/Leipzig_gof_TAPNET_Early_season_WITH_REGULAR_traits2.rds")

# Goodness of fit info-------------------------------------------------------
# Similarity between fitted and observed network expressed as Bray-Curtis 
# similarity (bc_sim_web)
Halle_tapnet_gof$bc_sim_web # Small?
Jena_tapnet_gof$bc_sim_web
Leipzig_tapnet_gof$bc_sim_web

# Correlation between fitted and observed number of interactions, 
# expressed as Spearman correlation ~0.3
Halle_tapnet_gof$cor_web
Jena_tapnet_gof$cor_web
Leipzig_tapnet_gof$cor_web


# Plot network indices computed for the observed and repeated draws from
# the fitted multinomial distribution.
Halle_tapnet_gof_df <- as.data.frame(Halle_tapnet_gof$net_indices[[1]])
Jena_tapnet_gof_df <- as.data.frame(Jena_tapnet_gof$net_indices[[1]])
Leipzig_tapnet_gof_df <- as.data.frame(Leipzig_tapnet_gof$net_indices[[1]])

Halle_tapnet_gof_df$Site <- "Halle"
Jena_tapnet_gof_df$Site <- "Jena"
Leipzig_tapnet_gof_df$Site <- "Leipzig"

combined_df <- rbind(Halle_tapnet_gof_df, Jena_tapnet_gof_df, Leipzig_tapnet_gof_df)

df_long <- combined_df %>%
  pivot_longer(cols = c(Observed, Mean, q2.5, q97.5), names_to = "Tipo", values_to = "Valor")

metric_ranges <- data.frame(
  Index = c("connectance", "NODF", "weighted NODF", "H2"),
  y_min = c(0, 0, 0, 0),
  y_max = c(1, 100, 100, 1)
)

df_long_fixed <- df_long %>% 
  left_join(metric_ranges, by = "Index")

ggplot() +
  # Error bars for simulations
  geom_errorbar(
    data = df_long_fixed %>%
      filter(Tipo %in% c("q2.5", "q97.5")) %>%
      pivot_wider(names_from = Tipo, values_from = Valor),
    aes(x = Site, ymin = q2.5, ymax = q97.5, color = Site),
    width = 0.2, position = position_dodge(width = 0.5)
  ) +
  # Simulated mean
  geom_point(
    data = df_long_fixed %>% filter(Tipo == "Mean"),
    aes(x = Site, y = Valor, color = Site),
    shape = 1, size = 3,
    position = position_dodge(width = 0.5)
  ) +
  # Observed value
  geom_point(
    data = df_long_fixed %>% filter(Tipo == "Observed"),
    aes(x = Site, y = Valor, color = Site),
    shape = 16, size = 3,
    position = position_dodge(width = 0.5)
  ) +
  # Force y-axis limits per facet
  geom_blank(data = df_long_fixed, aes(x = Site, y = y_min)) +
  geom_blank(data = df_long_fixed, aes(x = Site, y = y_max))+
  facet_wrap(~ Index, scales = "free_y") +
  ylab("Value") + xlab("Site") +
  ggtitle("Comparison of network metrics across sites for Early season") +
  theme_minimal() +
  theme(strip.text = element_text(face = "bold"))
# none of the four indices includes the observed even in the 95% confidence interval (i.e. not good).


# Model fittings--------------------------------------------------------
# Parameters: 
# 1) Delta: the width of the trait-matching function (j in eq. [3]) for each 
# pair of traits #
# 2) the width of the trait-matching function for the latent traits, and
# 3) two vectors of parameters for the construction of the latent trait (eq. [5]).

# 1) Delta
# value of 1 indicates that traits (observed and latent) are as important as the abundance.
Halle_tapnet_fit$par_opt$delta # 1
Jena_tapnet_fit$par_opt$delta # 1
Leipzig_tapnet_fit$par_opt$delta # 1


Halle_tapnet_fit$par_opt$pem_shift
# 2 the standard deviations of the trait-matching function
# When this value is large it indicates that the traits were not fitting very 
# well to each other and the model did not find the observed traits useful
Halle_tapnet_fit$par_opt$tmatch_width_pem # Look small
Jena_tapnet_fit$par_opt$tmatch_width_pem # Look small
Leipzig_tapnet_fit$par_opt$tmatch_width_pem # Look small

# check for correlation between the latent trait and the (independent) abundance of the species
Halle_fitted_lin_low <- Halle_tapnet_fit$par_opt$lat_low[which(names(Halle_tapnet_fit$par_opt$lat_low) %in%
                                                   colnames(Halle_tapnet_web$networks[[1]]$pems$low))]
Halle_fitted_lat_low <- as.vector(scale(rowSums(matrix(Halle_fitted_lin_low,
                                                 nrow = nrow (Halle_tapnet_web$networks[[1]]$pems$low),
                                                 ncol = ncol(Halle_tapnet_web$networks[[1]]$pems$low), byrow = TRUE) *
                                            Halle_tapnet_web$networks[[1]]$pems$low)))

Halle_fitted_lin_high <- Halle_tapnet_fit$par_opt$lat_high[which(names(Halle_tapnet_fit$par_opt$lat_high) %in%
                                                     colnames(Halle_tapnet_web$networks[[1]]$pems$high))]
Halle_fitted_lat_high <- as.vector(scale(
  rowSums(matrix(Halle_fitted_lin_high,nrow = nrow (Halle_tapnet_web$networks[[1]]$pems$high),
                 ncol = ncol(Halle_tapnet_web$networks[[1]]$pems$high), byrow = TRUE) *
            Halle_tapnet_web$networks[[1]]$pems$high)))

Jena_fitted_lin_low <- Jena_tapnet_fit$par_opt$lat_low[which(names(Jena_tapnet_fit$par_opt$lat_low) %in%
                                                                 colnames(Jena_tapnet_web$networks[[1]]$pems$low))]
Jena_fitted_lat_low <- as.vector(scale(rowSums(matrix(Jena_fitted_lin_low,
                                                       nrow = nrow (Jena_tapnet_web$networks[[1]]$pems$low),
                                                       ncol = ncol(Jena_tapnet_web$networks[[1]]$pems$low), byrow = TRUE) *
                                                  Jena_tapnet_web$networks[[1]]$pems$low)))

Jena_fitted_lin_high <- Jena_tapnet_fit$par_opt$lat_high[which(names(Jena_tapnet_fit$par_opt$lat_high) %in%
                                                                   colnames(Jena_tapnet_web$networks[[1]]$pems$high))]
Jena_fitted_lat_high <- as.vector(scale(
  rowSums(matrix(Jena_fitted_lin_high,nrow = nrow (Jena_tapnet_web$networks[[1]]$pems$high),
                 ncol = ncol(Jena_tapnet_web$networks[[1]]$pems$high), byrow = TRUE) *
            Jena_tapnet_web$networks[[1]]$pems$high)))

Leipzig_fitted_lin_low <- Leipzig_tapnet_fit$par_opt$lat_low[which(names(Leipzig_tapnet_fit$par_opt$lat_low) %in%
                                                               colnames(Leipzig_tapnet_web$networks[[1]]$pems$low))]
Leipzig_fitted_lat_low <- as.vector(scale(rowSums(matrix(Leipzig_fitted_lin_low,
                                                      nrow = nrow (Leipzig_tapnet_web$networks[[1]]$pems$low),
                                                      ncol = ncol(Leipzig_tapnet_web$networks[[1]]$pems$low), byrow = TRUE) *
                                                 Leipzig_tapnet_web$networks[[1]]$pems$low)))

Leipzig_fitted_lin_high <- Leipzig_tapnet_fit$par_opt$lat_high[which(names(Leipzig_tapnet_fit$par_opt$lat_high) %in%
                                                                 colnames(Leipzig_tapnet_web$networks[[1]]$pems$high))]
Leipzig_fitted_lat_high <- as.vector(scale(
  rowSums(matrix(Leipzig_fitted_lin_high,nrow = nrow (Leipzig_tapnet_web$networks[[1]]$pems$high),
                 ncol = ncol(Leipzig_tapnet_web$networks[[1]]$pems$high), byrow = TRUE) *
            Leipzig_tapnet_web$networks[[1]]$pems$high)))


# Check for correlation between the latent trait and the (independent) abundance 
# of the species
cor.test(Halle_fitted_lat_low, Halle_tapnet_web$networks[[1]]$abuns$low, method = "spearman")
cor.test(Halle_fitted_lat_high, Halle_tapnet_web$networks[[1]]$abuns$high, method = "spearman")

cor.test(Jena_fitted_lat_low, Jena_tapnet_web$networks[[1]]$abuns$low, method = "spearman")
cor.test(Jena_fitted_lat_high, Jena_tapnet_web$networks[[1]]$abuns$high, method = "spearman")

cor.test(Leipzig_fitted_lat_low, Leipzig_tapnet_web$networks[[1]]$abuns$low, method = "spearman")
cor.test(Leipzig_fitted_lat_high, Leipzig_tapnet_web$networks[[1]]$abuns$high, method = "spearman")

# Check for correlation between the latent trait and the REGULAR traits
cor.test(Halle_fitted_lat_low, Halle_tapnet_web$networks[[1]]$traits$low, method = "spearman")
cor.test(Halle_fitted_lat_high, Halle_tapnet_web$networks[[1]]$traits$high, method = "spearman")

cor.test(Jena_fitted_lat_low, Jena_tapnet_web$networks[[1]]$traits$low, method = "spearman")
cor.test(Jena_fitted_lat_high, Jena_tapnet_web$networks[[1]]$traits$high, method = "spearman")

cor.test(Leipzig_fitted_lat_low, Leipzig_tapnet_web$networks[[1]]$traits$low, method = "spearman")
cor.test(Leipzig_fitted_lat_high, Leipzig_tapnet_web$networks[[1]]$traits$high, method = "spearman")

