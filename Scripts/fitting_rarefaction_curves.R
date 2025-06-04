
library(tidyverse)
library(patchwork)

rarefied_curves <- readRDS("Data/Working_files/rarefied_curves.rds")
mean_rarefied <- readRDS("Data/Working_files/mean_rarefied_curve.rds")

# Create a group variable for garden and sampling
mean_rarefied$Group <- paste(mean_rarefied$Botanical_garden, mean_rarefied$Sampling, sep = "_")
rarefied_curves$Group <- paste(rarefied_curves$Botanical_garden, rarefied_curves$Sampling, sep = "_")

# Plot 3D
plot_ly(mean_rarefied,
        x = ~Mean_cumulative_time,
        y = ~Plant_sampled,
        z = ~Mean_cumulative_spp,
        color = ~Group,
        colors = "Set1",  # Puedes probar también "Set2", "Dark2", etc.
        type = "scatter3d",
        mode = "lines+markers",
        marker = list(size = 3),
        line = list(width = 4)) %>%
  layout(
    scene = list(
      xaxis = list(title = "Cumulative Time (min)"),
      yaxis = list(title = "Plant Sampled"),
      zaxis = list(title = "Cumulative Species Richness")
    )
  )


ggplot(rarefied_curves, aes(x=Cumulative_time, y = Unique_spp, color = as.factor(Group)))+
  geom_point()

ggplot(mean_rarefied, aes(x=Plant_sampled, y = Mean_cumulative_spp, color = as.factor(Group)))+
  geom_point()+
  theme_bw()

ggplot(mean_rarefied, aes(x=log(Plant_sampled), y = log(Mean_cumulative_spp), color = as.factor(Group)))+
  geom_point()+
  theme_bw()

ggplot(mean_rarefied, aes(x=Mean_cumulative_time, y = Mean_cumulative_spp, color = as.factor(Group)))+
  geom_point()+
  theme_bw()

ggplot(mean_rarefied, aes(x=Mean_cumulative_time, y = Plant_sampled, color = as.factor(Group)))+
  geom_point()+
  theme_bw()



Halle_Focal_points <- mean_rarefied %>% filter(Group == "Halle_Focal")
Halle_Random_census_points <- mean_rarefied %>% filter(Group == "Halle_Random_census")
Halle_Focal_points$scl_Mean_cumulative_spp <- scale(Halle_Focal_points$Mean_cumulative_spp)
Halle_Focal_points$scl_Mean_cumulative_time <- scale(Halle_Focal_points$Mean_cumulative_time)
Halle_Random_census_points$scl_Mean_cumulative_spp <- scale(Halle_Random_census_points$Mean_cumulative_spp)
Halle_Random_census_points$scl_Mean_cumulative_time <- scale(Halle_Random_census_points$Mean_cumulative_time)

Jena_Focal_points <- mean_rarefied %>% filter(Group == "Jena_Focal")
Jena_Random_census_points <- mean_rarefied %>% filter(Group == "Jena_Random_census")
Jena_Focal_points$scl_Mean_cumulative_spp <- scale(Jena_Focal_points$Mean_cumulative_spp)
Jena_Focal_points$scl_Mean_cumulative_time <- scale(Jena_Focal_points$Mean_cumulative_time)
Jena_Random_census_points$scl_Mean_cumulative_spp <- scale(Jena_Random_census_points$Mean_cumulative_spp)
Jena_Random_census_points$scl_Mean_cumulative_time <- scale(Jena_Random_census_points$Mean_cumulative_time)


Leipzig_Focal_points <- mean_rarefied %>% filter(Group == "Leipzig_Focal")
Leipzig_Random_census_points <- mean_rarefied %>% filter(Group == "Leipzig_Random_census")
Leipzig_Focal_points$scl_Mean_cumulative_spp <- scale(Leipzig_Focal_points$Mean_cumulative_spp)
Leipzig_Focal_points$scl_Mean_cumulative_time <- scale(Leipzig_Focal_points$Mean_cumulative_time)
Leipzig_Random_census_points$scl_Mean_cumulative_spp <- scale(Leipzig_Random_census_points$Mean_cumulative_spp)
Leipzig_Random_census_points$scl_Mean_cumulative_time <- scale(Leipzig_Random_census_points$Mean_cumulative_time)

###########################################################
# Results for Halle----------------------------------------
###########################################################

nlm_Halle_focal <- nls(Mean_cumulative_spp ~ a * (1 - exp(-Plant_sampled / b)), start = list(a = 1, b = 1),
                       data = Halle_Focal_points)
summary(nlm_Halle_focal)



# Add predicted values
Halle_focal_points$Predicted_nlm <- predict(nlm_Halle_focal, newdata = Halle_focal_points, type = "response")

# Plot Plant_sampled vs. Mean_cumulative_spp with prediction line

ggplot(data = Halle_focal_points, aes(x = Plant_sampled, y = Mean_cumulative_spp)) +
  geom_point(color = "black", alpha = 0.7) +
  geom_line(aes(x = Plant_sampled, y = Predicted_nlm), color = "blue", size = 1.2) +
  labs(
    title = "Model Predictions vs. Observed Data (Halle focal)",
    x = "Plant Sampled",
    y = "Mean Cumulative Species"
  ) +
  theme_minimal()

ggplot(Halle_focal_points, aes(y = Mean_cumulative_spp, x = Mean_cumulative_time)) +
  geom_point(color = "black", alpha = 0.7) +
  geom_line(aes(y = Predicted_nlm), color = "blue", size = 1.2) +
  labs(
    title = "Model Predictions vs. Observed Data (Halle focal)",
    x = "Mean Cumulative Time",
    y = "Mean Cumulative Species"
  ) +
  theme_minimal()


# Fit the nonlinear model for Random_census
nlm_Halle_Random_census <- 
 nls(Mean_cumulative_spp ~ a * (1 - exp(-Plant_sampled / b)), start = list(a = 1, b = 1),
     data = Halle_Random_census_points)
summary(nlm_Halle_Random_census)


# Add predicted values (on response scale)
Halle_Random_census_points$Predicted_nlm <- predict(nlm_Halle_Random_census, newdata = Halle_Random_census_points, type = "response")

# Plot Plant_sampled vs. Mean_cumulative_spp with prediction line

ggplot(data = Halle_Random_census_points, aes(x = Plant_sampled, y = Mean_cumulative_spp)) +
  geom_point(color = "black", alpha = 0.7) +
  geom_line(aes(x = Plant_sampled, y = Predicted_nlm), color = "blue", size = 1.2) +
  labs(
    title = "Model Predictions vs. Observed Data (Halle Random census)",
    x = "Plant Sampled",
    y = "Mean Cumulative Species"
  ) +
  theme_minimal()

ggplot(Halle_Random_census_points, aes(y = Mean_cumulative_spp, x = Mean_cumulative_time)) +
  geom_point(color = "black", alpha = 0.7) +
  geom_line(aes(y = Predicted_nlm), color = "blue", size = 1.2) +
  labs(
    title = "Model Predictions vs. Observed Data (Halle Random census)",
    x = "Mean Cumulative Time",
    y = "Mean Cumulative Species"
  ) +
  theme_minimal()

# Plot resulting curves

coef_Halle_Random <- coef(nlm_Halle_Random_census)
coef_Halle_Focal <- coef(nlm_Halle_focal)
plants <- seq(0,500,by=10)
fits_Halle <- data.frame(Plant_sampled = plants,
                         Richness_Random = coef_Halle_Random["a"] * (1 - exp(-plants/coef_Halle_Random["b"])),
                         Richness_Focal = coef_Halle_Focal["a"] * (1 - exp(-plants/coef_Halle_Focal["b"])))

plot_Halle <- ggplot(data = fits_Halle, aes(x = Plant_sampled, y = Richness_Random))+
  geom_path(color = "red")+
  geom_path(data = fits_Halle, aes(x = Plant_sampled, y = Richness_Focal), color = "blue")+
  geom_hline(yintercept = coef_Halle_Random["a"],color = "red", linetype = "dashed")+
  geom_hline(yintercept = coef_Halle_Focal["a"],color = "blue", linetype = "dashed")+
  geom_point(data = Halle_Random_census_points, aes(x = Plant_sampled, y = Mean_cumulative_spp,
                                                    color = Mean_cumulative_time))+
  geom_point(data = Halle_Focal_points, aes(x = Plant_sampled, y = Mean_cumulative_spp,
                                            color = Mean_cumulative_time))+
  labs(title = "Halle", x = "Plants sampled", y = "Pollinator richness")+
  annotate("text", x = max(fits_Halle$Plant_sampled), y = max(fits_Halle$Richness_Random),
           label = "Random", color = "red", hjust = 1) +
  annotate("text", x = max(fits_Halle$Plant_sampled), y = max(fits_Halle$Richness_Focal),
           label = "Focal", color = "blue", hjust = 1) +
  theme_bw()




###########################################################
# Results for Jena----------------------------------------
###########################################################


nlm_Jena_focal <- nls(Mean_cumulative_spp ~ a * (1 - exp(-Plant_sampled / b)), start = list(a = 1, b = 1),
                       data = Jena_Focal_points)
summary(nlm_Jena_focal)



# Add predicted values
Jena_Focal_points$Predicted_nlm <- predict(nlm_Jena_focal, newdata = Jena_Focal_points, type = "response")

# Plot Plant_sampled vs. Mean_cumulative_spp with prediction line

ggplot(data = Jena_Focal_points, aes(x = Plant_sampled, y = Mean_cumulative_spp)) +
  geom_point(color = "black", alpha = 0.7) +
  geom_line(aes(x = Plant_sampled, y = Predicted_nlm), color = "blue", size = 1.2) +
  labs(
    title = "Model Predictions vs. Observed Data (Jena focal)",
    x = "Plant Sampled",
    y = "Mean Cumulative Species"
  ) +
  theme_minimal()

ggplot(Jena_Focal_points, aes(y = Mean_cumulative_spp, x = Mean_cumulative_time)) +
  geom_point(color = "black", alpha = 0.7) +
  geom_line(aes(y = Predicted_nlm), color = "blue", size = 1.2) +
  labs(
    title = "Model Predictions vs. Observed Data (Jena focal)",
    x = "Mean Cumulative Time",
    y = "Mean Cumulative Species"
  ) +
  theme_minimal()

################################

# Fit the nonlinear model for Random_census
nlm_Jena_Random_census <- 
  nls(Mean_cumulative_spp ~ a * (1 - exp(-Plant_sampled / b)), start = list(a = 1, b = 1),
      data = Jena_Random_census_points)
summary(nlm_Jena_Random_census)


# Add predicted values (on response scale)
Jena_Random_census_points$Predicted_nlm <- predict(nlm_Jena_Random_census, newdata = Jena_Random_census_points, type = "response")

# Plot Plant_sampled vs. Mean_cumulative_spp with prediction line

ggplot(data = Jena_Random_census_points, aes(x = Plant_sampled, y = Mean_cumulative_spp)) +
  geom_point(color = "black", alpha = 0.7) +
  geom_line(aes(x = Plant_sampled, y = Predicted_nlm), color = "blue", size = 1.2) +
  labs(
    title = "Model Predictions vs. Observed Data (Jena Random census)",
    x = "Plant Sampled",
    y = "Mean Cumulative Species"
  ) +
  theme_minimal()

ggplot(Jena_Random_census_points, aes(y = Mean_cumulative_spp, x = Mean_cumulative_time)) +
  geom_point(color = "black", alpha = 0.7) +
  geom_line(aes(y = Predicted_nlm), color = "blue", size = 1.2) +
  labs(
    title = "Model Predictions vs. Observed Data (Jena Random census)",
    x = "Mean Cumulative Time",
    y = "Mean Cumulative Species"
  ) +
  theme_minimal()

# Plot resulting curves

coef_Jena_Random <- coef(nlm_Jena_Random_census)
coef_Jena_Focal <- coef(nlm_Jena_focal)
plants <- seq(0,500,by=10)
fits_Jena <- data.frame(Plant_sampled = plants,
                         Richness_Random = coef_Jena_Random["a"] * (1 - exp(-plants/coef_Jena_Random["b"])),
                         Richness_Focal = coef_Jena_Focal["a"] * (1 - exp(-plants/coef_Jena_Focal["b"])))

plot_Jena <- ggplot(data = fits_Jena, aes(x = Plant_sampled, y = Richness_Random))+
  geom_path(color = "red")+
  geom_path(data = fits_Jena, aes(x = Plant_sampled, y = Richness_Focal), color = "blue")+
  geom_hline(yintercept = coef_Jena_Random["a"],color = "red", linetype = "dashed")+
  geom_hline(yintercept = coef_Jena_Focal["a"],color = "blue", linetype = "dashed")+
  geom_point(data = Jena_Random_census_points, aes(x = Plant_sampled, y = Mean_cumulative_spp,
                                                   color = Mean_cumulative_time))+
  geom_point(data = Jena_Focal_points, aes(x = Plant_sampled, y = Mean_cumulative_spp,
                                           color = Mean_cumulative_time))+
  labs(title = "Jena", x = "Plants sampled", y = "Pollinator richness")+
  annotate("text", x = max(fits_Jena$Plant_sampled), y = max(fits_Jena$Richness_Random),
           label = "Random", color = "red", hjust = 1) +
  annotate("text", x = max(fits_Jena$Plant_sampled), y = max(fits_Jena$Richness_Focal),
           label = "Focal", color = "blue", hjust = 1) +
  theme_bw()


###########################################################
# Results for Leipzig----------------------------------------
###########################################################


nlm_Leipzig_focal <- nls(Mean_cumulative_spp ~ a * (1 - exp(-Plant_sampled / b)), start = list(a = 1, b = 1),
                      data = Leipzig_Focal_points)
summary(nlm_Leipzig_focal)



# Add predicted values
Leipzig_Focal_points$Predicted_nlm <- predict(nlm_Leipzig_focal, newdata = Leipzig_Focal_points, type = "response")

# Plot Plant_sampled vs. Mean_cumulative_spp with prediction line

ggplot(data = Leipzig_Focal_points, aes(x = Plant_sampled, y = Mean_cumulative_spp)) +
  geom_point(color = "black", alpha = 0.7) +
  geom_line(aes(x = Plant_sampled, y = Predicted_nlm), color = "blue", size = 1.2) +
  labs(
    title = "Model Predictions vs. Observed Data (Leipzig focal)",
    x = "Plant Sampled",
    y = "Mean Cumulative Species"
  ) +
  theme_minimal()

ggplot(Leipzig_Focal_points, aes(y = Mean_cumulative_spp, x = Mean_cumulative_time)) +
  geom_point(color = "black", alpha = 0.7) +
  geom_line(aes(y = Predicted_nlm), color = "blue", size = 1.2) +
  labs(
    title = "Model Predictions vs. Observed Data (Leipzig focal)",
    x = "Mean Cumulative Time",
    y = "Mean Cumulative Species"
  ) +
  theme_minimal()

################################

# Fit the nonlinear model for Random_census
nlm_Leipzig_Random_census <- 
  nls(Mean_cumulative_spp ~ a * (1 - exp(-Plant_sampled / b)), start = list(a = 1, b = 1),
      data = Leipzig_Random_census_points)
summary(nlm_Leipzig_Random_census)


# Add predicted values (on response scale)
Leipzig_Random_census_points$Predicted_nlm <- predict(nlm_Leipzig_Random_census, newdata = Leipzig_Random_census_points, type = "response")

# Plot Plant_sampled vs. Mean_cumulative_spp with prediction line

ggplot(data = Leipzig_Random_census_points, aes(x = Plant_sampled, y = Mean_cumulative_spp)) +
  geom_point(color = "black", alpha = 0.7) +
  geom_line(aes(x = Plant_sampled, y = Predicted_nlm), color = "blue", size = 1.2) +
  labs(
    title = "Model Predictions vs. Observed Data (Leipzig Random census)",
    x = "Plant Sampled",
    y = "Mean Cumulative Species"
  ) +
  theme_minimal()

ggplot(Leipzig_Random_census_points, aes(y = Mean_cumulative_spp, x = Mean_cumulative_time)) +
  geom_point(color = "black", alpha = 0.7) +
  geom_line(aes(y = Predicted_nlm), color = "blue", size = 1.2) +
  labs(
    title = "Model Predictions vs. Observed Data (Leipzig Random census)",
    x = "Mean Cumulative Time",
    y = "Mean Cumulative Species"
  ) +
  theme_minimal()

# Plot resulting curves

coef_Leipzig_Random <- coef(nlm_Leipzig_Random_census)
coef_Leipzig_Focal <- coef(nlm_Leipzig_focal)
plants <- seq(0,500,by=10)
fits_Leipzig <- data.frame(Plant_sampled = plants,
                        Richness_Random = coef_Leipzig_Random["a"] * (1 - exp(-plants/coef_Leipzig_Random["b"])),
                        Richness_Focal = coef_Leipzig_Focal["a"] * (1 - exp(-plants/coef_Leipzig_Focal["b"])))

plot_Leipzig <- ggplot(data = fits_Leipzig, aes(x = Plant_sampled, y = Richness_Random))+
  geom_path(color = "red")+
  geom_path(data = fits_Leipzig, aes(x = Plant_sampled, y = Richness_Focal), color = "blue")+
  geom_hline(yintercept = coef_Leipzig_Random["a"],color = "red", linetype = "dashed")+
  geom_hline(yintercept = coef_Leipzig_Focal["a"],color = "blue", linetype = "dashed")+
  geom_point(data = Leipzig_Random_census_points, aes(x = Plant_sampled, y = Mean_cumulative_spp,
                                                      color = Mean_cumulative_time))+
  geom_point(data = Leipzig_Focal_points, aes(x = Plant_sampled, y = Mean_cumulative_spp,
                                              color = Mean_cumulative_time))+
  labs(title = "Leipzig", x = "Plants sampled", y = "Pollinator richness")+
  annotate("text", x = max(fits_Leipzig$Plant_sampled), y = max(fits_Leipzig$Richness_Random),
           label = "Random", color = "red", hjust = 1) +
  annotate("text", x = max(fits_Leipzig$Plant_sampled), y = max(fits_Leipzig$Richness_Focal),
           label = "Focal", color = "blue", hjust = 1) +
  theme_bw()


plot_Halle / plot_Jena / plot_Leipzig
