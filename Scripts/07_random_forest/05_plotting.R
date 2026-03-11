library(dplyr)
library(ggplot2)
library(patchwork)


#Load data
shap_importance = readRDS("Data/Working_files/shap_importance.rds")
rf_stack = readRDS("Data/Working_files/rf_stack.rds")


base_theme = theme_classic(base_size = 14) +
  theme(
    axis.text = element_text(color = "black"),
    axis.ticks = element_line(color = "black"),
    axis.line = element_line(color = "black", linewidth = 0.8)
  )

# recode names
shap_importance <- shap_importance %>% 
  mutate(variable = recode(
    variable,
    Total_pollinator_abundance = "Pollinator abundance",
    Floral_abundance = "Flower abundance",
    T_gauss = "Trait matching",
    Flower_width = "Flower width",
    Overlap_days = "Phenology overlap"
  ))


cols <- c(
  "Pollinator abundance" = "#E64B35",
  "Flower abundance" = "#F39B7F",
  "Trait matching" = "#00A087",
  "Flower width" = "#4DBBD5",
  "Phenology overlap" = "grey49"
)


var_levels = shap_importance %>%
  arrange(desc(mean_abs_shap)) %>% 
  pull(variable)

shap_importance <- shap_importance %>%
  mutate(variable = factor(variable, levels = var_levels))

p1 <- ggplot(shap_importance,
             aes(y = factor(variable, levels = rev(var_levels)),
                 x = mean_abs_shap,
                 fill = variable)) +
  geom_col(orientation = "y", width = 0.8,color = "white", linewidth = 0.6) +
  
  scale_y_discrete(position = "right") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_fill_viridis_d(option = "magma", direction = -1, begin = 0.05, end=0.85) +
  labs(x = "Mean SHAP value", y = NULL) +
  ggtitle("Global importance") +
  base_theme +
  theme(
    axis.text.y.left = element_blank(),
    axis.ticks.y.left = element_blank(),
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "none",
    axis.text.y = element_text(face = "bold")
  )

# order variables by SHAP importance
var_levels <- shap_importance %>%
  arrange(desc(mean_abs_shap)) %>%   # smallest -> largest
  pull(variable)
# use same factor order in both datasets
shap_importance <- shap_importance %>%
  mutate(variable = factor(variable, levels = var_levels))

rf_stack <- rf_stack %>%
  mutate(variable = factor(variable, levels = var_levels))

p2 <- ggplot(rf_stack,
             aes(x = Week_center,
                 y = importance_rel,
                 fill = variable)) +
  geom_area(alpha = 0.95) +
  scale_fill_viridis_d(option = "magma", direction = -1, begin = 0.05, end=0.85) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1.05)) +
  labs(x = "Week", y = "Relative importance (SHAP)") +
  base_theme +
  theme(legend.position = "none")


p2 <- p2 + theme(plot.margin = margin(5.5, 0, 5.5, 5.5))
p1 <- p1 + theme(plot.margin = margin(5.5, 5.5, 5.5, 0))

p1 =plot_spacer() / p1 & plot_layout(heights = c(0.2, 1.45))

p2 + p1 + plot_layout(widths = c(1, 1))


