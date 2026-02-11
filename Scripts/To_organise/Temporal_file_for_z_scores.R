
#Temporal file, just doing trials here

#Plot z-score distribution
colnames(z_sc_data_interactions_genus_final)
z_sc_data_interactions_genus_final %>% 
ggplot(aes(x = z_score)) +
geom_histogram(aes(y = ..density.., fill = Type), 
               bins = 100, alpha = 0.5, color = "black", position = "stack") +
theme_bw() +
coord_cartesian(expand = FALSE) +
#geom_vline(xintercept = -abs(critical_value), linetype = "longdash", colour = "red") +
#geom_vline(xintercept = abs(critical_value), linetype = "longdash", colour = "red") +
ylab("Density") +
xlab("Z scores")  +
scale_fill_manual(name = "Observed against null",
                  limits = c("Under-represented", "Normal", "Over-represented"),
                  labels = c("Under-represented", "Normal", "Over-represented"),
                  values = c("Under-represented" = "coral2", 
                             "Normal" = "palegreen3", 
                             "Over-represented" = "cyan3"))


#Check association between vars
trial = z_sc_data_interactions_genus_final %>% 
filter(is.finite(z_score))
cor.test(trial$Total_interactions,trial$z_score,
         na.rm=T)

#Explore graphical pattern
trial %>% 
ggplot(aes(z_score, Total_interactions )) +
geom_point() +
scale_y_log10()

#Think how to visualize this
check = z_sc_data_interactions_genus_final %>% 
filter(Type == "Over-represented") %>% 
filter(!Pollinator_genus=="Apis") %>% 
  mutate(Pair = paste0(Plant_genus,"_",Pollinator_genus)) %>% 
  group_by(Pair,Type) %>% 
  count() 

#Maybe ignore temporal component now!
  