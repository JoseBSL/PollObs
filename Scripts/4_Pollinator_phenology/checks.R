


match("Meliscaeva auricollis", spp_order)


# Check fit of some models
species =  "Eucera nigrescens"
sp_data = oc2 %>% filter(Species == species)


sp_model = gam(n_individuals ~ s(Doy, k = 20, m=1),
               family = nb(link = "log"), data = sp_data, method = "REML")
qq.gam(sp_model, main = "QQ plot of residuals")
sp_data1 = oc3 %>% filter(Species == species)

#Looks good
#Create a new data frame with unique Doy values
unique_dates = tibble(Doy = seq(from = 1, to = 365, by = 1))

#Make predictions using the new data frame
predicted_values = predict(sp_model, unique_dates, type = "response")
#Assign the predicted values to the new data frame
unique_dates$Abundances = round(predicted_values)
#Normalize the predicted values to convert to probabilities
max_abundance = max(unique_dates$Abundances)
unique_dates = unique_dates %>%
mutate(Probability = Abundances / max_abundance)
#Add the species name to the data frame
unique_dates$Species = species
#Plot the predicted probabilities
ggplot(unique_dates, aes(x = Doy, y = Probability)) +
  geom_line(color = "blue") +
    labs(title = paste("Predicted Probabilities for", species, "Over Day of Year (Doy)"),
         x = "Day of Year (Doy)",
         y = "Predicted Probability") +
  theme_minimal() +
  geom_point(data = sp_data, aes(x = Doy, y = n_individuals/(max(n_individuals)))) +
  geom_vline(data = sp_data1, aes(xintercept = Doy), linetype = "dashed") 


