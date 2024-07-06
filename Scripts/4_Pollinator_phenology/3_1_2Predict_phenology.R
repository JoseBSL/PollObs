#Same script as before but adjusting species according to
#their phenology (e.g., unimodal or bimodal not fully associated with voltinism)
#Load data
oc2 = readRDS("Data/Working_files/poll_occurrences_over_60_records.rds")
poll_phenology_info = readr::read_csv("Data/Working_files/filled_poll_phenology.csv")
#select cols of interest
poll_phenology_info = poll_phenology_info %>% 
select(Species, Phenology)
#Use this expected phenologies
#To adjust values of models in accordance to the 
#species phenology
oc2_informative = left_join(poll_phenology_info, oc2)
#Now adjust with a case_when
#Add k equal to 5 when unimodal
#Add k equal to 20 when bimodal 
#Keep uni-bimodal as it is
#Check levels
#Add default K and m values for each species
oc2_informative = oc2_informative %>% 
mutate(k_value = case_when(
   Phenology == "bimodal" ~ 20,
   Phenology == "unimodal" ~ 5,
   NA ~ TRUE))
#Now fix some particular cases that I have checked manually


sp = "Harmonia axyridis"
sp_model = gam(n_individuals ~ s(Doy, k = 5, m=1)
               
sp = "Pieris rapae"
sp_model = gam(n_individuals ~ s(Doy, k = 9, m=2),

sp = "Pieris napi"
sp_model = gam(n_individuals ~ s(Doy, k = 16, m=2),

sp = "Bombus lapidarius"
sp_model = gam(n_individuals ~ s(Doy, k = 5, m=2),

sp = "Xylocopa violacea"
sp_model = gam(n_individuals ~ s(Doy, k = 8, m=2),

sp = "Bombus campestris"
sp_model = gam(n_individuals ~ s(Doy, k = 5, m=2),

sp = "Anthomyia procellaris"
sp_model = gam(n_individuals ~ s(Doy, k = 5, m=2),

sp = "Timarcha goettingensis"
sp_model = gam(n_individuals ~ s(Doy, k = 4, m=2),

sp = "Meliscaeva auricollis"
sp_model = gam(n_individuals ~ s(Doy, k = 8, m=2),

sp = "Minettia longipennis"
sp_model = gam(n_individuals ~ s(Doy, k = 4, m=2),
mutate(Flying_period = if_else(Probability <= 0.35, "No", "Yes"))

sp = "Nomada zonata"
sp_model = gam(n_individuals ~ s(Doy, k = 5, m=1),
mutate(Flying_period = if_else(Probability <= 0.35, "No", "Yes"))

sp = "Eristalinus aeneus"
sp_model = gam(n_individuals ~ s(Doy, k = 4, m=2),
mutate(Flying_period = if_else(Probability <= 0.35, "No", "Yes"))

sp = "Bombus bohemicus"
sp_model = gam(n_individuals ~ s(Doy, k = 5, m=2),

             
               
