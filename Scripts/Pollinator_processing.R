

#Apoidea sp
matched_gbif1 = matched_gbif1 %>% 
mutate(Rank = case_when(
  Fixed_name == "Apoidea sp" ~ "SUPERFAMILY",
  T ~ Rank)) %>% 
mutate(Status = case_when(
  Fixed_name == "Apoidea sp" ~ "ACCEPTED",
  T ~ Status)) %>% 
mutate(Matchtype = case_when(
  Fixed_name == "Apoidea sp" ~ "EXACT",
  T ~ Matchtype)) %>% 
mutate(Scientific_name = case_when(
  Fixed_name == "Apoidea sp" ~ "Apoidea",
  T ~ Scientific_name)) %>% 
mutate(Canonical_name = case_when(
  Fixed_name == "Apoidea sp" ~ "Apoidea",
  T ~ Canonical_name)) %>% 
mutate(Accepted_name = case_when(
  Fixed_name == "Apoidea sp" ~ "Apoidea",
  T ~ Accepted_name)) %>% 
mutate(Kingdom = case_when(
  Fixed_name == "Apoidea sp" ~ "Animalia",
  T ~ Kingdom)) %>% 
mutate(Phylum = case_when(
  Fixed_name == "Apoidea sp" ~ "Arthropoda",
  T ~ Phylum)) %>% 
mutate(Order = case_when(
  Fixed_name == "Apoidea sp" ~ "Hymenoptera",
  T ~ Order))

#Pieris sp
matched_gbif1 = matched_gbif1 %>% 
mutate(Matchtype = case_when(
  Fixed_name == "Pieris sp" ~ "EXACT",
  T ~ Matchtype))

#Bombylius sp
matched_gbif1 = matched_gbif1 %>% 
mutate(Matchtype = case_when(
  Fixed_name == "Bombylius sp" ~ "EXACT",
  T ~ Matchtype))

#Anthophora sp
matched_gbif1 = matched_gbif1 %>% 
mutate(Rank = case_when(
  Fixed_name == "Anthophora sp" ~ "GENUS",
  T ~ Rank)) %>% 
mutate(Status = case_when(
  Fixed_name == "Anthophora sp" ~ "ACCEPTED",
  T ~ Status)) %>% 
mutate(Matchtype = case_when(
  Fixed_name == "Anthophora sp" ~ "EXACT",
  T ~ Matchtype)) %>% 
mutate(Scientific_name = case_when(
  Fixed_name == "Anthophora sp" ~ "Anthophora",
  T ~ Scientific_name)) %>% 
mutate(Canonical_name = case_when(
  Fixed_name == "Anthophora sp" ~ "Anthophora",
  T ~ Canonical_name)) %>% 
mutate(Accepted_name = case_when(
  Fixed_name == "Anthophora sp" ~ "Anthophora",
  T ~ Accepted_name)) %>% 
mutate(Kingdom = case_when(
  Fixed_name == "Anthophora sp" ~ "Animalia",
  T ~ Kingdom)) %>% 
mutate(Phylum = case_when(
  Fixed_name == "Anthophora sp" ~ "Arthropoda",
  T ~ Phylum)) %>% 
mutate(Order = case_when(
  Fixed_name == "Anthophora sp" ~ "Hymenoptera",
  T ~ Order)) %>% 
mutate(Family = case_when(
  Fixed_name == "Anthophora sp" ~ "Apidae",
  T ~ Family)) %>% 
mutate(Genus = case_when(
  Fixed_name == "Anthophora sp" ~ "Anthophora",
  T ~ Genus))

#Bombus sp
matched_gbif1 = matched_gbif1 %>% 
mutate(Matchtype = case_when(
  Fixed_name == "Bombus sp" ~ "EXACT",
  T ~ Matchtype))

#Lasioglossum sp
matched_gbif1 = matched_gbif1 %>% 
mutate(Matchtype = case_when(
  Fixed_name == "Lasioglossum sp" ~ "EXACT",
  T ~ Matchtype))

#Megachile sp
matched_gbif1 = matched_gbif1 %>% 
mutate(Matchtype = case_when(
  Fixed_name == "Megachile sp" ~ "EXACT",
  T ~ Matchtype))

#Anthribidae sp
matched_gbif1 = matched_gbif1 %>% 
mutate(Matchtype = case_when(
  Fixed_name == "Anthribidae sp" ~ "EXACT",
  T ~ Matchtype))

#Formicidae sp
matched_gbif1 = matched_gbif1 %>% 
mutate(Matchtype = case_when(
  Fixed_name == "Formicidae sp" ~ "EXACT",
  T ~ Matchtype))

#Volucella sp
matched_gbif1 = matched_gbif1 %>% 
mutate(Rank = case_when(
  Fixed_name == "Volucella sp" ~ "GENUS",
  T ~ Rank)) %>% 
mutate(Status = case_when(
  Fixed_name == "Volucella sp" ~ "ACCEPTED",
  T ~ Status)) %>% 
mutate(Matchtype = case_when(
  Fixed_name == "Volucella sp" ~ "EXACT",
  T ~ Matchtype)) %>% 
mutate(Scientific_name = case_when(
  Fixed_name == "Volucella sp" ~ "Volucella",
  T ~ Scientific_name)) %>% 
mutate(Canonical_name = case_when(
  Fixed_name == "Volucella sp" ~ "Volucella",
  T ~ Canonical_name)) %>% 
mutate(Accepted_name = case_when(
  Fixed_name == "Volucella sp" ~ "Volucella",
  T ~ Accepted_name)) %>% 
mutate(Kingdom = case_when(
  Fixed_name == "Volucella sp" ~ "Animalia",
  T ~ Kingdom)) %>% 
mutate(Phylum = case_when(
  Fixed_name == "Volucella sp" ~ "Arthropoda",
  T ~ Phylum)) %>% 
mutate(Order = case_when(
  Fixed_name == "Volucella sp" ~ "Diptera",
  T ~ Order)) %>% 
mutate(Family = case_when(
  Fixed_name == "Volucella sp" ~ "Syrphidae",
  T ~ Family)) %>% 
mutate(Genus = case_when(
  Fixed_name == "Volucella sp" ~ "Volucella",
  T ~ Genus))

#Anthidium sp
matched_gbif1 = matched_gbif1 %>% 
mutate(Matchtype = case_when(
  Fixed_name == "Anthidium sp" ~ "EXACT",
  T ~ Matchtype))

#Chelostoma sp
matched_gbif1 = matched_gbif1 %>% 
mutate(Matchtype = case_when(
  Fixed_name == "Chelostoma sp" ~ "EXACT",
  T ~ Matchtype))

#Anthrenus sp
matched_gbif1 = matched_gbif1 %>% 
mutate(Matchtype = case_when(
  Fixed_name == "Anthrenus sp" ~ "EXACT",
  T ~ Matchtype))

#Symmorphus sp
matched_gbif1 = matched_gbif1 %>% 
mutate(Matchtype = case_when(
  Fixed_name == "Symmorphus sp" ~ "EXACT",
  T ~ Matchtype))
