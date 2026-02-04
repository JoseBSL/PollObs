library(gtExtras)
library(dplyr)
library(tidyr)
library(gt)

plant_traits = readRDS("Data/Working_files/plant_trait_data.rds")
poll_traits  = readRDS("Data/Trait_data/Processed/PollTraits_all.rds")

# --- Plant traits: keep numeric columns only (you already do this)
plant_traits = plant_traits %>%
  select(!c(Species, Family_all, Species_all, Genus_all,
            Mean_interactions_total, Mean_interaction_frequency,
            Mean_degree, Mean_normalised_degree))

new_names = c(
  "Autonomous_selfing_level_fruit_set" = "Autonomous selfing",
  "Flowers_per_plant"                 = "Flowers per plant",
  "Corolla_diameter_mean"             = "Corolla diameter",
  "Style_length"                      = "Style length",
  "Ovule_number"                      = "Ovule number",
  "Plant_height_mean_m"               = "Plant height",
  "Mean_flowering_length"             = "Flowering length",
  "Mean_flower_lifespan"              = "Flower lifespan",
  "Mean_nectar_volume"                = "Nectar volume",
  "Seed_mass"                         = "Seed mass",
  "Pollen_size"                       = "Pollen size",
  "Pollen_per_flower"                 = "Pollen per flower"
)
names(plant_traits) <- new_names[names(plant_traits)]

# --- Pollinator traits: drop non-numeric id columns, keep trait columns
poll_traits_num = poll_traits %>%
  select(-Pollinator_accepted_name, -Pollinator_genus)

# (optional) nicer labels for pollinator trait names
poll_new_names = c(
  "IT_mean"               = "IT",
  "Body_length_mean"      = "Body length",
  "Proboscis_length_mean" = "Proboscis length"
)
names(poll_traits_num) <- poll_new_names[names(poll_traits_num)]

# --- helper to build the summary tibble in the same shape for both
make_summary <- function(df, group_name) {
  df %>%
    pivot_longer(cols = everything(), names_to = "Traits", values_to = "Values") %>%
    group_by(Traits) %>%
    summarise(
      Mean   = round(mean(Values, na.rm = TRUE), 2),
      #Median = round(median(Values, na.rm = TRUE), 2),
      SD     = round(sd(Values, na.rm = TRUE), 2),
      data   = list(Values),
      .groups = "drop"
    ) %>%
    mutate(Group = group_name)
}

plant_sum <- make_summary(plant_traits,   "a) Plant traits")
poll_sum  <- make_summary(poll_traits_num,"b) Pollinator traits")

# stack pollinators at the bottom
all_sum <- bind_rows(plant_sum, poll_sum)

table_kable <- all_sum %>%
  
  # Keep only columns to display
  select(Group, Traits, Mean, SD) %>%
  
  # Format numeric values
  mutate(
    Mean = round(Mean, 2),
    SD   = round(SD, 2)
  ) %>%
  
  arrange(Group)   # IMPORTANT for group_rows()

saveRDS(table_kable, "Data/Working_files/Table_traits.rds")

library(kableExtra)
table_kable %>%
  select(-Group) %>%     # Group used only for row grouping
  kbl(
    caption = "Summary of plant and pollinator traits",
    align = c("l", "c", "c"),
    booktabs = TRUE
  ) %>%
  kable_styling(full_width = FALSE)


# --- build gt table (now has BOTH sections)
tab = all_sum %>%
  gt(groupname_col = "Group") %>%
  gt_plt_dist(
    data,
    type = "density",
    same_limit = FALSE,
    fill_color = "lightgrey",
    line_color = "black"
  ) %>%
  fmt_scientific(
    columns = c(Mean, SD),
    rows = (Group == "a) Plant traits" & Traits == "Pollen per flower"),
    decimals = 2
  ) %>%
  # base color for everything
  tab_style(
    style = cell_fill(color = "#B4EEB433"),
    locations = cells_body()
  ) |>
  # pollinators get a different color
  tab_style(
    style = cell_fill(color = "#FFDAB966"),
    locations = cells_body(rows = Group == "b) Pollinator traits")
  ) %>%
  tab_style(
    style = cell_text(weight = "bold", align = "center"),
    locations = cells_column_labels(everything())
  ) %>%
  cols_label(data = "Density")

tab

saveRDS(tab, "Data/Working_files/Trait_summary_table.rds")
