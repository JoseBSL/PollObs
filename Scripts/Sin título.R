# ======================================================
# Pollinator preference exploration at family level
# ======================================================

# Load libraries
library(bipartite)
library(dplyr)
library(tidyr)
library(purrr)
library(tibble)
library(magrittr)
library(pheatmap)

# ======================================================
# Load data
raw_data = readRDS("Data/Working_files/interaction_data.rds")
phenobs_spp = readRDS("Data/Working_files/phenobs_spp.rds")

# ======================================================
# Prepare interaction data
poll_order = c("Hymenoptera", "Diptera")

int_data_clean = raw_data %>%
  filter(Pollinator_order %in% poll_order) %>%
  select(Plant_genus, Plant, Pollinator_family, Pollinator_accepted_name, Botanical_garden) %>%
  drop_na()

# Split phenobs and non-phenobs
int_data_phenobs = int_data_clean %>% filter(Plant %in% phenobs_spp)
int_data_nonphenobs = int_data_clean %>% filter(!Plant %in% phenobs_spp)

# Keep only families present in phenobs
phenobs_fam_vector = int_data_phenobs %>% distinct(Plant_genus) %>% pull()

int_data_combined = bind_rows(
  int_data_phenobs,
  int_data_nonphenobs %>% filter(Plant_genus %in% phenobs_fam_vector)
)

# Remove plant families with < 15 interactions
families_to_keep = int_data_combined %>%
  count(Plant_genus) %>%
  filter(n >= 15) %>%
  pull(Plant_genus)

int_data_combined = int_data_combined %>%
  filter(Plant_genus %in% families_to_keep)

# ======================================================
# Create preference table
pref.table = int_data_combined %>%
  count(Pollinator_family, Plant_genus) %>%
  pivot_wider(names_from = Plant_genus, values_from = n, values_fill = 0) %>%
  column_to_rownames("Pollinator_family")

# ======================================================
# Run null models
set.seed(1)
n.sim = 1000
n.mod = nullmodel(pref.table, N = n.sim, method = "r2dtable")

for (i in 1:n.sim) {
  rownames(n.mod[[i]]) <- rownames(pref.table)
  colnames(n.mod[[i]]) <- colnames(pref.table)
}

# ======================================================
# Calculate expected means & SDs
expected.means = reduce(n.mod, `+`) / n.sim

expected.sds = n.mod %>%
  map(~ (.x - expected.means)^2) %>%
  reduce(`+`) %>%
  divide_by(n.sim) %>%
  sqrt()

# Compute SES
SES = (as.matrix(pref.table) - expected.means) / expected.sds

# ======================================================
# Compute p-values (fully tidyverse)
obs_df = pref.table %>%
  as.data.frame() %>%
  rownames_to_column("Pollinator_family") %>%
  pivot_longer(-Pollinator_family, names_to = "Plant_genus", values_to = "Observed")

# Convert null matrices into tidy tibble with SimID
sim_df = map2_dfr(n.mod, seq_along(n.mod), ~ as.data.frame(.x) %>%
                    rownames_to_column("Pollinator_family") %>%
                    pivot_longer(-Pollinator_family, names_to = "Plant_genus", values_to = "Simulated") %>%
                    mutate(SimID = .y))

# Compute p-values in tidyverse
pval_df = sim_df %>%
  inner_join(obs_df, by = c("Pollinator_family", "Plant_genus")) %>%
  group_by(Pollinator_family, Plant_genus) %>%
  summarise(p_value = (sum(Simulated >= Observed) + 1) / (n.sim + 1), .groups = "drop")

# ======================================================
# Merge SES and p-values
ses_df = as.data.frame(SES) %>%
  rownames_to_column("Pollinator_family") %>%
  pivot_longer(-Pollinator_family, names_to = "Plant_genus", values_to = "SES")

results = left_join(ses_df, pval_df, by = c("Pollinator_family", "Plant_genus")) %>%
  arrange(p_value)

# ======================================================
# Plot heatmap
pheatmap(SES,
         cluster_rows = TRUE,
         cluster_cols = TRUE,
         display_numbers = round(SES, 1),
         main = "Pollinator Preference SES (Observed vs Null Expectation)")

# Check percentage of significant z-scores (p<0.05)
significant_ses = ses_df %>%
  mutate(Significant = abs(SES) >= 1.96) %>%
  filter(Significant)
# Check percentage
nrow(significant_ses)/nrow(ses_df) *100
