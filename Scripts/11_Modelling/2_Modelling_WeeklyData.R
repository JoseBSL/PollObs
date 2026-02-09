library(dplyr)
library(tibble)
library(stringr)
library(ggplot2)
library(glmmTMB)
library(DHARMa)
library(ggeffects)
library(performance)

weekly_data <- readRDS("Data/Working_files/weekly_data.rds")

# ---- Helper: standardize term labels
standardize_terms <- function(r2_table) {
  r2_table %>%
    mutate(
      Term = case_when(
        str_detect(Dropped_term, "^Floral")      ~ "Floral abundance",
        str_detect(Dropped_term, "^Pollinator")  ~ "Pollinator abundance",
        str_detect(Dropped_term, "^Botanical")   ~ "Botanical garden",
        str_detect(Dropped_term, "^Interaction") ~ "Interaction",
        TRUE ~ Dropped_term
      ),
      Term = factor(Term, levels = c(
        "Pollinator abundance",
        "Floral abundance",
        "Botanical garden",
        "Interaction"
      ))
    )
}

# ---- Helper: run everything for one week
run_week <- function(dat, week_value,
                     poll_probs = c(0.5, 0.7, 0.895),
                     keep_terms = c("Floral abundance", "Pollinator abundance"),
                     min_n = 50,
                     min_pairs = 20) {
  
  d <- dat %>% filter(Week == week_value)
  
  # Skip tiny weeks (prevents nonsense / convergence issues)
  if (nrow(d) < min_n || dplyr::n_distinct(d$Pair) < min_pairs) {
    return(NULL)
  }
  
  m <- glmmTMB(
    VisitRate ~ log_flower_z * log_poll_z + Botanical_garden + (1 | Pair),
    family = Gamma(link = "log"),
    data = d
  )
  
  r2_full <- r2_nakagawa(m)$R2_marginal
  
  m_no_inter  <- update(m, . ~ . - log_flower_z:log_poll_z)
  m_no_flower <- update(m, . ~ . - log_flower_z - log_flower_z:log_poll_z)
  m_no_poll   <- update(m, . ~ . - log_poll_z   - log_flower_z:log_poll_z)
  m_no_garden <- update(m, . ~ . - Botanical_garden)
  
  r2_table <- tibble(
    Week = week_value,
    Dropped_term = c(
      "Interaction (log_flower_z × log_poll_z)",
      "Floral abundance (log_flower_z + interaction)",
      "Pollinator abundance (log_poll_z + interaction)",
      "Botanical_garden"
    ),
    Reduced_model_R2 = c(
      r2_nakagawa(m_no_inter)$R2_marginal,
      r2_nakagawa(m_no_flower)$R2_marginal,
      r2_nakagawa(m_no_poll)$R2_marginal,
      r2_nakagawa(m_no_garden)$R2_marginal
    )
  ) %>%
    mutate(
      Full_model_R2 = r2_full,
      Delta_R2 = Full_model_R2 - Reduced_model_R2
    )
  
  r2_table_std <- standardize_terms(r2_table)
  
  # keep only biological predictors if desired
  r2_table_plot <- r2_table_std %>%
    filter(Term %in% keep_terms) %>%
    mutate(Term = factor(Term, levels = keep_terms))
  
  list(
    week = week_value,
    model = m,
    r2_table_full = r2_table_std,
    r2_table_plot = r2_table_plot
  )
}

# ---- Run for all weeks present (sorted)
weeks_to_run <- weekly_data %>%
  distinct(Week) %>%
  arrange(Week) %>%
  pull(Week)

results_weekly <- lapply(weeks_to_run, function(w) run_week(weekly_data, w))
results_weekly <- Filter(Negate(is.null), results_weekly)

# ---- Combine tables across weeks
r2_weekly <- bind_rows(lapply(results_weekly, `[[`, "r2_table_full"))

# Optionally keep only bio predictors
r2_weekly <- r2_weekly %>%
  filter(Term %in% c("Floral abundance", "Pollinator abundance"))


# ensure Week is ordered
r2_weekly <- r2_weekly %>%
  mutate(
    Week = as.integer(Week),
    Term = factor(Term, levels = c("Floral abundance", "Pollinator abundance"))
  )

ggplot(r2_weekly,
       aes(x = Week, y = Term, fill = Delta_R2)) +
  geom_tile() +
  scale_fill_viridis_c() +
  labs(
    x = "Week",
    y = "Predictor",
    fill = "ΔR²",
    title = "Relative importance of predictors across weeks"
  ) +
  theme_minimal()

r2_weekly <- r2_weekly %>%
  mutate(
    Week = factor(Week, levels = sort(unique(Week)), ordered = TRUE)
  )

saveRDS(r2_weekly, "Data/Working_files/r2_weekly.rds")

