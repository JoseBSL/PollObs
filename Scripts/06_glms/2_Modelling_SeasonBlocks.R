#############################################################
# SEASON BLOCKS: run models + importance + interaction plots
# for each Season (Early/Mid/Late)
#############################################################
library(dplyr)
library(tibble)
library(stringr)
library(ggplot2)
library(glmmTMB)
library(DHARMa)
library(ggeffects)
library(performance)

season_block_data <- readRDS("Data/Working_files/season_blocks_data.rds")

# ---- Helper: standardize term labels (same names every time)
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

# ---- Helper: run everything for one season
run_season <- function(dat, season_name,
                       poll_probs = c(0.5, 0.7, 0.895),
                       keep_terms = c("Floral abundance", "Pollinator abundance")) {
  
  d <- dat %>% filter(Season == season_name)
  
  # ---- Fit model
  m <- glmmTMB(
    VisitRate ~ log_flower_z * log_poll_z + Botanical_garden + (1 | Pair),
    family = Gamma(link = "log"),
    data = d
  )
  
  # ---- Diagnostics objects (you can print/plot outside if you want)
  sim <- simulateResiduals(m)
  
  # ---- R2
  r2_full <- r2_nakagawa(m)$R2_marginal
  
  # ---- Reduced models (hierarchy respected)
  m_no_inter  <- update(m, . ~ . - log_flower_z:log_poll_z)
  m_no_flower <- update(m, . ~ . - log_flower_z - log_flower_z:log_poll_z)
  m_no_poll   <- update(m, . ~ . - log_poll_z   - log_flower_z:log_poll_z)
  m_no_garden <- update(m, . ~ . - Botanical_garden)
  
  r2_table <- tibble(
    Season = season_name,
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
  
  r2_table_plot <- r2_table_std %>%
    filter(Term %in% keep_terms) %>%
    mutate(Term = factor(Term, levels = keep_terms))
  
  p_importance <- ggplot(r2_table_plot,
                         aes(x = reorder(Term, Delta_R2), y = Delta_R2)) +
    geom_col(fill = "steelblue") +
    coord_flip() +
    labs(
      x = "Predictor removed",
      y = "Change in marginal R²",
      title = paste("Relative importance (Season:", season_name, ")")
    ) +
    theme_minimal()
  
  # ---- Interaction plot: choose pollinator levels on RAW scale (from THIS season)
  poll_levels_raw <- as.numeric(
    quantile(d$Pollinator_abundance, probs = poll_probs, na.rm = TRUE)
  )
  
  # Convert raw poll levels -> z-scale used in model
  # IMPORTANT: use the *same* center/scale used to create log_poll_z in this season dataset
  poll_center <- attr(scale(d$log_poll), "scaled:center")
  poll_scale  <- attr(scale(d$log_poll), "scaled:scale")
  poll_levels_z <- (log1p(poll_levels_raw) - poll_center) / poll_scale
  
  eff <- ggpredict(
    m,
    terms = c(
      "log_flower_z",
      paste0("log_poll_z [", paste(round(poll_levels_z, 3), collapse = ", "), "]"),
      "Botanical_garden"
    )
  ) %>% as.data.frame()
  
  # ---- Back-transform flowers correctly: log_flower_z -> log_flower -> raw flowers
  # eff$x is log_flower_z (standardized), so unscale using d$log_flower's center/scale
  flower_center <- attr(scale(d$log_flower), "scaled:center")
  flower_scale  <- attr(scale(d$log_flower), "scaled:scale")
  log_flower_unscaled <- eff$x * flower_scale + flower_center
  eff$Floral_abundance_raw <- expm1(log_flower_unscaled)
  
  # Legend labels = RAW poll values
  eff$group <- factor(eff$group, levels = unique(eff$group))
  eff$poll_group_label <- factor(
    eff$group,
    levels = levels(eff$group),
    labels = round(poll_levels_raw, 0)
  )
  
  p_interaction <- ggplot(
    eff,
    aes(x = Floral_abundance_raw, y = predicted,
        color = poll_group_label, fill = poll_group_label)
  ) +
    geom_line(linewidth = 1.2) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
                alpha = 0.15, color = NA) +
    scale_color_viridis_d(option = "D") +
    scale_fill_viridis_d(option = "D") +
    facet_wrap(~ facet) +
    labs(
      title = paste("Predicted visitation (Season:", season_name, ")"),
      x = "Floral abundance (raw)",
      y = "Predicted visitation rate",
      color = "Pollinator abundance",
      fill  = "Pollinator abundance"
    ) +
    theme_minimal()
  
  list(
    season = season_name,
    data = d,
    model = m,
    sim = sim,
    r2_table_full = r2_table_std,
    r2_table_plot = r2_table_plot,
    plot_importance = p_importance,
    plot_interaction = p_interaction,
    eff_interaction = eff
  )
}

# ---- Run for all seasons you care about
seasons_to_run <- c("Early", "Mid", "Late")

results <- lapply(seasons_to_run, function(s) run_season(season_block_data, s))
names(results) <- seasons_to_run

# ---- Combine tables across seasons (useful for export)
r2_season_blocks <- bind_rows(lapply(results, `[[`, "r2_table_full"))

# Print combined importance table
r2_season_blocks


ggplot(r2_season_blocks,
       aes(x = Term,
           y = Delta_R2,
           fill = Season)) +
  geom_col(position = "dodge") +
  labs(
    x = "Predictor removed",
    y = "Change in marginal R²",
    title = "Relative importance across seasons"
  ) +
  theme_minimal()

r2_season_blocks <- r2_season_blocks %>% 
  filter(Term %in% c("Floral abundance", "Pollinator abundance"))


saveRDS(r2_season_blocks, "Data/Working_files/r2_season_blocks.rds")

ggplot(r2_season_blocks,
       aes(x = Season,
           y = Term,
           fill = Delta_R2)) +
  geom_tile() +
  scale_fill_viridis_c() +
  labs(
    x = NULL,
    y = "Predictor",
    fill = "ΔR²",
    title = "Relative importance of predictors across seasons"
  ) +
  theme_minimal()
