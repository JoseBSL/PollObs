############################################################ #
#Model FULL SEASON:
############################################################ #
library(ggplot2)
library(glmmTMB)
library(DHARMa)
library(ggeffects)
library(performance)
library(stringr)

full_season_data = readRDS("Data/Working_files/full_season_data.rds")


model1 = glmmTMB(VisitRate ~  log_flower_z * 
                   log_poll_z +
                   Botanical_garden +
                   (1 | Pair),
                 family = Gamma(link = "log"),
                 data = full_season_data)

summary(model1)
simulationOutput <- simulateResiduals(fittedModel = model1)
plot(simulationOutput)
r2(model1)


# 1) Full model marginal R²
r2_full <- r2_nakagawa(model1)$R2_marginal

# 2) Reduced models (respect hierarchy)
m_no_inter  <- update(model1, . ~ . - log_flower_z:log_poll_z)

# If you drop a main effect, drop the interaction too (hierarchy)
m_no_flower <- update(model1, . ~ . - log_flower_z - log_flower_z:log_poll_z)
m_no_poll   <- update(model1, . ~ . - log_poll_z   - log_flower_z:log_poll_z)

m_no_garden <- update(model1, . ~ . - Botanical_garden)

# 3) Extract marginal R² and compute ΔR²
r2_table <- tibble(
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
  ) %>%
  arrange(desc(Delta_R2))

r2_table_std <- r2_table %>%
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

r2_full_season = r2_table_std %>%
  filter(Term %in% c("Floral abundance", "Pollinator abundance"))


saveRDS(r2_full_season, "Data/Working_files/r2_full_season.rds")


ggplot(r2_full_season,
       aes(x = reorder(Term, Delta_R2),
           y = Delta_R2)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    x = "Variable removed",
    y = "Change in marginal R²",
    title = "Relative importance of predictors"
  ) +
  theme_minimal()



# ============================================================
# Faceted interaction plot by Botanical_garden
# RAW axes + RAW legend values
# ============================================================

# 1) Choose pollinator abundance levels (RAW scale)
poll_levels_raw <- as.numeric(
  quantile(full_season_data$Pollinator_abundance,
           probs = c(0.5, 0.7, 0.895),
           na.rm = TRUE)
)

# 2) Convert RAW levels to model scale (log_poll_z)
poll_center <- attr(scale(full_season_data$log_poll), "scaled:center")
poll_scale  <- attr(scale(full_season_data$log_poll), "scaled:scale")

poll_levels_z <- (log1p(poll_levels_raw) - poll_center) / poll_scale

# 3) Predict interaction at those levels, BY GARDEN
eff_interaction <- ggpredict(
  model1,
  terms = c(
    "log_flower_z",
    paste0("log_poll_z [", paste(round(poll_levels_z, 3), collapse = ", "), "]"),
    "Botanical_garden"
  )
)

eff_interaction <- as.data.frame(eff_interaction)

# 4) Back-transform floral abundance to RAW scale
eff_interaction$Floral_abundance_raw <- exp(eff_interaction$x) - 1

# 5) Use RAW pollinator values directly in legend
eff_interaction$group <- factor(eff_interaction$group, levels = unique(eff_interaction$group))

eff_interaction$poll_group_label <- factor(
  eff_interaction$group,
  levels = levels(eff_interaction$group),
  labels = round(poll_levels_raw, 0)
)

# 6) Plot (facet by garden)
ggplot(eff_interaction,
       aes(x = Floral_abundance_raw, y = predicted,
           color = poll_group_label, fill = poll_group_label)) +
  geom_line(linewidth = 1.2) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
              alpha = 0.15, color = NA) +
  scale_color_viridis_d(option = "D") +
  scale_fill_viridis_d(option = "D") +
  facet_wrap(~ facet) +
  labs(
    x = "Floral abundance",
    y = "Predicted visitation rate",
    color = "Pollinator abundance",
    fill  = "Pollinator abundance"
  ) +
  theme_minimal()
