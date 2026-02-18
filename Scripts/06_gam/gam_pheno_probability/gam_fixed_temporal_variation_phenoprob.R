############################################################ #
# Base model, no temporal variation of abundances:
############################################################ #
# Load libraries
library(mgcv)
library(gratia)
library(dplyr)
library(ggplot2)
library(tidyr)
############################################################ #
# Load data 
weekly_data = readRDS("Data/Working_files/weekly_data_pheno_probability.rds")
colnames(weekly_data)
############################################################ #

dat <- weekly_data %>%
  arrange(Pair, Week) %>%
  mutate(AR.start = c(TRUE, Pair[-1] != Pair[-n()]))

m0_ar1 <- bam(
  Total_pair_interactions ~ 
    log_flower_z + log_poll_z +
    T_gauss_z + Pheno_probability_z +
    Botanical_garden +
    s(Week, Botanical_garden, bs="fs", k=8) +
    s(Pair, bs="re") +
    offset(log(Total_time_plant_week)),
  family = nb(theta = 2, link="log"),
  data = dat,
  method = "fREML",
  rho = 0.3,
  AR.start = dat$AR.start,
  discrete = TRUE
)

sim_res <- simulateResiduals(
  fittedModel = m0_ar1,
  n = 500
)

plot(sim_res)
testDispersion(sim_res)


library(dplyr)
library(ggplot2)

coef_tab <- as.data.frame(summary(m0_ar1)$p.table) %>%
  tibble::rownames_to_column("term")

# keep only the drivers you care about
drivers <- c("log_flower_z", "log_poll_z", "T_gauss_z", "Pheno_probability_z")
lab_map <- c(
  log_flower_z = "Flower abundance",
  log_poll_z   = "Pollinator abundance",
  T_gauss_z    = "Trait matching",
  Pheno_probability_z = "Phenology overlap prob."
)

imp <- coef_tab %>%
  filter(term %in% drivers) %>%
  mutate(
    Variable = lab_map[term],
    CI_low = Estimate - 1.96 * `Std. Error`,
    CI_high= Estimate + 1.96 * `Std. Error`,
    sig = `Pr(>|t|)` < 0.05,
    absE = abs(Estimate)
  ) %>%
  arrange(absE) %>%
  mutate(Variable = factor(Variable, levels = Variable))

ggplot(imp, aes(x = Variable, y = Estimate)) +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_pointrange(aes(ymin = CI_low, ymax = CI_high, shape = sig),
                  linewidth = 0.8) +
  coord_flip() +
  labs(
    x = NULL,
    y = "Standardized effect (link scale, ±95% CI)",
    title = "Relative importance of drivers (GAM, negative binomial)"
  ) +
  theme_bw(base_size = 12) +
  theme(legend.position = "none")


