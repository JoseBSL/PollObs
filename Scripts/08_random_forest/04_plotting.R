library(dplyr)
library(ggplot2)

plot_dat <- rf_dat %>%
  filter(Total_pollinator_abundance > 0) %>%
  mutate(
    abundance_group = ntile(log10(Total_pollinator_abundance), 3),
    abundance_group = factor(
      abundance_group,
      labels = c("Low pollinator abundance",
                 "Medium pollinator abundance",
                 "High pollinator abundance")
    ),
    T_bin = cut(T_gauss, breaks = seq(0, 1, by = 0.1), include.lowest = TRUE)
  ) %>%
  group_by(abundance_group, T_bin) %>%
  summarise(
    T_mid = mean(T_gauss, na.rm = TRUE),
    median_visit = median(VisitRate, na.rm = TRUE),
    q25 = quantile(VisitRate, 0.25, na.rm = TRUE),
    q75 = quantile(VisitRate, 0.75, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) %>%
  filter(n >= 5)

ggplot(plot_dat, aes(x = T_mid, y = median_visit)) +
  geom_ribbon(aes(ymin = q25, ymax = q75), alpha = 0.2) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_y_log10() +
  facet_wrap(~ abundance_group, nrow = 1) +
  theme_classic(base_size = 14) +
  labs(
    x = "Trait matching (T_gauss)",
    y = "Median visit rate (log scale)"
  )