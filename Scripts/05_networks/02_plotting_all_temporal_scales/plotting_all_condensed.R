

panel_trait_swapped + panel_pheno_swapped + panel_abundance_swapped


library(patchwork)

final_plot <- panel_abundance_swapped +
  panel_trait_swapped +
  panel_pheno_swapped +

  
  plot_layout(
    ncol = 3,
    guides = "collect"   # <- THIS is the key
  ) &
  
  theme(
    legend.position = "right",   # or "bottom"
    legend.box = "vertical"
  )

final_plot



library(patchwork)

final_plot_col <-   panel_abundance_swapped /
panel_trait_swapped /
  panel_pheno_swapped +
  
  plot_layout(
    ncol = 1,
    guides = "collect"   # collect identical legends
  ) &
  
  theme(
    legend.position = "right",   # or "bottom"
    legend.box = "vertical"
  )

final_plot_col
