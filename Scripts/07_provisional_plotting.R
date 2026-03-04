library(ComplexHeatmap)
library(circlize)
library(grid)
library(dplyr)
library(tidyr)
library(tibble)

# -----------------------------
# Inputs assumed:
#   SES      : matrix (Plant_family x Pollinator_family) of standardized effect sizes
#   pval_df  : data.frame with Plant_family, Pollinator_family, p_two (or p_fdr)
# -----------------------------

alpha <- 0.05
p_col <- "p_two"   # change to "p_fdr" if you computed that

# 1) Build significance matrix aligned to SES
sig_mat <- pval_df %>%
  transmute(
    Plant_family,
    Pollinator_family,
    Sig = !is.na(.data[[p_col]]) & (.data[[p_col]] < alpha)
  ) %>%
  pivot_wider(names_from = Pollinator_family, values_from = Sig, values_fill = FALSE) %>%
  column_to_rownames("Plant_family") %>%
  as.matrix()

# Align to SES (important!)
sig_mat <- sig_mat[rownames(SES), colnames(SES)]

# 2) Optional: cap SES for better contrast
cap <- 6
SES_plot <- SES
SES_plot[SES_plot >  cap] <-  cap
SES_plot[SES_plot < -cap] <- -cap

# 3) Diverging colour function (your softened endpoints)
col_fun <- colorRamp2(
  c(-cap, 0, cap),
  c("#C65DAE", "white", "#FDB863")
)

# 4) Heatmap
ht <- Heatmap(
  SES_plot,
  col = col_fun,
  cluster_rows = FALSE,
  cluster_columns = FALSE,
  column_names_rot = 45,
  column_names_gp = gpar(fontsize = 8),
  row_names_gp = gpar(fontsize = 8),
  rect_gp = gpar(col = "black"),
  na_col = "grey90",
  name = "SES",
  column_title = "Pollinator preference (phenology-constrained null)",
  column_title_gp = gpar(fontsize = 16, fontface = "bold"),
  heatmap_legend_param = list(
    title = "SES",
    at = c(-cap, 0, cap),
    labels = c(paste0("-", cap, " (Avoidance)"), "0", paste0("+", cap, " (Association)"))
  ),
  cell_fun = function(j, i, x, y, width, height, fill) {
    if (isTRUE(sig_mat[i, j])) {
      grid.text("*", x = x, y = y, gp = gpar(col = "black", fontsize = 10))
    }
  }
)

draw(ht, padding = unit(c(10, 15, 10, 15), "mm"))
