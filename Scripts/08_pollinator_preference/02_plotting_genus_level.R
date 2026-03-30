# ======================================================
# Heatmap of SES with:
#  - diverging gradient (avoidance -> neutral -> association)
#  - "*" overlay for significant cells (p < alpha)
#  - "×" overlay for phenology-impossible cells (never co-occurred in any block)
#  - row/col ordering by number of significant cells (optional)
#
# REQUIRED OBJECTS:
#   SES        : matrix (Plant_family x Pollinator_family)
#   pval_df    : data.frame with Plant_family, Pollinator_family, p_two (or p_fdr)
#   block_mats : list of garden-week matrices (Plant_family x Pollinator_family), padded & aligned
# ======================================================

library(ComplexHeatmap)
library(circlize)
library(grid)
library(dplyr)
library(tidyr)
library(tibble)
library(purrr)

# Load data
pval_df   <- readRDS("Data/Working_files/pval_df.rds")
SES       <- readRDS("Data/Working_files/SES.rds")
block_mats <- readRDS("Data/Working_files/block_mats.rds")

alpha <- 0.01
p_col <- "p_two"   # or "p_fdr" if you computed it

# -----------------------------
# 1) Build significance matrix aligned to SES
# -----------------------------
sig_mat = pval_df %>%
  transmute(
    Plant_family,
    Pollinator_family,
    Sig = !is.na(.data[[p_col]]) & (.data[[p_col]] < alpha)
  ) %>%
  pivot_wider(
    names_from = Pollinator_family,
    values_from = Sig,
    values_fill = FALSE
  ) %>%
  column_to_rownames("Plant_family") %>%
  as.matrix()

sig_mat = sig_mat[rownames(SES), colnames(SES), drop = FALSE]

# keep only strong deviations
sig_mat = sig_mat & abs(SES) >= 6


# -----------------------------
# 2) Phenology-impossible matrix:
#    TRUE if a plant-pollinator family pair never co-occurred in ANY block
# -----------------------------
possible_in_block <- function(M) {
  r_present <- rowSums(M) > 0
  c_present <- colSums(M) > 0
  outer(r_present, c_present, FUN = "&")
}

possible_list <- lapply(block_mats, possible_in_block)

# possible across the whole season if possible in ANY block
possible_any <- Reduce(`|`, possible_list)

# Phenology-impossible cells
phenology_impossible <- !possible_any
phenology_impossible <- phenology_impossible[rownames(SES), colnames(SES), drop = FALSE]

# -----------------------------
# 3) Prepare SES for plotting (cap extremes for contrast)
# -----------------------------
cap <- 6
SES_plot <- SES
SES_plot[SES_plot >  cap] <-  cap
SES_plot[SES_plot < -cap] <- -cap

# Diverging colour function
col_fun <- colorRamp2(
  c(-cap, 0, cap),
  c("#99159FFF", "#F4F4F4", "#FDAE32FF")
)

# -----------------------------
# 4) Reorder rows/cols by number of significant cells
# -----------------------------
row_sig_n <- rowSums(sig_mat, na.rm = TRUE)
col_sig_n <- colSums(sig_mat, na.rm = TRUE)

row_order <- names(sort(row_sig_n, decreasing = TRUE))
col_order <- names(sort(col_sig_n, decreasing = TRUE))

SES_plot <- SES_plot[row_order, col_order, drop = FALSE]
sig_mat  <- sig_mat[row_order, col_order, drop = FALSE]
phenology_impossible <- phenology_impossible[row_order, col_order, drop = FALSE]

# -----------------------------
# 5) Build heatmap
# -----------------------------
ht <- Heatmap(
  SES_plot,
  col = col_fun,
  cluster_rows = FALSE,
  cluster_columns = FALSE,
  column_names_rot = 45,
  column_names_gp = gpar(fontsize = 9),
  row_names_gp = gpar(fontsize = 9),
  rect_gp = gpar(col = "grey88", lwd = 0.25),
  na_col = "grey95",
  name = "SES",
  column_title = "Pollinator preference",
  column_title_gp = gpar(fontsize = 16, fontface = "bold"),
  heatmap_legend_param = list(
    title = "SES",
    at = c(-cap, 0, cap),
    labels = c(
      paste0("-", cap, " (Avoidance)"),
      "0",
      paste0("+", cap, " (Association)")
    )
  ),
  cell_fun = function(j, i, x, y, width, height, fill) {
    
    # Phenology-impossible cells: white background + diagonal cross
    if (isTRUE(phenology_impossible[i, j])) {
      grid.rect(
        x = x, y = y,
        width = width, height = height,
        gp = gpar(fill = "white", col = "grey85", lwd = 0.3)
      )
      
      grid.segments(
        x0 = x - width * 0.22, y0 = y - height * 0.22,
        x1 = x + width * 0.22, y1 = y + height * 0.22,
        gp = gpar(col = "grey55", lwd = 0.6)
      )
      grid.segments(
        x0 = x - width * 0.22, y0 = y + height * 0.22,
        x1 = x + width * 0.22, y1 = y - height * 0.22,
        gp = gpar(col = "grey55", lwd = 0.6)
      )
    }
    
    # Significant cells: black dot
    if (isTRUE(sig_mat[i, j]) && !isTRUE(phenology_impossible[i, j])) {
      grid.points(
        x = x, y = y,
        pch = 16,
        size = unit(1.2, "mm"),
        gp = gpar(col = "grey20")
      )
    }
  }
)

# -----------------------------
# 6) Custom legends for symbols
# -----------------------------
lgd_sig <- Legend(
  labels = "P < 0.01",
  title = NULL,
  type = "points",
  pch = 16,
  size = unit(1.2, "mm"),
  legend_gp = gpar(col = "grey20")
)

lgd_impossible <- Legend(
  labels = "Spatiotemporally impossible",
  title = NULL,
  graphics = list(function(x, y, w, h) {
    grid.rect(
      x = x, y = y,
      width = w * 0.9,
      height = h * 0.9,
      gp = gpar(fill = "white", col = "grey85", lwd = 0.3)
    )
    grid.segments(
      x0 = x - w * 0.22, y0 = y - h * 0.22,
      x1 = x + w * 0.22, y1 = y + h * 0.22,
      gp = gpar(col = "grey55", lwd = 0.6)
    )
    grid.segments(
      x0 = x - w * 0.22, y0 = y + h * 0.22,
      x1 = x + w * 0.22, y1 = y - h * 0.22,
      gp = gpar(col = "grey55", lwd = 0.6)
    )
  })
)

extra_legends <- packLegend(
  lgd_sig,
  lgd_impossible,
  direction = "vertical",
  gap = unit(1, "mm")
)

# -----------------------------
# 7) Draw heatmap + extra legends
# -----------------------------
draw(
  ht,
  padding = unit(c(10, 15, 10, 15), "mm"),
  heatmap_legend_side = "right",
  annotation_legend_side = "right",
  annotation_legend_list = list(extra_legends),
  merge_legends = TRUE
)

