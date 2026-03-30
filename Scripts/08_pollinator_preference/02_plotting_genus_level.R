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
pval_df = readRDS("Data/Working_files/pval_df_genus.rds")
SES = readRDS("Data/Working_files/SES_genus.rds")
block_mats = readRDS("Data/Working_files/block_mats_genus.rds")

alpha = 0.05
p_col = "p_two"   # or "p_fdr" if you computed it

# -----------------------------
# 1) Build significance matrix aligned to SES
# -----------------------------
sig_mat = pval_df %>%
  transmute(
    Plant_family,
    Pollinator_genus,
    Sig = !is.na(.data[[p_col]]) & (.data[[p_col]] < alpha)
  ) %>%
  pivot_wider(
    names_from  = Pollinator_genus,
    values_from = Sig,
    values_fill = FALSE
  ) %>%
  column_to_rownames("Plant_family") %>%
  as.matrix()

sig_mat = sig_mat[rownames(SES), colnames(SES)]

# -----------------------------
# 2) Phenology-impossible matrix:
#    TRUE if a plant–poll family pair never co-occurred in ANY garden-week block
# -----------------------------
# Convert each block matrix to presence/absence of "possible interaction"
# Here, "possible" is defined as: both families are present in that block,
# which is equivalent to: the cell exists in the padded matrix (it does) and
# at least one of row/col totals > 0 in that block. A robust way:
# mark possible cells where BOTH row sum and col sum are > 0 in that block.
possible_in_block <- function(M) {
  r_present = rowSums(M) > 0
  c_present = colSums(M) > 0
  outer(r_present, c_present, FUN = "&")
}

possible_list = lapply(block_mats, possible_in_block)

# possible across the whole season if possible in ANY block
possible_any = Reduce(`|`, possible_list)

# Phenology-impossible cells
phenology_impossible = !possible_any
phenology_impossible = phenology_impossible[rownames(SES), colnames(SES)]

# -----------------------------
# 3) Prepare SES for plotting (cap extremes for contrast)
# -----------------------------
cap <- 6
SES_plot <- SES
SES_plot[SES_plot >  cap] <-  cap
SES_plot[SES_plot < -cap] <- -cap

# Keep SES NA as NA (these are "not testable / zero-variance" etc.)
# Phenology-impossible will be shown via "×" overlay and (optionally) a different NA color.

# Diverging colour function
col_fun <- colorRamp2(
  c(-cap, 0, cap),
  c("#C65DAE", "white", "#FDB863")
)

# -----------------------------
# 4) OPTIONAL: reorder rows/cols by number of significant cells
# -----------------------------
row_sig_n = rowSums(sig_mat, na.rm = TRUE)
col_sig_n = colSums(sig_mat, na.rm = TRUE)

row_order = names(sort(row_sig_n, decreasing = TRUE))
col_order = names(sort(col_sig_n, decreasing = TRUE))

SES_plot = SES_plot[row_order, col_order, drop = FALSE]
sig_mat  = sig_mat [row_order, col_order, drop = FALSE]
phenology_impossible <- phenology_impossible[row_order, col_order, drop = FALSE]

# -----------------------------
# 5) Build heatmap
# -----------------------------
ht = Heatmap(
  SES_plot,
  col = col_fun,
  cluster_rows = FALSE,
  cluster_columns = FALSE,
  column_names_rot = 45,
  column_names_gp = gpar(fontsize = 8),
  row_names_gp = gpar(fontsize = 8),
  rect_gp = gpar(col = "grey70", lwd = 0.4),
  na_col = "grey90",
  name = "SES",
  column_title = "Pollinator preference",
  column_title_gp = gpar(fontsize = 16, fontface = "bold"),
  heatmap_legend_param = list(
    title = "SES",
    at = c(-cap, 0, cap),
    labels = c(paste0("-", cap, " (Avoidance)"), "0", paste0("+", cap, " (Association)"))
  ),
  cell_fun = function(j, i, x, y, width, height, fill) {
    
    # Dark grey for phenology-impossible cells
    if (isTRUE(phenology_impossible[i, j])) {
      grid.rect(
        x = x, y = y,
        width = width,
        height = height,
        gp = gpar(fill = "grey60",lwd = 0.4, col = "grey70")
      )
    }
    
    # Significance star
    if (isTRUE(sig_mat[i, j])) {
      grid.text("*", x = x, y = y,
                gp = gpar(col = "black", fontsize = 10))
    }
  }
)

draw(ht, padding = unit(c(10, 15, 10, 15), "mm"))
