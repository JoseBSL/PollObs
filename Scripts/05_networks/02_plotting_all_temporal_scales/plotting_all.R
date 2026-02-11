


library(patchwork)

# procrustes
panel1 = readRDS("Data/Working_files/Figure2_panel1.rds")
panel2 = readRDS("Data/Working_files/Figure2_panel2.rds")
panel3 = readRDS("Data/Working_files/Figure2_panel3.rds")

# mantel
#panel1 = readRDS("Data/Working_files/FigureS2_panel1.rds")
#panel2 = readRDS("Data/Working_files/FigureS2_panel2.rds")
#panel3 = readRDS("Data/Working_files/FigureS2_panel3.rds")

panel1 + panel2 + panel3


