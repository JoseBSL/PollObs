#Combine global and local trait spectrum in a single graph
#This is the first version without adding more traits to
#the local reproductive spectrum

#Load libraries
library(dplyr)
library(ggplot2)
library(patchwork)

# Theme 
theme_ms <- function(base_size=12, base_family="Helvetica") {
  (theme_bw(base_size = base_size, base_family = base_family)+
     theme(text=element_text(color="black"),
           axis.title=element_text( size = rel(1.6)),
           axis.text=element_text(size = rel(1.6), color = "black"),
           legend.title=element_text(face="bold"),
           legend.text=element_text(size = rel(1.1)),
           legend.background=element_rect(fill="transparent"),
           legend.key.size = unit(0.4, 'lines'),
           panel.border=element_rect(color="black",size=1),
           panel.grid.minor.x =element_blank(),
           panel.grid.minor.y= element_blank(),
           panel.grid.major= element_blank(),
           plot.title = element_text(size = rel(2.5), face="bold")
     ))
}

#Load global PCA data
p1 = readRDS("Data/Working_files/global_pca_phenobs_species_output.rds")
d1 = readRDS("Data/Working_files/global_pca_phenobs_species_data.rds")

#Load local PCA data 
p2 = readRDS("Data/Working_files/local_pca_phenobs_species_output.rds")
d2 = readRDS("Data/Working_files/local_pca_phenobs_species_data.rds")

#Global PCA graph
PC = p1

global_pollobs_pca <- function(PC, x="PC1", y="PC2") {
  # PC being a prcomp object
  data <- data.frame(PC$S)
  plot <- ggplot(data, aes_string(x=x, y=y)) 
  
  dat <- data.frame(x = data[,x], y = data[,y])
  
  #######
  #DENSITY FUNCTION
  #######
  get_density <- function(x, y, ...) {
    dens <- MASS::kde2d(x, y, ...)
    ix <- findInterval(x, dens$x)
    iy <- findInterval(y, dens$y)
    ii <- cbind(ix, iy)
    return(dens$z[ii])
  }
  
  dat$density <- get_density(dat$x, dat$y, h = c(2, 2), n = 1000) 
  
  dat$PhenObs <- as.factor(all_species1$Pollobs)
  
  plot <- plot + geom_point(data=dat, aes(-x, -y,colour=PhenObs), size=2.25,  alpha = 0.85) +
    scale_colour_manual(values=c("#1a78ab", "#cc67ff"))
  
  ########
  #ADD ARROWS 
  ########
  datapc <- data.frame(PC$L) 
  mult <- min(
    (max(data[,y]) - min(data[,y])/(max(datapc[,y])-min(datapc[,y]))),
    (max(data[,x]) - min(data[,x])/(max(datapc[,x])-min(datapc[,x])))
  )
  datapc <- transform(datapc,
                      v1 = .65 * mult * (get(x)),
                      v2 = .65 * mult * (get(y))
  )
  
  plot <- plot + geom_segment(data=datapc,linejoin="round", lineend="round",aes(x=0, y=0, xend=-v1, yend=-v2),size=1.2, arrow=arrow(length=unit(0.5,"cm")), alpha=1, color="black")
  
  percentage <- round(diag(PC$Eval) / sum(PC$Eval) * 100, 2) 
  
  plot <- plot + geom_segment(data=datapc, aes(x=0, y=0, xend=-v1, yend=-v2),size=0.8, arrow=arrow(length=unit(0,"cm")),linetype=2, alpha=0.8, color="black")
  
  rownames(PC$L) <- c("S", "FN", "FS", "SL", "ON", "PH" )
  
  PCAloadings <- data.frame(Variables = rownames(PC$L), PC$L)
  plot <- plot + annotate("text", x = -(PCAloadings$PC1*c(4.6,5.2,5.25,7.9,7.1,6)), y = -(PCAloadings$PC2*c(4.45,4.2,5.4,7.7,6,6.5)+c(0,0,0,0,-0.1,0)),
                          label = PCAloadings$Variables, color="black",size=6, fontface=2)
  
  # CHANGE THEME and reverse the y-axis if selfing should be low at the bottom
  plot <- plot + theme_ms() + 
    ylim(-4, 4) + 
    xlim(-4, 4) + 
    scale_y_reverse() +  # This reverses the y-axis
    theme(legend.position = c(0.220, 0.130)) + 
    xlab("PC1") + 
    ylab("PC2")
  
  plot + theme(legend.position = "top", legend.justification = c(0, 1))
}

# Call the function
plot1 = global_pollobs_pca(PC)

# Load plots
PC = p2

local_phenobs_pca <- function(PC, x = "PC1", y = "PC2") {
  # PC being a prcomp object
  data <- data.frame(PC$S)
  plot <- ggplot(data, aes_string(x = x, y = y)) 
  
  dat <- data.frame(x = data[, x], y = data[, y])
  
  #######
  # DENSITY FUNCTION
  #######
  get_density <- function(x, y, ...) {
    dens <- MASS::kde2d(x, y, ...)
    ix <- findInterval(x, dens$x)
    iy <- findInterval(y, dens$y)
    ii <- cbind(ix, iy)
    return(dens$z[ii])
  }
  
  dat$density <- get_density(dat$x, dat$y, h = c(2, 2), n = 1000) 
  
  plot <- plot + geom_point(data = dat, aes(-x, -y), 
                            size = 2.25, alpha = 0.85,
                            colour = "#cc67ff") 
  
  ########
  # ADD ARROWS 
  ########
  datapc <- data.frame(PC$L) 
  mult <- min(
    (max(data[, y]) - min(data[, y])) / (max(datapc[, y]) - min(datapc[, y])),
    (max(data[, x]) - min(data[, x])) / (max(datapc[, x]) - min(datapc[, x]))
  )
  
  datapc <- transform(datapc,
                      v1 = 0.65 * mult * get(x),
                      v2 = 0.65 * mult * get(y)
  )
  
  plot <- plot + geom_segment(data = datapc, linejoin = "round", lineend = "round", 
                              aes(x = 0, y = 0, xend = -v1, yend = -v2), size = 1.2, 
                              arrow = arrow(length = unit(0.5, "cm")), alpha = 1, color = "black")
  
  # Percentage for the axes
  percentage <- round(diag(PC$Eval) / sum(PC$Eval) * 100, 2) 
  
  plot <- plot + geom_segment(data = datapc, aes(x = 0, y = 0, xend = -v1, yend = -v2), 
                              size = 0.8, arrow = arrow(length = unit(0, "cm")), 
                              linetype = 2, alpha = 0.8, color = "black")
  
  rownames(PC$L) <- c("S", "FN", "FS", "SL", "ON", "PH")
  PCAloadings <- data.frame(Variables = rownames(PC$L), PC$L)
  
  # Position labels at the end of segments
  label_positions <- -datapc[, c("v1", "v2")]
  
  plot <- plot + annotate("text", x = label_positions[[1]]*c(1.1,1.2,1.1,1.18,1,1.15), y = label_positions[[2]]*c(5,1.35,1,0.9,1,1.5), 
                          label = PCAloadings$Variables, color = "black", 
                          size = 6, fontface = 2, vjust = 1.5)  # Adjust vjust if needed
  
  # CHANGE THEME 
  plot <- plot + theme_ms() + 
    ylim(-4, 4) + 
    xlim(-4, 4) + 
    scale_y_reverse() +  # This reverses the y-axis
    theme(legend.position = c(0.220, 0.130)) + 
    xlab("PC1") + 
    ylab("PC2")
  
  plot + theme(legend.position = "top", legend.justification = c(0, 1))
}
# Call the function
plot2 = local_phenobs_pca(PC)


#Combine plots
plot1 + plot2

