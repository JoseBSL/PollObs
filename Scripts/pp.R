library(ggplot2)

# Crear secuencia temporal
dias <- seq(1, 300, by = 1)

# Crear data frame para 3 polinizadores
pollinadores <- data.frame(
  dia = rep(dias, 3),
  densidad = 10 * c(
    dnorm(dias, mean=50,  sd=20),   
    dnorm(dias, mean=120, sd=30),   
    dnorm(dias, mean=220, sd=20)    
  ),
  especie = factor(rep(c("a", "b", "c"), each=length(dias))),
  tipo = "Insect"
)

# Crear data frame para 3 plantas
plantas <- data.frame(
  dia = rep(dias, 3),
  densidad = 10 * c(
    dnorm(dias, mean=60,  sd=25),   
    dnorm(dias, mean=140, sd=25),   
    dnorm(dias, mean=230, sd=25)    
  ),
  especie = factor(rep(c("a", "b", "c"), each=length(dias))),
  tipo = "Plant"
)

# Unir ambos data frames
fenologia <- rbind(pollinadores, plantas)

# Graficar con facet_wrap por tipo (plantas vs polinizadores), apilados verticalmente
ggplot(fenologia, aes(x=dia, y=densidad, fill=especie, color=especie)) +
  geom_area(alpha=0.25, size=0.7, position = "identity") +
  facet_wrap(~ tipo, scales = "free_y", ncol = 1) +
  theme_bw() +
  theme(
    text = element_text(size=14),
    legend.title = element_blank(),
    axis.line.x = element_blank(),
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  ) +
  xlab("Time") +
  ylab("Abundance") +
  scale_fill_brewer(palette = "Dark2") +
  scale_color_brewer(palette = "Dark2") +
  scale_y_continuous(expand = c(0, 0)) +
  coord_cartesian(clip = "off")
