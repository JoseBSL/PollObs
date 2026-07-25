
library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(patchwork)

d_prime_poll_interactions <- readr::read_csv("Data/Working_files/average_d_prime_by_week_corrected_by_int.csv") %>%
  dplyr::select(Botanical_garden, Week, av_dprime_poll, 
                av_dprime_poll_upper, av_dprime_poll_lower) %>%
  rename(av_dprime_poll_int = av_dprime_poll, 
         av_dprime_poll_int_upper = av_dprime_poll_upper, 
         av_dprime_poll_int_lower = av_dprime_poll_lower)

d_prime_poll_abundance <- readr::read_csv("Data/Working_files/average_d_prime_by_week_corrected_by_ab.csv") %>%
  dplyr::select(Botanical_garden, Week, av_dprime_poll, 
                av_dprime_poll_upper, av_dprime_poll_lower) %>%
  rename(av_dprime_poll_ab = av_dprime_poll, 
         av_dprime_poll_ab_upper = av_dprime_poll_upper, 
         av_dprime_poll_ab_lower = av_dprime_poll_lower)


average_specialization <- d_prime_poll_interactions %>%
  left_join(d_prime_poll_abundance, by = c("Botanical_garden","Week"))

average_specialization_SEASON_int <- readr::read_csv("Data/Working_files/average_d_prime_by_SEASON_corrected_by_int.csv")
average_specialization_SEASON_ab <- readr::read_csv("Data/Working_files/average_d_prime_by_SEASON_corrected_by_ab.csv")


plot_av_dprime_poll_interactions <- ggplot(average_specialization, aes(x=Week,y=av_dprime_poll_int, color = Botanical_garden))+
  geom_point(size=3)+
  geom_line(linewidth=1.5)+
  geom_hline(yintercept = 
               average_specialization_SEASON_int$av_dprime_poll[average_specialization_SEASON_int$Botanical_garden=="Jena"],
             linewidth = 1.5, color = "#00BA38", linetype = "dashed")+
  geom_hline(yintercept = 
               average_specialization_SEASON_int$av_dprime_poll[average_specialization_SEASON_int$Botanical_garden=="Halle"],
             linewidth = 1.5, color = "#F8766D", linetype = "dashed")+
  geom_hline(yintercept = 
               average_specialization_SEASON_int$av_dprime_poll[average_specialization_SEASON_int$Botanical_garden=="Leipzig"],
             linewidth = 1.5, color = "#619CFF", linetype = "dashed")+
  # geom_ribbon(aes(ymax = av_dprime_poll_int_upper,
  #                 ymin = av_dprime_poll_int_lower, 
  #                 fill = Botanical_garden),alpha=.2, colour = NA)+
  ylim(0,1)+
  labs(x="Week number", y= "Weighted mean d prime index\nfor pollinators", title = "Only interactions\nare considered", color = NULL)+
  theme_bw()+
  theme(legend.position = "bottom", legend.title = element_blank())+
  theme(legend.text = element_text(size = 18),
        axis.text=element_text(size=16),
        axis.title=element_text(size=16,face="bold"),
        plot.title=element_text(size=16,face="bold"),
        strip.text = element_text(size = 18))+ guides(fill="none")

plot_av_dprime_poll_abundance <- ggplot(average_specialization, aes(x=Week,y=av_dprime_poll_ab, color = Botanical_garden))+
  geom_point(size=3)+
  geom_line(linewidth=1.5)+
  geom_hline(yintercept = 
               average_specialization_SEASON_ab$av_dprime_poll[average_specialization_SEASON_ab$Botanical_garden=="Jena"],
             linewidth = 1.5, color = "#00BA38", linetype = "dashed")+
  geom_hline(yintercept = 
               average_specialization_SEASON_ab$av_dprime_poll[average_specialization_SEASON_ab$Botanical_garden=="Halle"],
             linewidth = 1.5, color = "#F8766D", linetype = "dashed")+
  geom_hline(yintercept = 
               average_specialization_SEASON_ab$av_dprime_poll[average_specialization_SEASON_ab$Botanical_garden=="Leipzig"],
             linewidth = 1.5, color = "#619CFF", linetype = "dotted")+
  # geom_ribbon(aes(ymax = av_dprime_poll_ab_upper,
  #                 ymin = av_dprime_poll_ab_lower, 
  #                 fill = Botanical_garden),alpha=.2, colour = NA)+
  ylim(0,1)+
  labs(x="Week number", y= "Weighted mean d prime index\nfor pollinators", title = "Flower abundance\nis also considered", color = NULL)+
  theme_bw()+
  theme(legend.position = "bottom", legend.title = element_blank())+
  theme(legend.text = element_text(size = 18),
        axis.text=element_text(size=16),
        axis.title=element_text(size=16,face="bold"),
        plot.title=element_text(size=16,face="bold"),
        strip.text = element_text(size = 18))+ guides(fill="none")


(plot_av_dprime_poll_interactions|plot_av_dprime_poll_abundance) + plot_layout(guides = "collect") & theme(legend.position = 'bottom')
