##############Figures

library(kewr)
library(dplyr)
library(tidyr)
library(readr)
library(readxl)
library(plyr)
library(ggplot2)


######families with missing genus and species

mis_general <- df_ipni_powo_absent %>% group_by(family) %>%
                               summarise(unique_genus = n_distinct(genus),
                                    unique_sp = n_distinct(name)) %>%
                     ungroup() %>% 
     pivot_longer(starts_with("unique"), names_to = "taxon", values_to = "number")


mis_plus20 <- df_ipni_powo_absent %>% group_by(family) %>%
  summarise(unique_genus = n_distinct(genus),
            unique_sp = n_distinct(name)) %>%
  ungroup() %>% filter(unique_genus >= 20) %>% 
  pivot_longer(starts_with("unique"), names_to = "taxon", values_to = "number")

#####plotting families with more than 20 missing genus


p1 <- mis_general %>%  
      ggplot(aes(x = family, y = number, fill = taxon))+
      geom_bar(stat = "identity", position = "dodge")



png("text/Figures/missing_plus20.png",
    width = 17, height = 18, units = "cm",
    pointsize = 8, res = 300)

p2 <- mis_plus20 %>%  
            ggplot(aes(x = family, y = number, fill = taxon))+
            geom_bar(stat = "identity", position = "dodge") +
      scale_fill_manual(values = c("#d8b365", "#5ab4ac"),
                        name = "Taxa",
                        breaks = c("unique_genus", "unique_sp"),
                        labels = c("Gênero", "Espécies"))+
      coord_flip()+
     xlab("Família")+
     ylab("Número de taxa faltante")+
     ggtitle("Famílias com mais de 20 gêneros com espécies faltando no Flora")+
      theme_bw()+
     theme(legend.position = "bottom",
           legend.title = element_text(family = "serif", size = (11),
                                       face = "bold"),
           legend.text = element_text(family = "serif", size = (10)),
           axis.title = element_text(family = "serif", size = (11),
                                     face = "bold"),
           axis.text = element_text(family = "serif", size = (11)),
           plot.title = element_text(family = "serif", size = (12),
                                     face = "bold"))

p2

dev.off()



