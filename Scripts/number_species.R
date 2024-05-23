library(tidyverse)
library(readxl)


# Angiospermas ------------------------------------------------------------



angio = read_excel("Data/Raw/datasheet_angiosperms_07Nov2023.xlsx")

##looking in powo and ipni
angio_rep = present_FB(angio)

x <- angio_rep %>% filter(publicationYear >= 2013) %>% 
                        count('publicationYear') %>% 
                        mutate(tot = cumsum(freq))

write_csv(x, file = "Data/Metadata/Angiosperms/angio_riqueza.csv")

#themes
mynamestheme <- theme(strip.text = element_text(family = "serif", size = (11)),
                      axis.title = element_text(family = "serif", size = (12),
                                                face = "bold"),
                      axis.text = element_text(family = "serif", size = (9)),
                      legend.title = element_text(family = "serif", size = (11),
                                                  face = "bold"),
                      legend.text = element_text(family = "serif", size = (10)),
                      legend.position = "bottom",
                      legend.background = element_rect(fill="gray90",
                                                       size=.5, linetype="dotted"))


##with the cumulative sum
png("text/Figures/angio_sp_acum.png", width = 17,
    height = 12, units = "cm", 
    pointsize = 8, res = 300)


ggplot(x, aes(x = publicationYear, y = tot))+
  geom_line()+
  geom_point()+
  geom_label(label = x$tot)+
  geom_label(label = "HV", x = 2013, y = 400, fill = "lightblue")+
  geom_label(label = "Flora Brasil", x = 2020, y = 1950, fill = "lightblue")+
  scale_x_continuous(breaks = seq(min(x$publicationYear), max(x$publicationYear), by = 1))+
  xlab("Ano")+
  ylab("Espécies acumuladas")+
  ggtitle("Angiospermas")+
  theme_bw()

dev.off()

##with the frequency
png("text/Figures/angio_sp_freq.png", width = 17,
    height = 12, units = "cm", 
    pointsize = 8, res = 300)

ggplot(x, aes(x = publicationYear, y = freq))+
  geom_line()+
  geom_point()+
  geom_label(label = x$freq)+
  #geom_label(label = "HV", x = 2013, y = 150, fill = "lightblue")+
  #geom_label(label = "Flora Brasil", x = 2020, y = 765, fill = "lightblue")+
  scale_x_continuous(breaks = seq(min(x$publicationYear), max(x$publicationYear), by = 1))+
  xlab("Ano")+
  ylab("Espécies descritas")+
  ggtitle("Angiospermas")+
  theme_bw()

dev.off()


# Gimnospermas ------------------------------------------------------------

gimno <- read_excel("Data/Raw/Gimnospermas_07 08 2023.xlsx")

##looking in powo and ipni
gimno_rep = present_FB(gimno)

x <- gimno_rep %>% 
  count('publicationYear') %>% 
  mutate(tot = cumsum(freq)) %>% 
  drop_na() %>% 
  filter(publicationYear >= 1982)
write_csv(x, file = "Data/Metadata/Angiosperms/gimno_riqueza.csv")

#themes
mynamestheme <- theme(strip.text = element_text(family = "serif", size = (11)),
                      axis.title = element_text(family = "serif", size = (12),
                                                face = "bold"),
                      axis.text = element_text(family = "serif", size = (9)),
                      legend.title = element_text(family = "serif", size = (11),
                                                  face = "bold"),
                      legend.text = element_text(family = "serif", size = (10)),
                      legend.position = "bottom",
                      legend.background = element_rect(fill="gray90",
                                                       size=.5, linetype="dotted"))


##with the cumulative sum
png("text/Figures/gimno_sp_acum.png", width = 35,
    height = 10, units = "cm", 
    pointsize = 8, res = 300)


ggplot(x, aes(x = publicationYear, y = tot))+
  geom_line()+
  geom_point()+
  geom_label(label = x$tot)+
  geom_label(label = "HV", x = 2013, y = 18, fill = "lightblue")+
  geom_label(label = "Flora Brasil", x = 2020, y = 19, fill = "lightblue")+
  scale_x_continuous(breaks = seq(min(x$publicationYear), max(x$publicationYear), by = 1))+
  xlab("Ano")+
  ylab("Espécies acumuladas")+
  ggtitle("Gimnospermas")+
  theme_bw()

dev.off()

##with the frequency
png("text/Figures/angio_sp_freq.png", width = 35,
    height = 10, units = "cm", 
    pointsize = 8, res = 300)

ggplot(x, aes(x = publicationYear, y = freq))+
  geom_line()+
  geom_point()+
  geom_label(label = x$freq)+
  #geom_label(label = "HV", x = 2013, y = 150, fill = "lightblue")+
  #geom_label(label = "Flora Brasil", x = 2020, y = 765, fill = "lightblue")+
  scale_x_continuous(breaks = seq(min(x$publicationYear), max(x$publicationYear), by = 1))+
  xlab("Ano")+
  ylab("Espécies descritas")+
  ggtitle("Gimnospermas")+
  theme_bw()

dev.off()

###################Samambaias e Licofitas##################################

samam <- read_excel("Data/Raw/Samambaias e Licofitas_07 08 2023.xlsx")


##looking in powo and ipni
samam_rep = present_FB(samam)

x <- samam_rep %>% filter(publicationYear >= 2013) %>% 
  count('publicationYear') %>% 
  mutate(tot = cumsum(freq))

write_csv(x, file = "Data/Metadata/Angiosperms/samam_riqueza.csv")

#themes
mynamestheme <- theme(strip.text = element_text(family = "serif", size = (11)),
                      axis.title = element_text(family = "serif", size = (12),
                                                face = "bold"),
                      axis.text = element_text(family = "serif", size = (9)),
                      legend.title = element_text(family = "serif", size = (11),
                                                  face = "bold"),
                      legend.text = element_text(family = "serif", size = (10)),
                      legend.position = "bottom",
                      legend.background = element_rect(fill="gray90",
                                                       size=.5, linetype="dotted"))


##with the cumulative sum
png("text/Figures/samam_sp_acum.png", width = 17,
    height = 12, units = "cm", 
    pointsize = 8, res = 300)


ggplot(x, aes(x = publicationYear, y = tot))+
  geom_line()+
  geom_point()+
  geom_label(label = x$tot)+
  geom_label(label = "HV", x = 2013, y = 10, fill = "lightblue")+
  geom_label(label = "Flora Brasil", x = 2020, y = 105, fill = "lightblue")+
  scale_x_continuous(breaks = seq(min(x$publicationYear), max(x$publicationYear), by = 1))+
  xlab("Ano")+
  ylab("Espécies acumuladas")+
  ggtitle("Samambaias e Licófitas")+
  theme_bw()

dev.off()

##with the frequency
png("text/Figures/samam_sp_freq.png", width = 17,
    height = 12, units = "cm", 
    pointsize = 8, res = 300)

ggplot(x, aes(x = publicationYear, y = freq))+
  geom_line()+
  geom_point()+
  geom_label(label = x$freq)+
  #geom_label(label = "HV", x = 2013, y = 150, fill = "lightblue")+
  #geom_label(label = "Flora Brasil", x = 2020, y = 765, fill = "lightblue")+
  scale_x_continuous(breaks = seq(min(x$publicationYear), max(x$publicationYear), by = 1))+
  xlab("Ano")+
  ylab("Espécies descritas")+
  ggtitle("Samambaias e Licófitas")+
  theme_bw()

dev.off()
