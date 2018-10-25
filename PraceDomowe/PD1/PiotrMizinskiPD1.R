library(ggplot2)
library(ggrepel)

options(stringsAsFactors=FALSE)

dane <- read.csv("./dane.csv", sep = ";") #wczytanie zbioru danych

df <- expand.grid(x=8:22, y=6:21) #stworzenie siatki dla gradientu

ggplot(data = dane, aes(x = ShotsTaken, y = ShotsFaced)) + 
  geom_raster(data =df, aes(x, y,fill = x-y),interpolate = TRUE)+
  scale_fill_distiller(type = "div", palette = 8, direction=1)+ 
  geom_point(size = 5)+
  geom_hline(yintercept=mean(dane$ShotsFaced), color = "gray", size=1.5,alpha = 0.7)+
  geom_vline(xintercept = mean(dane$ShotsTaken), color = "gray", size=1.5,alpha = 0.7)+
  geom_text_repel(data = dane, aes(label=Team),size=6,force=1) +
  annotate("label", x = 20, y = 8, label = "Nieustanny atak",size=10)+
  annotate("label", x = 10.5, y = 18.5, label = "Desperacka obrona",size=10)+
  xlab('Średnia liczba strzałów oddanych na mecz') + 
  ylab("Średnia liczba strzałów oddanych przez rywali na mecz")+
  scale_x_continuous(breaks=seq(8,22,1))+
  scale_y_continuous(breaks=seq(6,21,1))+
  geom_hline(yintercept=6:21, color = "black", size=0.5,alpha = 0.1)+
  geom_vline(xintercept=8:22, color = "black", size=0.5,alpha = 0.1)+
  coord_cartesian(expand = F) + 
  theme_bw() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=16),
        plot.title = element_text(size=20, hjust = 0.5),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = "black", size=1),
        legend.position="none")+
  ggtitle("Premier League: strzały oddane vs strzały oddane przez rywali na mecz")