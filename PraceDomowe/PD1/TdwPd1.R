#Pierwszy wykres
library("ggplot2")
library("ggthemes")
library("scales")
library("ggfortify")
library("ggalt")

library("data.table")

points_scale <- 0:25
Miasta <- c("Gdańsk", "Katowice", "Warszawa", "Lublin", "Wrocław")
Punkty <- c(24,23,21,20,12)
data <- data.frame(Miasta,Punkty)

theme_set(theme_solarized_2(light=FALSE))
g <- ggplot(data, aes(x = reorder(Miasta, -Punkty),y = Punkty, group = Miasta, fill = Miasta))
g + geom_bar(stat="identity", colour = "black", width = 0.5) + 
  theme(axis.text.x = element_text(size=15),axis.title.x=element_blank()) +
  labs(title="W jakim mieście opłaca się kupić mieszkanie na wynajem:", 
       subtitle="Ranking pod wzgędem lokalizacji") +
  geom_text(aes(label=Punkty, color = Miasta, size = 15), position=position_dodge(width=0.9), vjust=-0.25) + theme(legend.position="none")

#Drugi Wykres

Wielkość <- c("35-40", "41-50", "do 35", "51-60", "71-80")
ROI <- c(9.21, 8.81, 8.73, 7.97, 7.84)
data <- data.frame(Wielkość,ROI)

theme_set(theme_economist())
g <- ggplot(data, aes(x = reorder(Wielkość, -ROI), y=ROI))
g + geom_bar(stat="identity", colour = "black", width = 0.5) + 
  scale_x_discrete(name ="Powierzchnia (m^2)") +
  scale_y_continuous(breaks = seq(0, 9, by = 1.0)) +
  ylab("ROI (%)")+
  labs(title="Ile można zarobić w zależności od wielkości mieszkania.") +
  geom_text(aes(label=ROI), position=position_dodge(width=0.9), vjust=-0.25) 
