library(plotrix)
library(ggplot2)

dane1 <- read.csv("./dane1.csv", sep = ";")
pie3D(dane1$licznosc,labels = dane1$grupa,explode = 0.1, main = "Wykres ko³owy 3D 1")

ggplot(data = dane1, aes(x=grupa, y=licznosc)) +
  geom_bar(stat="identity", fill="steelblue")+
  ggtitle("Wykres s³upkowy 1")+
  theme(line = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank())


dane2 <- read.csv("./dane2.csv", sep = ";")
pie3D(dane2$licznosc,labels = dane2$grupa,explode = 0.1, main = "Wykres ko³owy 3D 2 ")

ggplot(data = dane2, aes(x=grupa, y=licznosc)) +
  geom_bar(stat="identity", fill="steelblue")+
  ggtitle("Wykres s³upkowy 2")+
  theme(line = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank())

dane3 <- read.csv("./dane3.csv", sep = ";")
pie3D(dane3$licznosc,labels = dane3$grupa,explode = 0.1, main = "Wykres ko³owy 3D 3")

ggplot(data = dane3, aes(x=grupa, y=licznosc)) +
  geom_bar(stat="identity", fill="steelblue")+
  ggtitle("Wykres s³upkowy 3")+
  theme(line = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank())
