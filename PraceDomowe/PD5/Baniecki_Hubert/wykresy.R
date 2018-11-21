library(tidyverse)
library(scales)
library(RColorBrewer)
kolory <- c(brewer.pal(4,"Set1"), brewer.pal(7,"Pastel2"))
kolory2 <- brewer.pal(11,"Spectral")[-6][c(1,3,2,4,9,7,8,10,5,6)]
data <- read.csv("2015.csv")
# data %>% select(Country, Region, Happiness.Rank, Happiness.Score)
x <- data %>% group_by(Region) %>% summarise(count = n(), value = mean(Happiness.Score))

x <- x[c(2,10,3,7,8,5,9,6,4,1),]
z <- as.character(x$Region)
y <- c("Europa Śr-Wsch", "Europa Zach", "Azja Wsch", "Azja Pd-Wsch", "Azja Pd", "Bliski Wsch i Afryka Pn", "Afryka Pd", "Ameryka Pn", "Ameryka Pd", "Australia i NZ")
x$Region <- factor(y, levels=y)

x
p <- ggplot(x, aes(x="", y=value, fill = Region)) +
  geom_bar(stat="identity") + 
  coord_polar("y", start = 0) +
  scale_fill_brewer(palette="RdGy") + 
  #scale_fill_manual(values = kolory2) +
  labs(title = "Stopień ludzkiego zadowolenia z życia dla regionów Ziemii", x="", y="") + 
    theme_bw() +
  theme(axis.text.x = element_blank()) 
  #geom_text(aes(y = value[10:1]/2.5 + c(0, cumsum(value[10:1])[-length(value[10:1])]), label = round(value[10:1],2)))

p

kolory3 <- brewer.pal(11,"RdGy")[-6][c(10,1,3,9,6,8,4,5,7,2)]

p2 <- ggplot(data, aes(x=Region, y = Happiness.Score, color=Region)) +
  geom_boxplot() + coord_flip() + geom_jitter(shape=16, position=position_jitter(0.1)) +
  scale_color_manual(values=kolory3) + 
  scale_x_discrete(labels=y, limits=z) + 
  labs(x="", y="Stopień ludzkiego zadowolenia z życia", title="Rozkład ludzkiego zadowolenia z życia dla krajów świata") +
  theme_bw() +
  guides(color=FALSE)


p2
    