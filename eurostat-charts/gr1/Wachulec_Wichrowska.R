install.packages("eurostat")
install.packages("ggrepel")
library(eurostat)
library(data.table)
library(ggplot2)
library(ggrepel)

tabela <- read.table(file = 'sdg_11_40.tsv', sep = '\t', header = TRUE)
tabela <- tidyr:::separate(tabela,"unit.geo.time",c("unit","geo"),sep=",")

tabela <- tabela[,c(1,2,18)]
tabela <- tabela[-c(11,40),]

row.names(tabela) <- 1:56

tabela1 <- tabela[1:28,]
tabela2 <- tabela[29:56,]

wynik <- merge(tabela1,tabela2,all = TRUE,by="geo")

wynik$X2016.y <- gsub("([a-z])","",wynik$X2016.y)
wynik$X2016.x <- as.numeric(as.character(wynik$X2016.x))
wynik$X2016.y <- as.numeric(wynik$X2016.y)
wynik <- wynik[,c(1,3,5)]
names(wynik) <- c("kraj","number","ratio")

ggplot(wynik,aes(x=number,y=ratio)) +
  geom_point(aes(x=number,y=ratio)) +
  xlab("Liczba ofiar w wypadkach samochodowych")+
  ylab("Proporcja ofiar na 100.000 obywateli")+
  ggtitle("Ofiary w wypadkach samochodowych w krajach EU w 2016 roku",subtitle = "Polska notuje 4 najwyzszy odsetek ofiar") +
  geom_label_repel(aes(x=number,y=ratio,label=kraj))+
  geom_point(aes(x=3026,y=8.0),color="red")+
  coord_flip()
