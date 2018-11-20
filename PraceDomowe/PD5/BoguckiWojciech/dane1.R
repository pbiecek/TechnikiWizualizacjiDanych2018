library(ggplot2)
library(ggrepel)
library(dplyr)

dane <- read.csv("dane.csv")

dane <- mutate(dane,procent_klientow=klienci/sum(klienci))
ggplot(dane,aes(x="",y=procent_klientow,fill=Bank)) + 
  geom_bar(stat="identity",width = 1, color="black") + 
  theme(panel.background = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank()) +
  scale_fill_brewer(palette = "Set3",direction = -1) +
  coord_polar("y", start=0)

ggsave("wykres1.png", width=6, height = 4)

dane$Bank <- factor(dane$Bank,levels= dane$Bank[order(dane$klienci,decreasing = TRUE)])
ggplot(dane,aes(x=Bank,y=klienci, fill=oprocentowanie)) + 
  geom_col() +
  scale_y_discrete(limits = seq(0,140,10), expand=c(0,0,0,5)) +
  ylab("Liczba klientów (w tys.)")+
  scale_fill_continuous(breaks = c(0.7,1,1.5,2,2.5))

ggsave("wykres2.png", width=8, height = 6.6)

ggplot(dane,aes(x=oprocentowanie, y=klienci, label=Bank)) + 
  geom_point(size=2.5, color="red") + 
  scale_y_discrete(limits = seq(70,140,10), expand=c(0,10,0,5)) +
  ylab("Liczba klientów (w tys.)")+
  scale_x_discrete(limits=seq(0.25,2.75,0.25)) +
  geom_text_repel()

ggsave("wykres3.png", width=6, height = 4)

dane$Bank <- factor(dane$Bank,levels= dane$Bank[order(dane$klienci,decreasing = TRUE)])
ggplot(dane[c(2,7,10),],aes(x=Bank,y=klienci, fill=oprocentowanie)) + 
  geom_col(width=1) +
  scale_y_discrete(limits = seq(0,140,10), expand=c(0,0,0,5)) +
  ylab("Liczba klientów (w tys.)")+
  scale_fill_continuous(breaks = c(0.7,1,1.5,2,2.5),limits=c(0.8,2.5))
