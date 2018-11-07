library(ggthemes)
library(ggplot2)

dane <- read.csv("./dane.csv", sep = ";")


ggplot(data = dane, aes(x=reorder(partia, -mandaty), y=mandaty)) +
  geom_bar(stat="identity", fill="steelblue")+
  theme_economist(base_size = 12)+
  geom_text(aes(label = paste0(round(100*mandaty/sum(mandaty)), "%")), stat = "identity", 
            vjust = 2,colour='white',size=5,fontface=2)+
  scale_y_continuous(breaks=seq(0,20,2))+
  labs(title="Podział mandatów do Sejmiku Województwa Małopolskiego", 
       x="Partia", y = "Liczba uzyskanych mandatów", size = 10)+
  theme(axis.text=element_text(size=12),
       axis.title=element_text(size=12,face="bold"),
       plot.title = element_text(hjust = 0.5))
