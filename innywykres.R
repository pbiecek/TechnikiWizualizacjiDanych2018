library(ggplot2)
dane<-data.frame(nazwa=factor(c("Podstawowe nieukończone","Podstawowe ukończone","Zasadnicze zawodowe","Średnie ogólno-kształcące",'Średnie zawodowe','Policealne','Wyższe'),levels = c("Podstawowe nieukończone","Podstawowe ukończone","Zasadnicze zawodowe","Średnie ogólno-kształcące",'Średnie zawodowe','Policealne','Wyższe')),
                 procenty=c(2.4,37.9,38.8,2.2,15.9,1.0,1.6))


ggplot(data=dane, aes(x=nazwa, y=procenty)) +
  geom_bar(stat="identity") + labs(title="Użytkownicy pracujący wyłącznie lub głównie w swoim\n gospodarstwie rolnym (na działce rolnej)\n według poziomu wykształcenia w 2002 r.",x=" ") + coord_flip() +theme(title = element_text(vjust = 0.5))
  
  

