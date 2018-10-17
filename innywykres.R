library(ggplot2)
library(ggplot2)
dane<-data.frame(nazwa=factor(c("Podstawowe nieukończone","Podstawowe ukończone","Zasadnicze zawodowe","Średnie ogólno-kształcące",'Średnie zawodowe','Policealne','Wyższe')),
                 procenty=c(2.4,37.9,38.8,2.2,15.9,1.0,1.6))
ggplot(dane,aes(x=nazwa,y=procenty,fill=nazwa)) + geom_col(width=0.7) + 
  geom_text(aes(label=as.character(procenty),y=procenty+1),size=5,fontface="bold") + guides(fill=FALSE) + 
  coord_fixed(ratio = 0.25) +
  labs(title="Użytkownicy pracujący wyłącznie lub głównie w swoim gospodarstwie rolnym (na działce rolnej) według poziomu wykształcenia w 2002 r.",y="",x="",subtitle="(W PROC.)") +
  theme_minimal()+theme(panel.border = element_blank(),
                        panel.grid.major = element_blank(),
                        panel.grid.minor = element_blank(),
                        axis.text.y = element_blank(),
                        axis.text = element_text(face = "bold",size = rel(1.5)),
                        plot.title = element_text(size = rel(1.75),hjust=-.1,face="bold")
                        
  )
                        
  )
