library(reshape2)
library(ggplot2)

dane <- read.csv("./dane.csv", sep = ";")
dane <- dane[with(dane, order(pierwotny)),]
dane <- dane[nrow(dane):1,]
dane$miasto <- factor(dane$miasto, levels = dane$miasto)
dat.m<-melt(dane, id.vars = "miasto")  
names(dat.m) <- c("miasto", "rynek", "cena")

#stary wykres
ggplot(data = dat.m,aes(x = miasto, y = cena, fill = rynek))+
  geom_bar(stat = "identity", width =  0.8, position=position_dodge())+
  scale_fill_brewer(palette="Paired")+
  theme_minimal() +
  labs(title="Przeciętne ceny za mkw. mieszkania w największych miastach Polski na rynku wtórnym i pierwotnym", x="miasto", y = "średnia cena za mkw. (w zl)") +
  theme(axis.text.x = element_text(angle = 45)) 


#wykres a
ggplot(data = dat.m,aes(x = miasto, y = cena, fill = rynek))+
  coord_flip() +
  geom_bar(stat = "identity", position=position_dodge())+
  scale_fill_brewer(palette="Set2")+
  theme_economist(horizontal=FALSE,base_size = 11)+
  theme(axis.ticks.y = element_blank(),        
        axis.text.y = element_blank(),
        axis.text=element_text(size=16),
        axis.title=element_text(size=14,face="bold"))+
  labs(title="Przeciętne ceny za mkw. mieszkania w największych miastach Polski na rynku wtórnym i pierwotnym", 
       x="miasto", y = "średnia cena za mkw. (w zl)") +
  guides(fill=guide_legend(title="Rynek",reverse=TRUE))+
  annotate(geom = 'text', label = dane$miasto, 
           x = dane$miasto, y = 2800,colour='azure',size=7,fontface=2)+
  scale_y_continuous(breaks=seq(0,8000,1000))


#wykres b
ggplot(data = dat.m,aes(x = miasto, y = cena, fill = rynek))+
  coord_flip() +
  geom_bar(stat = "identity", position=position_dodge())+
  scale_fill_brewer(palette="Set2")+
  theme_economist(horizontal=FALSE,base_size = 12)+
  theme(axis.ticks.y = element_blank(),      
        axis.text.y = element_blank(),
        axis.text=element_text(size=16),
        axis.title=element_text(size=14,face="bold"))+
  labs(title="Przeciętne ceny za mkw. mieszkania w największych miastach Polski na rynku wtórnym i pierwotnym", 
       x="miasto", y = "średnia cena za mkw. (w zl)") +
  guides(fill=guide_legend(title="Rynek",reverse=TRUE))+
  geom_text(aes(label=ifelse((rynek == 'pierwotny'), levels(dane$miasto),'')), 
            hjust = 3,stat = "identity",colour='white',size=7,fontface=2)+
  scale_y_continuous(breaks=seq(0,8000,1000))
