library(ggplot2)
ggplot()+geom_polygon(aes(x=c(-1,1,1,-1),y=c(0,0,3,3)),fill="brown")+geom_polygon(aes(x=c(0,0,4),y=c(20,1,1)),fill="green")+coord_equal()+
  geom_polygon(aes(x=-c(0,0,4),y=c(20,1,1)),fill="green")+theme_void()+
  geom_point(aes(x=c(-2,-1,0,1,2,1,-1,0,1,2,-2),y=c(2,7,15,3,6,11,12,5,8,2,5)),color=c("red"),shape=c(19))+
  theme(panel.background = element_rect(fill = "blue"))+
  geom_point(aes(x=0,y=20),shape=8,color="gold",size=8)+
  geom_curve(aes(x=-3.2,y=5,xend=3.2,yend=5),color="gold",size=2)+
  geom_curve(aes(x=-2.1,y=10,xend=2.1,yend=10),color="gold",size=2)
                                                                         
                                                                        
