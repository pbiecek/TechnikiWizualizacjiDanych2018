library(ggplot2)
ggplot()+geom_polygon(aes(x=c(-1,1,1,-1),y=c(0,0,3,3)))+geom_polygon(aes(x=c(0,0,4),y=c(20,1,1)),fill="green")+coord_equal()+
  geom_polygon(aes(x=-c(0,0,4),y=c(20,1,1)),fill="green")+theme_void()+geom_point(aes(x=c(-2,-1,0,1),y=c(2,7,15,3)),color=c(1,1,2,1),shape=c(1,2,3,4))+
  theme(panel.background = element_rect(fill = "blue"))+geom_curve(aes(x=-2,y=3,xend=1,yend=6),color="gold")
                                                                         
                                                                        
