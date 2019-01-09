x1 <- seq(0,1002,2)
x2 <- seq(1,1003,2)
y1 <- rep(0,502)
y2 <- c(seq(0,150,1),rep(150,200),seq(150,0,-1))

bombki1x <- c(60,125,270,340,500,520,540,670,750,850,920)
bombki1y <- c(10,30,70,50,40,110,75,40,75,40,15)
kolor1 <- floor(runif(11)*10)

bombki <- data.frame(bombki1x,bombki1y,kolor1)


x1b <- seq(150,850,2)
x2b <- seq(151,851,2)
y1b <- rep(0,351)
y2b <- c(seq(0,150,150/105),rep(150,139),seq(150,0,-150/105))


bombki2x <- c(220,324,364,460,522,623,650,800)
bombki2y <- c(20,40,70,50,120,75,40,15)
kolor2 <- floor(runif(8)*10)

bombki2 <- data.frame(bombki2x,bombki2y,kolor2)

x1c <- seq(250,750,2)
x2c <- seq(251,751,2)
y1c <- rep(0,251)
y2c <- c(seq(0,150,150/124), seq(150,0,-150/125))

bombki3x <- c(344,401,425,500,580,680)
bombki3y <- c(35,70,40,100,65,15)
kolor3 <- floor(runif(6)*10)

bombki3 <- data.frame(bombki3x,bombki3y,kolor3)



library(ggiraph)
library(ggplot2)
library(gridExtra)
library(ggpubr)
library(RColorBrewer)
margin = theme(plot.margin = unit(c(-0.06,-0.06,-0.06,-0.06),"cm"))
p1 <- ggplot() + geom_rect_interactive(aes(xmin=x1,xmax=x2,ymin=y1,ymax=y2,fill=x1)) +
  geom_point_interactive(data = bombki, aes(tooltip = "Wesolych!",x=bombki1x, y=bombki1y,data_id=bombki1x,color=kolor1), size = 10) +
  scale_fill_gradient(low="darkgreen",high="green")+
  scale_x_continuous(limits = c(0,1002))+
  scale_y_continuous(expand = c(0,0))+
  scale_color_gradient(low="yellow",high="red") +
  theme_minimal() + theme(
  axis.text = element_blank(), axis.title = element_blank(), panel.grid = element_blank(), legend.position = "null"
) + margin

p2 <- ggplot() + geom_rect_interactive(aes(xmin=x1b,xmax=x2b,ymin=y1b,ymax=y2b,fill=x1b))+ scale_x_continuous(limits = c(0,1002)) +
  scale_fill_gradient(low="darkgreen",high="green")+
  geom_point_interactive(data = bombki2, aes(tooltip = "Wesolych!",x=bombki2x, y=bombki2y,data_id=bombki2x,color=kolor2), size = 10) +
  scale_y_continuous(expand = c(0,0)) + 
  scale_color_gradient(low="orange",high="red") +
  theme_minimal() + theme(
    axis.text = element_blank(), axis.title = element_blank(), panel.grid = element_blank(), legend.position = "null"
  ) + margin

p3 <- ggplot() + geom_rect_interactive(aes(xmin=x1c,xmax=x2c,ymin=y1c,ymax=y2c,fill=x1c))+
  geom_point_interactive(data = bombki3, aes(tooltip = "Wesolych!",x=bombki3x, y=bombki3y,data_id=bombki3x,color=kolor3), size = 10) +
  scale_x_continuous(limits = c(0,1002)) +
  scale_fill_gradient(low="darkgreen",high="green")+
  scale_y_continuous(expand = c(0,0))  + theme_minimal()+
  scale_color_gradient(low="orange",high="red") +
  theme(
  axis.text = element_blank(),axis.title = element_blank(), panel.grid = element_blank(), legend.position = "null"
) + margin

x <- girafe(code = grid.arrange(p3,p2,p1, ncol = 1))
x
