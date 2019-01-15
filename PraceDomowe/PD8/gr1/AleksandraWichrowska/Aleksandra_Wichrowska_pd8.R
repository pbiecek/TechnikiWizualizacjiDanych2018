library(ggplot2)

# generowanie losowych punktów

x1 <- runif(100000, -1, 1)
y1 <- runif(100000, -1, 1.6)

# równanie na serce
which1 <- x1^2+(y1-(x1^2)^(1/4))^2<=1
x1 <- x1[which1]
y1 <- y1[which1]

# drugi poziom
x2 <- 1.5*x1 -1
y2 <- 1.5*y1 

# trzeci poziom
x3 <- 2*x1 -2
y3 <- 2*y1


dt <- data.frame(c(x1,x2,x3),c(y1,y2,y3))
colnames(dt) <- c("x", "y")
dt$y <- dt$y - 0.25
dt <- dt[dt$x>-2 & dt$x<1 & dt$y>-2.25 & dt$y<0,]
dt <- data.frame(c(dt$x,dt$x), c(dt$y, -dt$y))
colnames(dt) <- c("x", "y")

#pieniek

p1 <- runif(1000, -0.5, 0.5)
p2 <- runif(1000, -2.5, -2)

pieniek <- data.frame(p1,p2)

colors <- c('green', 'green2', 'green4', 'darkgreen')
colors2 <- rep(colors,length.out=dim(dt)[1])

bombki <- data.frame(c(-1.8, -1.7, -1.1, -0.6, 0, 0.3), c(-1,0.7,-0.9,0.1,-0.3,0.3))
colnames(bombki) <- c("x", "y")

lancuch <- data.frame(data.frame(x1 = -0.5, x2 = 1, y1 = 21.0, y2 = 15.0))

gwiazda <- data.frame(c(0), c(0.7))
colnames(gwiazda)<- c("x", "y")

ggplot(data=NULL) + geom_point(data=dt, aes(x = y, y=x), col=colors2) + 
  geom_point(data=bombki, aes(x=y, y=x), size=10, col='blue') +
  geom_point (data=pieniek, aes(x=p1, y=p2), col='brown') + 
  geom_point(data=gwiazda, aes(x=x, y=y),size=10,shape=24, col="yellow", fill='yellow') +
  geom_point(data=gwiazda, aes(x=x ,y=y),size=10,shape=25, col="yellow", fill='yellow') +
  geom_curve(aes(x=-1,xend=1.2,y=-0.65,yend=-1.2), col='red',size=2, curvature = 0.3) + 
  geom_curve(aes(x=1.2, y=-1.2, xend=-1.2, yend=-1.5), size=2, col="red", curvature = -0.3) +
  geom_curve(aes(x=-1,xend=0.8,y=-0.65,yend=-0.5), col='red',size=2, curvature = 0.3) +
  geom_curve(aes(x=-0.8,xend=0.8,y=-0.2,yend=-0.5), col='red',size=2, curvature = 0.2) +
  geom_curve(aes(x=-0.8,xend=0.9,y=-0.2,yend=0.15), col='red',size=2, curvature = 0.3) +
  geom_curve(aes(x=-0.65,xend=0.9,y=0.35,yend=0.15), col='red',size=2, curvature = 0.3) +
  theme(axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),legend.position="none",
        panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),plot.background=element_blank())
  
  