library(ggplot2)
library(data.table)
library(gganimate)
library(gifski)
set.seed(123)
n <- 5000
m <- 10
punkty_normalne <- function(n,przes,szer,eps=0.05) {
  x <- rnorm(n,0,1)
  y <- c(runif(length(x[x<0]),przes,przes+pnorm(x[x<0])),
         runif(length(x[x>=0]),przes,przes+pnorm(-x[x>=0])))
  x <- c(x[x<0],x[x>=0])
  df <- data.frame(x=x*szer,y=y)
  return(df[y>przes+eps,])
}
pien_x <- runif(2000,-1,1)
pien_y <- runif(2000,1,2)
snieg_x <- runif(2000,-1,1)
snieg_y <- runif(2000,0.2,2.1)
snieg_iter <- sample(1:10,2000,replace=TRUE)
snieg <- data.frame(x=snieg_x,y=snieg_y,iter=snieg_iter)
a <- ggplot(data=data.frame(x=pien_x/10,y=pien_y/7),aes(x,y)) +
  geom_point(data=snieg,mapping=aes(x,y),color='white',shape=8,size=1) +
  geom_point(size=5,color='brown')
for (i in 2:5) {
  a <- a + 
    geom_point(data=punkty_normalne(n,i/2.7-0.5,1/i),mapping=aes(x=x,y=y),
               colour='green',size=3)
}
punkty <- punkty_normalne(0,0,0)
points <- data.frame(x=rep(0,0),y=rep(0,0),iter=rep(0,0))
for (j in 1:10) {
  for (i in 2:5) {
    this <- punkty_normalne(9*m/i,i/2.7-0.5,1/i)
    punkty <- rbind(punkty,this) 
    # cbind(this,data.frame(partia=rep(j,nrow(this)))
  }
  points <- rbind(points,data.frame(x=punkty$x,y=punkty$y,iter=rep(j,nrow(punkty))))
  punkty <- punkty_normalne(0,0,0)
}

points <- data.table(cbind(points,data.frame(col=sample(1:5,nrow(points),replace=TRUE))))

palette(c('yellow1','turquoise2','red2','orange','navy'))

b <- a + geom_point(data=points,mapping=aes(x=x,y=y),
                      color=points$col,size=3) +
  geom_point(aes(x=x,y=y),data.frame(x=rep(0,10),y=rep(1.85,10),iter=1:10),color='yellow1',shape=42,size=40) +
  transition_manual(iter) +
  theme_void() + theme(panel.background = element_rect(fill='darkblue')) 
tree <- animate(b)

save_animation(tree,'tree.gif')

#install.packages('gifski')
#devtools::install_github("gganimate", username="thomasp85")
#devtools::install_github("thomasp85/gganimate")
#install.packages('magick')
