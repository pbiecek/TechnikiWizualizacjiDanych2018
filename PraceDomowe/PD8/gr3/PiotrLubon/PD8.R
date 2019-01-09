library(ggplot2)
sun <- data.frame(x=rnorm(1000,100, 40),y=rnorm(1000, 900, 40))
r <- data.frame(xmin=0, xmax=1000, ymin=0, ymax=50)
t <- data.frame(xmin=475, xmax=525, ymin=40, ymax=200)
t_x <- c()
t_y <- c()
for(y in 0:500)
{
  for(x in (y/2):(500-y/2))
  {
    t_x <- c(t_x, x)
    t_y <- c(t_y, y)
  }
}
tree <- data.frame(x=t_x, y=t_y)
tree$x <- tree$x + 250
tree$y <- tree$y + 200
tree2 <- data.frame(x=tree$x, y=tree$y)
tree2$y <- tree2$y + 250
tree2$x <- (tree2$x * 0.8) + 100
ggplot()+
  xlim(0,1000)+
  ylim(0,1000)+
  geom_point(aes(x=x, y=y), data = sun, colour='yellow')+
  geom_rect(aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill='white', data=r)+
  geom_rect(aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill='brown', data=t)+
  geom_point(aes(x=x,y=y,color=x), data=tree)+
  scale_color_gradient(limits=c(250,750), low='#00b300', high='black')+
  geom_point(aes(x=x,y=y,color=x), data=tree2)+
  ggtitle('Choinka ze światłocieniem')+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  guides(color=FALSE)
  