library(ggplot2)
library(ggforce)
library(dplyr)
sample_lights <- function(n) {
  y = runif(n, 0.2, 1.8)
  group=sample(4:7, n, replace = T)
  x=c()
  for (i in y) {
    m = (2-i)*0.55
    x = c(x, runif(1, -m, m))
  }
  list(x=x,y=y,group=group)
}
triangles = data.frame(group = c(1,1,1), x = c(-1.5,1.5,0), y = c(0,0,1))
star = data.frame(group=rep(2, 10), x=sin((0:9)*pi/5)*rep(c(1/5,1/10), 5), y=cos((0:9)*pi/5)*rep(c(1/5,1/10), 5)+2.1)
wood = data.frame(group = rep(3,4), x = c(-0.2, 0.2, 0.2, -0.2), y = c(0, 0, -0.3, -0.3))
lights = sample_lights(20)
circles = data.frame(y = lights$y, x = lights$x, r = rep(0.05, 10), group=lights$group)
ggplot() + 
  geom_polygon(data=triangles, aes(x=x, y=y, group=group, fill=as.factor(group))) +
  geom_polygon(data=mutate(triangles, y=y+0.5, x=x*0.8), aes(x=x, y=y, group=group, fill=as.factor(group))) +
  geom_polygon(data=mutate(triangles, y=y+1, x=x*0.64), aes(x=x, y=y, group=group, fill=as.factor(group))) +
  geom_polygon(data=mutate(triangles, y=c(1.5,1.5,2.2), x=x*0.512), aes(x=x, y=y, group=group, fill=as.factor(group))) +
  geom_polygon(data=star, aes(x=x, y=y, group=group, fill=as.factor(group))) +
  geom_polygon(data=wood, aes(x=x, y=y, group=group, fill=as.factor(group))) +
  geom_circle(aes(x0=x, y0=y, r=r, fill=as.factor(group)), data=circles) +
  coord_equal(ratio=1) +
  theme_void() +
  scale_fill_manual(values=c('#1b542b', '#FFD700', '#694c34', '#FF0000', '#FFFF00', '#0000FF', '#00FF00')) +
  guides(fill=FALSE)

