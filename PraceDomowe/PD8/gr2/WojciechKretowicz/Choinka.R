library(ggplot2)

generate_rectangle = function(x0, y0, x1, y1, n) {
  x <- runif(n, min=x0, max=x1)
  y <- runif(n, min=y0, max=y1)
  
  list(x = x,y = y)
}

generate_circle <- function(x,y,r,n) {
  R <- runif(n, min=0, max=r)
  theta <- runif(n, min=0, max=2*pi)
  list(x = R*cos(theta)+x, y = R*sin(theta)+y)
}

temp1 <- generate_rectangle(0,0,7,1,6000)
temp2 <- generate_rectangle(1,1,6,2,4000)
temp3 <- generate_rectangle(2,2,5,3,2000)
temp4 <- generate_rectangle(3,3,4,4,1000)

x <- c(temp1$x,temp2$x,temp3$x,temp4$x)
y <- c(temp1$y,temp2$y,temp3$y,temp4$y)

df <- data.frame(
  x = x,
  y = y
)

temp1 <- generate_circle(1,1,0.3,100)
temp2 <- generate_circle(3,2,0.3,100)
temp3 <- generate_circle(4,3,0.3,100)
temp4 <- generate_circle(5,1.5,0.3,100)
temp5 <- generate_circle(4,1,0.3,100)

x <- c(temp1$x,temp2$x,temp3$x,temp4$x,temp5$x)
y <- c(temp1$y,temp2$y,temp3$y,temp4$y,temp5$y)

df2 <- data.frame(
  x = x,
  y = y
)

temp <- generate_circle(1,4,0.5,100)

df3 <- data.frame(
  x = temp$x,
  y = temp$y
)

ggplot(data = df, aes(x=x,y=y)) + geom_density_2d(color='green') + 
  geom_density_2d(data = df2, aes(x=x,y=y), color='gold') +
  geom_density_2d(data = df3, aes(x=x,y=y), color='grey') +
  theme(panel.background = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank()) + ggtitle('Gesta choinka')
