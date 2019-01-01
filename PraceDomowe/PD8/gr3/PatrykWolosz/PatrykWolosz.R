library(ggplot2)
library(dplyr)

n <- 10

x <- c()
y <- c()
id <- c()
index <- 1
decoration_radius <- 20
decoration_thick <- 0.01

# CREATING CHRISTMAS TREE COORDS

for(i in 0:(n-1)) {
  x[index] <- -(n-i+0.5)
  x[index + 1] <- -(n-i-1)
  x[index + 2] <- n-i-1
  x[index + 3] <- n-i+0.5
  x[index + 4] <- -(n-i+0.5)
  x[index + 5] <- n-i+0.5
  y[index] <- i-1
  y[index + 1] <- i
  y[index + 2] <- i
  y[index + 3] <- i-1
  y[index + 4] <- i-1
  y[index + 5] <- i-1
  id[index:(index+3)] <- i
  id[(index+4):(index+5)] <- (i+1)*100
  index <- index + 6
}

# CREATING DECORATIONS

x_dec_1 <- seq(-8,9,by = 0.2)
y_dec_1 <- (abs(x_dec_1-2)**2)/50-1
group_1 <- rep(1, length.out = length(x_dec_1))
x_dec_2 <- seq(-7.5,6.7,by = 0.2)
y_dec_2 <- (abs(x_dec_2+2)**2)/50+1
group_2 <- rep(2, length.out = length(x_dec_2))
x_dec_3 <- seq(-5,6,by = 0.2)
y_dec_3 <- (abs(x_dec_3-2)**2)/50+3
group_3 <- rep(3, length.out = length(x_dec_3))
x_dec_4 <- seq(-4.6,4.2,by = 0.2)
y_dec_4 <- (abs(x_dec_4+2)**2)/50+4.5
group_4 <- rep(4, length.out = length(x_dec_4))
x_dec_5 <- seq(-2.8,3,by = 0.2)
y_dec_5 <- (abs(x_dec_5-2)**2)/50+6
group_5 <- rep(5, length.out = length(x_dec_5))
x_dec_6 <- seq(-1.6,1.4,by = 0.2)
y_dec_6 <- (abs(x_dec_6+2)**2)/50+7.5
group_6 <- rep(6, length.out = length(x_dec_6))


points <- data.frame(x = x, y = y, id = id)
bottom_points <- data.frame(x = c(-1,-1,1,1,-1,1), y = c(-3,-1,-1,-3,-3,-3), id = c(rep(1000,4),1001,1001))
decoration_points <- data.frame(x = c(x_dec_1, x_dec_2, x_dec_3, x_dec_4, x_dec_5, x_dec_6), 
                                y = c(y_dec_1, y_dec_2, y_dec_3, y_dec_4, y_dec_5, y_dec_6))


plot_limits_x <- c()
plot_limits_y <- c()
plot_limits_id <- c()
plot_limits_x[1] <- min(points$x) - 3
plot_limits_x[2] <- min(points$x) - 3
plot_limits_x[3] <- max(points$x) + 3
plot_limits_x[4] <- max(points$x) + 3
plot_limits_x[5] <- min(points$x) - 3
plot_limits_x[6] <- max(points$x) + 3

plot_limits_y[1] <- min(bottom_points$y)
plot_limits_y[2] <- max(points$y) + 2
plot_limits_y[3] <- max(points$y) + 2
plot_limits_y[4] <- min(bottom_points$y)
plot_limits_y[5] <- min(bottom_points$y)
plot_limits_y[6] <- min(bottom_points$y)

plot_limits_id[1:4] <- 1
plot_limits_id[5:6] <- 2

plot_limits <- data.frame(x = plot_limits_x, y = plot_limits_y, id = plot_limits_id)

# DRAW TREE

p <- ggplot() +
  geom_polygon(data = plot_limits, aes(x = x, y = y, group = id), inherit.aes = FALSE, fill = "black", alpha = 0.2) +
  geom_line(data = plot_limits, aes(x = x, y = y, group = id), inherit.aes = FALSE, size = 1) +
  geom_polygon(data = points, aes(x = x, y = y, group = id), fill = "green") +
  geom_line(data = points, aes(x = x, y = y, group = id),size = 1) +
  geom_polygon(data = bottom_points, aes(x = x, y = y, group = id),size = 1, inherit.aes = FALSE, fill = "brown") +
  geom_line(data = bottom_points, aes(x = x, y = y, group = id),size = 1, inherit.aes = FALSE) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(), 
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank()) 



x_p <- c(7,-2,-9,-4,3,-1.5,6,0.5,-5,3.5,1.3)
y_p <- c(-0.1,-0.3,-0.8,1.5,0.6,5.5,2.7,2.2,3.3,4.7,6.8)
single_decorations_points <- data.frame(x = x_p, y = y_p)

for(i in seq(1,30, by = 0.2)) {
   p <- p + geom_point(data = single_decorations_points, aes(x = x, y = y), colour = "red", size = i, shape = 16, alpha = 0.0025) 
}

p <- p + geom_point(data = single_decorations_points, aes(x = x, y = y), shape = 21, stroke = 1, colour = "black", size = 7, fill = "red") 
p <- p + geom_point(data = decoration_points, aes(x = x, y = y), shape = 8, size = 4, color = "yellow")


star_point_x <- 0
star_point_y <- 9
x_tmp <- c(2,1,0,2,0,0,2,0,2,0.5,0,2,1,0,2,0)
y_tmp <- c(1.5,0,0,1.5,1,0,1.5,0,-1.5,-1,0,-1.5,0,0,-1.5,0)
id_tmp <- c(1,1,1,2,2,2,3,3,4,4,4,5,5,5,6,6)
star_points <- data.frame(x = star_point_x-c(x_tmp,-x_tmp,0,-2,0.5,0,-0.5,0.5,0,-0.5,0,0)/1.5,
                          y = star_point_y-c(y_tmp,y_tmp,1,1.5,-1,-2.5,-1,-1,0,-1,-2.5,0)/2,
                          id = c(id_tmp,-id_tmp,7,7,8,8,8,9,9,9,10,10))

p <- p +
  geom_polygon(data = star_points, aes(x = x, y = y, group = id), fill = "yellow") +
  geom_line(data = star_points, aes(x = x, y = y, group = id),size=0.75) 

p
