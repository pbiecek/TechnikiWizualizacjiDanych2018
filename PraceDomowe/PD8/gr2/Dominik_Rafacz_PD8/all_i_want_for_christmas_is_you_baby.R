library(ggplot2)
library(gganimate)
library(svglite)
library(dplyr)

HEIGTH <- 100

gen_data_v <- function(n_points, n_states) {
  
  
  id <- rep(1:n_points, each = n_states)
  time <- rep(1:n_states, times = n_points)
  
  velocity <- abs(rnorm(n_points, 1, 0.5))
  
  y1 <- runif(n_points, 0, HEIGTH)
  
  y <- as.vector(crossprod(t(0:(n_states-1)), -velocity) + 
                   matrix(rep(y1, n_states), byrow = TRUE, n_states, n_points))
  y <- y %% HEIGTH
  
  x1 <- matrix(runif(n_points, 0, HEIGTH), 1, n_points)
  for(i in 2:n_states) {
    x1 <- rbind(x1, x1[i-1,] + rnorm(n_points, 0, 0.4))
  }
  
  x <- (as.vector(x1) %% HEIGTH) - 50
  
  data.frame(id, x, y, time)
} 

dat <- gen_data_v(10000, 100)

x <- ggplot(data = dat, aes(x = x, y = y, color = id)) +
  geom_point(data = dat %>% filter(y < HEIGTH - 3*x & y < HEIGTH + 3 * x), size = 2) +
  geom_point(x=0, y=100, color = "#ffc20c", shape = 8, size = 10) +
  scale_color_gradient(low = "#addd8e", high = "#11673A", guide = FALSE) +
  theme_void() +
  coord_fixed() +
  transition_time(time) +
  ease_aes('linear') 

y <- animate(x, renderer = magick_renderer(),width = 600, height = 1000)
y

save_animation(y, "underneath_the_christmas_tree.gif")
