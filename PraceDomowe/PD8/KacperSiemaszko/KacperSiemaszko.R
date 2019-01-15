library(ggplot2)
library(ggridges)
library(ggimage)
library(dplyr)

draw.christmas.tree <- function(height) {
  christmas.tree <- data.frame("value" = c(unlist(sapply(1:height, function(x) rnorm(x*100000, 0, (height - 2*x/3)/height)))),
             "group" = c(rep(1:height, (1:height)*100000)))
  p <- ggplot(data = christmas.tree, aes(x = value, y = group, group = group, fill = group)) + 
    geom_density_ridges(rel_min_height = 0.02, scale = 3, fill = "green", size = 0.2) +
    scale_color_brewer(palette = "Set2") +
    geom_point(data = data.frame("x" = c(runif(4*(height %/% 2 + height %% 2), -1.7, 1.7), runif(4*(height %/% 2), -1, 1)), "y" = rep(1:height, each = 4), "color" = as.character(rep(1:4,height))), aes(x = x, y = y, color = color), size = 10, inherit.aes = FALSE) + 
    theme_void() +
    theme(legend.position = "none")
  
  img = "https://images.theconversation.com/files/202446/original/file-20180118-158519-186b12q.jpg?ixlib=rb-1.1.0&q=45&auto=format&w=1356&h=668&fit=crop"
  ggbackground(p, img)
}

draw.christmas.tree(6)
