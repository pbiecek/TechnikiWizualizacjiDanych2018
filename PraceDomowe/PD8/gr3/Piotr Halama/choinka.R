library(ggplot2)
library(dplyr)

triangle <- function(n) {
  df <- data.frame(y=runif(n, 0, 1))
  df$x <- apply(df, 1, function(d) runif(1, d[1] - 1.001, 1.001 - d[1]))
  df
}

a_transform <- function(df, sx, sy, tx = 0, ty = 0) {
  df$x <- df$x * sx + tx
  df$y <- df$y * sy + ty
  return(df)
}

xmas_tree_green <- rbind(
  a_transform(triangle(5000), 9, 7, 0, 2),
  a_transform(triangle(3000), 6, 4, 0, 8),
  a_transform(triangle(2000), 4, 4, 0, 12)
)

xmas_tree_brown <- data.frame(x=runif(n=100, -1, 1), y=runif(1000, 0, 2))

decoration <- data.frame(y=runif(30, 2, 15))
decoration$x <- apply(decoration, 1, function(d){
  y <- d['y']
  samples <- xmas_tree_green[(xmas_tree_green$y < (y + 0.5)) & (xmas_tree_green$y > (y - 0.5)),]
  runif(1, min(samples$x), max(samples$x))
})
decoration$color <- factor(sample(1:3, size=30, replace = T))

ggplot(mapping = aes(x=x, y=y)) + 
  geom_point(data=xmas_tree_brown, color='brown') +
  geom_point(data=xmas_tree_green, color='#4d9e3a') +
  geom_point(data=decoration, aes(x=x, y=y, color=color), size=3) +
  scale_color_manual(values=c('red', 'orange', 'yellow')) +
  scale_x_continuous(limits=c(-10,10)) + scale_y_continuous(limits = c(0, 20)) + theme_void() +
  guides(color=F)

ggsave("Ugly Xmas tree.png")