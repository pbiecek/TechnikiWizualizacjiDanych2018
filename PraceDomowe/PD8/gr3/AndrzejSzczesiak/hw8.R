library(ggplot2)

n <- 80
tree_height <- 4

x <- seq(1, 2 ^ tree_height)
y <- rep(0, 2 ^ tree_height)
for (height in 1:tree_height) {
  p <- tree_height - height + 1
  x <- c(rowMeans(cbind(x[seq(1, 2 ^ p, 2)], x[seq(2, 2 ^ p, 2)])), x)
  y <- c(rep(height, 2 ^ (p - 1)), y)
}
baubles <- data.frame(x, y)

branches <- data.frame()
for (i in seq(2 ^ (tree_height + 1) - 1, 2)) {
  direction <- unlist(baubles[i %/% 2,] - baubles[i,])
  rownames(direction) <- c()
  y <- runif(n * norm(direction, type = "2"))
  x <- baubles[i, "x"] + direction["x"] * y
  y <- baubles[i, "y"] + y
  branches <- rbind(branches, data.frame(x, y))
}

chain = data.frame()
i <- c(2 ^ tree_height, 2 ^ tree_height - 1)
while (i[2] > 1) {
  direction <- unlist(baubles[i[2],] - baubles[i[1],])
  y <- runif(n * norm(direction, type = "2") / 3)
  x <- baubles[i[1], "x"] + direction["x"] * y
  y <- baubles[i[1], "y"] + y
  chain <- rbind(chain, data.frame(x, y))
  i <- i %/% 2
}

ggplot() +
  geom_point(data = branches, aes(x = x, y = y), color = "darkgreen", shape = 8, size = 4) +
  geom_point(data = chain, aes(x = x, y = y), shape = 8, size = 6, color = "gold2") +
  geom_point(aes(x = 8.5, y = 4), shape = 24, fill = "gold", color = "gold", size = 10) +
  geom_point(aes(x = 8.5, y = 4), shape = 25, fill = "gold", color = "gold", size = 10) +
  geom_point(aes(x = 8.5, y = -0.3), shape = 15, color = "#80461B", size = 16) +
  geom_point(data = baubles[-1,], aes(x = x, y = y), 
             color = sample(c("lightcyan3", "deepskyblue2", "red", "purple1"), 
                            nrow(baubles) - 1, replace = TRUE), size = 8) +
  ylim(-0.5, 4.3) +
  theme_void()

ggsave("christmas-tree.png")