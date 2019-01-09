library(extrafont)
font_import()
loadfonts()
fonts()
ChristmasTree <- read.csv("/Users/dasha/Documents/twd/tree.csv", header = TRUE,
                          sep = ";")
library(ggplot2)
n <- 500
bombki <- data.frame(x = c(6, 9, 15, 17, 5, 13, 16, 7, 10, 14, 7, 9, 11, 
                                   14, 8, 14, 9, 12, 11, 12, 14, 11, 17, 10))
bombki$y <- c(4, 5, 4, 4, 5, 5, 5, 6, 6, 6, 8, 8, 8, 8, 10,
                      10, 11, 11, 12, 13, 10, 16, 7, 14)
bombki$colour <- factor(c(1, 2, 2, 3, 2, 3, 1, 3, 1, 1, 1, 2, 1, 2,
                          3, 3, 2, 1, 3, 2, 1, 3, 3, 1))
bombki$size <- c(1, 3, 1, 1, 2, 1, 2, 2, 2, 1, 1, 1, 3, 3, 3,
                 2, 3, 1, 1, 2, 2, 3, 3, 2)
snow <- data.frame(x = sample(seq(1, 20), size = n, replace = TRUE), 
                   y = sample(seq(1, 20), size = n, replace = TRUE))
tree <- ggplot() + 
  geom_tile(data = ChristmasTree, aes(x, y, fill = colour)) +  
  scale_fill_identity() + 
  geom_point(data = bombki, aes(x, y, col = colour, size = size), shape = 16) +
  scale_colour_manual(values = c("red", "yellow", "blue")) +
  scale_size_area(max_size = 12) +
  geom_point(data = data.frame(x = 11, y = 18.75), aes(x, y), shape = 42, 
             size = 30,
             colour = "yellow") +
  geom_point(data = snow, aes(x, y), shape = 42, size = 10, colour = "white") +
  annotate("text", x = 11, y = 20, label = "Merry Christmas!", 
           family = "Comic Sans MS", size = 12, colour = "blue") +
  theme_void() +
  theme(panel.background = element_rect("lightblue"), legend.position = "none")
tree
