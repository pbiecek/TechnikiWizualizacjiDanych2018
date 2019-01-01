# pd 8
library(dplyr)
library(ggplot2)
library(ggpubr)
library(jpeg)
library(png)
library(grid)
library(gridGraphics)

n_samples <-  10000
pien <- data_frame(x = rnorm(n_samples,0,0.4), y = runif(n_samples,0,1))

dol1 <- data_frame(y = runif(n_samples, 1, 5 ))
dol1$x <- rnorm(n_samples, 0, 10 / dol1$y)
dol1$col <- ifelse(rbinom(n_samples, 1, 0.03) == 0, 'dark green', 'red')
dol1$alpha <- ifelse(dol1$col == 'dark green', 0.5, 1)


srodek <- data_frame(y = runif(n_samples, 5, 9 ))
srodek$x <- rnorm(n_samples, 0, 6 / (srodek$y - 4))
srodek$col <- ifelse(rbinom(n_samples, 1, 0.02) == 0, 'dark green', 'red')
srodek$alpha <- ifelse(srodek$col == 'dark green', 0.4, 1)


gora <- data_frame(y = runif(n_samples, 9, 12 ))
gora$x <- rnorm(n_samples, 0, 4 / (gora$y - 8) )
gora$col <- ifelse(rbinom(n_samples, 1, 0.01) == 0, 'dark green', 'red')
gora$alpha <- ifelse(gora$col == 'dark green', 0.3, 1)


img <- readJPEG("house.jpg")
text <- data_frame(x = 40, y = 14, label = 'Merry Christmas!')

img2 <- readPNG('christmas-ball.png')
g1 <- rasterGrob(img2, interpolate=FALSE)

star_pos <- 0.1
gwiazda <- data_frame(x = c(0, 4, -4), y = c(11,13,13))
gwiazda$y <- gwiazda$y + star_pos
gwiazda2 <- data_frame(x = c(0, 4, -4), y = c(14,12,12))
gwiazda2$y <- gwiazda2$y + star_pos

bombki <- data_frame(x = c(1, 10, 3, 5, -7, 0, -4), y = c(7, 1.5, 5, 3.5, 2, 10, 4))

ggplot(pien ,aes(x = x, y = y)) + 
  background_image(img) +
  geom_point(data = pien, color = 'brown', alpha = 0.3) + 
  geom_point(data = dol1, alpha = dol1$alpha, color = dol1$col, aes(x = x, y = y)) +
  geom_point(data = srodek, alpha = srodek$alpha, color = srodek$col, aes(x = x, y = y)) +
  geom_point(data = gora, alpha = gora$alpha, color = gora$col, aes(x = x, y = y)) +
  geom_polygon(data = gwiazda, fill = 'yellow', aes(x = x, y = y)) +
  geom_polygon(data = gwiazda2, fill = 'yellow', aes(x = x, y = y)) +
  geom_label(data = text, aes(x = x, y = y, label = label), size = 20, color = 'red') +
  geom_point(data = bombki, aes(x=x, y=y), fill = 'blue', color = 'blue',  pch = 24, size = 5) +
  annotation_custom(g1, xmin=1, xmax=3, ymin=1, ymax=3) +
  annotation_custom(g1, xmin=-1, xmax=1, ymin=5.5, ymax=7.5) +
  annotation_custom(g1, xmin=3, xmax=5, ymin=8, ymax=10) +
  annotation_custom(g1, xmin=-11, xmax=-9, ymin=0, ymax=2) +
  ylim(0, 15) + 
  xlim(-15, 60) 
