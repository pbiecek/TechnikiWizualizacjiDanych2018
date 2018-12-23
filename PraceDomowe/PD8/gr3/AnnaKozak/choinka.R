### wczytanie danych
w1 <- read.csv("warstwa1.csv")
w2 <- read.csv("warstwa2.csv")
w3 <- read.csv("warstwa3.csv")
w4 <- read.csv("warstwa4.csv")
w5 <- read.csv("warstwa5.csv")
w_all <- read.csv("warstwy_all.csv")

### zamiana na factor
w1$color <- factor(w1$color)
w2$color <- factor(w2$color)
w3$color <- factor(w3$color)
w4$color <- factor(w4$color)
w5$color <- factor(w5$color)
w_all$color <- factor(w_all$color, levels= 0:9)

### korzen
korzen <- data.frame(x = runif(10000, 1.7, 2.071542), y = runif(10000, -0.3, 0.1))

### snieg - podstawa
sniezek <-data.frame(x = runif(549, 0, 3.75), y = runif(549, -0.8, -0.2))
sniezek$size <- rep(c(1:3), 183)

### bombki
w_all[sample(nrow(w_all), 45), 1:2] -> bombki_czerwone
w_all[sample(nrow(w_all), 45), 1:2] -> bombki_niebieskie
w_all[sample(nrow(w_all), 45), 1:2] -> bombki_fioletowe
w_all[sample(nrow(w_all), 200), 1:2] -> swiatelka

### gwiazda
gwiazda <- data.frame(x = w_all[w_all$y == range(w_all$y)[2], 1:2][1], y = w_all[w_all$y == range(w_all$y)[2], 1:2][2])


### Generowanie sniegu - https://paulvanderlaken.com/2018/12/17/animated-snow-in-r-2-0-gganimate-api-update/ ###
library(here)
library(tidyverse)
library(gganimate)
library(animation)


map_to_range <- function(x, from, to) {
  x <- x - min(x)
  x <- x / max(x)
  x <- x * (to - from)
  x + from
}

N <- 500 
TIMES <- 100
XPOS_DELTA <- 0.01
YSPEED_MIN = 0.005
YSPEED_MAX = 0.03
FLAKE_SIZE_COINFLIP = 5
FLAKE_SIZE_COINFLIP_PROB = 0.1
FLAKE_SIZE_MIN = 4
FLAKE_SIZE_MAX = 20


set.seed(1)

size <- runif(N) + rbinom(N, FLAKE_SIZE_COINFLIP, FLAKE_SIZE_COINFLIP_PROB) # random flake size
yspeed <- map_to_range(size, YSPEED_MIN, YSPEED_MAX)


xpos <- rep(NA, N * TIMES)
ypos <- rep(NA, N * TIMES)


for(i in seq(TIMES)){
  if(i == 1){
    xpos[1:N] <- runif(N, min = 0, max = 3.75)
    ypos[1:N] <- runif(N, min = -0.5, max = 3.5)
  } else {
    first_obs <- (N * i - N + 1)
    last_obs <- (N * i)
    xpos[first_obs:last_obs] <- xpos[(first_obs-N):(last_obs-N)] - runif(N, min = -XPOS_DELTA, max = XPOS_DELTA)
    ypos[first_obs:last_obs] <- ypos[(first_obs-N):(last_obs-N)] - yspeed
    xpos <- ifelse(ypos < -0.1, runif(N), xpos) # restart at random x
    ypos <- ifelse(ypos < -0.1, 1.1, ypos) # restart just above top
  }
}



snieg <- cbind.data.frame(ID = rep(1:N, TIMES)
                          ,x = xpos
                          ,y = ypos 
                          ,s = size
                          ,t = rep(1:TIMES, each = N)) 

### CHOINKA

library(ggplot2)
ggplot(w_all, aes(x = x, y = y, col = color)) + 
  geom_point(data = korzen, aes(x = x, y = y), col = "#543005") + 
  geom_point(data = sniezek, aes(x = x, y = y, size = size), pch = 42, col = "white") + 
  geom_point(data = w1, aes(x = x,y = y, col = color)) +
  geom_point(data = w2, aes(x = x,y = y, col = color)) + 
  geom_point(data = w3, aes(x = x,y = y, col = color)) +  
  geom_point(data = w4, aes(x = x,y = y, col = color)) + 
  geom_point(data = w5, aes(x = x,y = y, col = color)) + 
  scale_color_manual(values = c("#002b11", "#00451b", "#005e25", "#00441b", "#238443" , "#006837", "#00782f","#009239", "#00ac43", "#16c600")) +
  geom_point(data = bombki_czerwone, aes(x = x,y = y), col = "red", size = 4) +
  geom_point(data = bombki_niebieskie, aes(x = x,y = y), col = "blue", size = 4) +
  geom_point(data = bombki_fioletowe, aes(x = x,y = y), col = "violet", size = 4) +
  geom_point(data = swiatelka, aes(x = x, y = y), col = "yellow", size = 1) + 
  geom_point(data = gwiazda, aes(x = x, y = y) , shape = 42, size = 25, fill = 3, col = "gold") +
  theme_dark() + 
  theme(legend.position = "none") +
  ylim(c(-0.8, 2.15)) +  
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        legend.title = element_blank()) + geom_point(data = snieg, aes(x, y, size = s, alpha = s), color = "white", pch = 42) +
  scale_size_continuous(range = c(FLAKE_SIZE_MIN, FLAKE_SIZE_MAX)) +
  scale_alpha_continuous(range = c(0.2, 0.8)) +
  transition_time(t) +
  ease_aes('linear') -> snow_plot


snow_anim <- animate(snow_plot, nframes = TIMES, width = 400, height = 700)






