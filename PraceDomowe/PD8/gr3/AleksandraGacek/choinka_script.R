library(ggplot2)
library(gganimate)


n <- 200
speed <- 0.1
times <- 100
xstart <- runif(n, min =2, max = 20) 
ystart <- runif(n, max = 25.6) 


star_df <- data.frame(x=11, y=25.8)


x_ch_pos <- rep(NA, 10*4+3)

y_ch_pos <- rep(NA, 10*4+3)

x_ch_pos[1:8] <- c(13, 9, 9, 13, 19, 3, 4, 18)
y_ch_pos[1:8] <- c(0, 0, 3, 3, 3, 3, 5, 5)

for(i in seq(8, 36, 4)) {
  x_ch_pos[(i+1):(i+4)] <- x_ch_pos[(i-3):i] + c(-0.5, 0.5, 0.5, -0.5)
  y_ch_pos[(i+1):(i+4)] <- y_ch_pos[(i-3):i] + c(2, 2, 2, 2)
}

x_ch_pos[(10*4+1):(10*4+3)] <- c(14.5, 7.5, 11)
y_ch_pos[(10*4+1):(10*4+3)] <- c(21, 21, 26.5)

positions <- data.frame(
  id = c(rep(c('1', '2', '3', '4', '5', '6', '7', '8', '9', '10'), each=4), c('12', '12', '12')),
  x = x_ch_pos,
  y = y_ch_pos,
  colo = c(rep('chocolate4', 4), rep('#143306', 39))
)

Desired.Lights <- n
total_Lights <- sum(round(Desired.Lights * 0.35) + round(Desired.Lights * 0.20) + 
                      round(Desired.Lights * 0.17) + round(Desired.Lights * 0.13) +
                      round(Desired.Lights * 0.10) + round(Desired.Lights * 0.05))

xpos <- rep(NA, n * times)

ypos <- rep(NA, n * times)
Lights.X <- rep(NA, n * times)
Lights.Y <- rep(NA, n * times)
Lights.Colour <- rep(NA, n * times)


xspeed <- seq(-0.02, 0.02, length.out = 100)
yspeed <- runif(n, min = 0.05, max = 0.25)


for(i in seq(times)){
  if(i == 1){

        xpos[1:n] <- xstart
    ypos[1:n] <- ystart
    Lights.X[1:n] = c(round(runif(round(Desired.Lights * 0.35), 4, 18), 0),
                 round(runif(round(Desired.Lights * 0.20), 5, 17), 0),
                 round(runif(round(Desired.Lights * 0.17), 6, 16), 0),
                 round(runif(round(Desired.Lights * 0.13), 7, 15), 0),
                 round(runif(round(Desired.Lights * 0.10), 8, 14), 0),
                 round(runif(round(Desired.Lights * 0.05), 10, 12), 0))
    Lights.Y[1:n] <- c(round(runif(round(Desired.Lights * 0.35), 4, 6), 0),
                  round(runif(round(Desired.Lights * 0.20), 7, 8), 0),
                  round(runif(round(Desired.Lights * 0.17), 9, 10), 0),
                  round(runif(round(Desired.Lights * 0.13), 11, 12), 0),
                  round(runif(round(Desired.Lights * 0.10), 13, 14), 0),
                  round(runif(round(Desired.Lights * 0.05), 15, 17), 0))
    Lights.Colour[1:n] <- c(round(runif(total_Lights, 1, 4), 0))
  } else {
    first_obs <- (n*i - n + 1)
    last_obs <- (n*i)

    
    xpos[first_obs:last_obs] <- xpos[(first_obs-n):(last_obs-n)] - sample(xspeed, n, TRUE)

    ypos[first_obs:last_obs] <- ypos[(first_obs-n):(last_obs-n)] - yspeed

    xpos <- ifelse(ypos < -0.1, runif(n, min = 2, max = 20), xpos) 
    ypos <- ifelse(ypos < -0.1, 25.6, ypos)
    
    if(i %% 2 == 0) {
      Lights.X[first_obs:last_obs] <- Lights.X[(first_obs-n):(last_obs-n)]
      Lights.Y[first_obs:last_obs] <- Lights.Y[(first_obs-n):(last_obs-n)]
      Lights.Colour[first_obs:last_obs] <- Lights.Colour[(first_obs-n):(last_obs-n)]
    } else {
      Lights.X[first_obs:last_obs] = c(runif(round(Desired.Lights * 0.35), 4, 18),
                                       runif(round(Desired.Lights * 0.20), 5, 17),
                                       runif(round(Desired.Lights * 0.17), 6, 16),
                                       runif(round(Desired.Lights * 0.13), 7, 15), 
                                       runif(round(Desired.Lights * 0.10), 8, 14), 
                                       runif(round(Desired.Lights * 0.05), 10, 12))
      Lights.Y[first_obs:last_obs] <- c(runif(round(Desired.Lights * 0.35), 3, 6),
                                        runif(round(Desired.Lights * 0.20), 6, 10), 
                                        runif(round(Desired.Lights * 0.17), 10, 13), 
                                        runif(round(Desired.Lights * 0.13), 13, 17), 
                                        runif(round(Desired.Lights * 0.10), 17, 20), 
                                        runif(round(Desired.Lights * 0.05), 20, 24))
      Lights.Colour[first_obs:last_obs] <- c(round(runif(total_Lights, 1, 4), 0))
    }
    
  }
}

lights_and_snow <- data.frame(x = xpos,  
                  y = ypos,  
                  Lights_X= Lights.X,
                  Lights_Y = Lights.Y,
                  Lights.Colour = Lights.Colour,
                  s = runif(n, min = 4, max = 20),
                  t = rep(1:times, each = n))

Baubles <- data.frame(Bauble.X = c(6, 9, 15, 17, 5, 13, 16, 7, 10, 14, 7, 9, 11, 
                                   14, 8, 14, 9, 12, 11, 12, 14, 11, 17, 10, 6, 11, 7.5, 14, 7, 15, 9, 12, 10, 15, 12, 9))
Baubles$Bauble.Y <- c(4, 5, 4, 4, 5, 5, 5, 6, 6, 6, 8, 8, 8, 8, 10,
                      10, 11, 11, 12, 13, 10, 16, 7, 14, 10, 21, 16, 18, 12.5, 12.6, 18, 22, 23, 15, 18, 20)
Baubles$Bauble.Colour <- factor(c(1, 2, 2, 3, 2, 3, 1, 3, 1, 1, 1, 2, 1, 2,
                                  3, 3, 2, 1, 3, 2, 1, 3, 3, 1, 1,2, 2, 1, 1, 3, 3, 3, 1, 1, 2,1))
Baubles$Bauble.Size <- c(1, 3, 1, 1, 2, 1, 2, 2, 2, 1, 1, 1, 3, 3, 3,
                         2, 3, 1, 1, 2, 2, 3, 3, 2, 2, 2, 3, 3, 2, 1, 2, 2,1,2, 2, 1)

Baubles$Bauble.Size <- Baubles$Bauble.Size*3


x <- ggplot() + 
  geom_polygon(data=positions,aes(x = x, y = y, group = id, fill=colo))+
  scale_size_area(max_size = 12) +
  geom_point(data=lights_and_snow,aes(Lights_X,Lights_Y, alpha = Lights.Colour),
             colour = "red", shape = 16) +
  geom_point(data = Baubles, aes(x = Bauble.X, y = Bauble.Y, 
                                 colour = Bauble.Colour, size = Bauble.Size), shape = 19)+       
  geom_point(data=star_df, aes(x=x, y=y, size=12), color='yellow', size=12, pch=8, stroke=4, inherit.aes = FALSE) +
  scale_colour_manual(values = c("turquoise1", "springgreen1", "hotpink1")) +
  geom_point(data=lights_and_snow, aes(x, y, size = s), color = "white", pch = 42) + 
  transition_states(
    t
  )  +
  theme_void() + 
  scale_x_continuous(breaks = NULL) + 
  scale_y_continuous(breaks = NULL) +
  theme(panel.background = element_rect("steelblue2"), legend.position = "none") +
  scale_size_identity() +
  scale_fill_identity() +       
  xlim(2, 21) +
  ylim(0, 26.5) +
  labs(x = "", y = "")
  
animate(x)
