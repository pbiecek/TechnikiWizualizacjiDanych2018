library(ggplot2)
library(gganimate)

library(caTools)

N = 1000

beg <- data.frame(x = rep(0,N), 
                  y = rep(100, N), 
                  size = rep(1, N), 
                  time = rep(0, N), 
                  momentum = abs(rnorm(N, 0.9, 1)) + 0.2, 
                  ordinal = 1:N)


mkstep <- function(dat){
  dat$time <- dat$time +1
  y_step <- abs(rnorm(N,1,1))
  dat$y = dat$y - ifelse(2 * y_step * dat$time < 10,  2 * y_step * dat$time, 10)
  size_step <- (abs(rnorm(N, 0, 0.5)) %% (y_step* dat $time))
  dat$size = ((100 - dat$y)/9 + 1) * rnorm(N, 1, 0.3) 
  dat$x <- dat$x + rnorm(N, 1) %% (y_step* dat$time) * sample(c(-1,1), N, replace = TRUE)
  dat
}

ggplot(data = mkstep(mkstep(mkstep(beg))), aes(x = x, y = y, size = size)) +
  geom_point() +
  coord_fixed()

con <- mkstep(beg)
xmastree <- rbind(beg, con, mkstep(con))

x <- ggplot(xmastree, aes(x = x, y = y, size = size)) + 
  geom_point() +
  transition_states(
    time,
    transition_length = 2,
    state_length = 1
  ) +
  enter_fade() + 
  exit_shrink() +
  ease_aes('sine-in-out')

x <- ggplot(xmastree, aes(x = x, y = y, size = size)) + 
  geom_point() +
  transition_time(time) +
  ease_aes('linear')

y <- animate(x, renderer = magick_renderer())

magick::image_write(y, path="xmasxmasxmas.gif")
dev.off()
