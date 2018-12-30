
library(ggplot2)
library(dplyr)
library(gganimate)
library(gifski)

{
  # drzewko
  max_width <- 10
  n <- 25
  s <- 8
  widths1 <- c(seq(max_width, 1, by = -0.5), 0.5, 0.25)
  widths2 <- c(widths1[3:length(widths1)], 0.5)
  widths <- c(widths1, widths2)
  y <- c(seq(from = 1, length.out = length(widths1), by = 2),
         seq(from = 2, length.out = length(widths2), by = 2))
  y <- y[-length(y)]
  lev <- list()
  for(i in 1:length(widths)) {
    lev[[i]] <- runif(n = n * widths[i], min = -widths[i], max = widths[i])
  }
  df <- data.frame(x = lev[[1]], y = rep(y[1], n * widths[1]))
  for(i in 2:length(widths)) {
    tmp <- data.frame(x = lev[[i]], y = rep(y[i], n * widths[i]))
    df <- rbind(df, tmp)
  }
  df <- df[df$y != 41, ]
  colors_green <- c('#74c476', '#41ab5d', '#238b45', '#006d2c', '#00441b')
  df$col <- sample(x = colors_green, size = nrow(df), replace = TRUE)
  df$size <- s
  
  # pien
  colors_brown <- c('#993404', '#662506', '#662506')
  tmp <- data.frame(x = runif(n = n * 30, min = -0.5, max = 0.5),
                    y = runif(n = n * 30, min = -4, max = 1.5),
                    col = sample(x = colors_brown, size = n * 30, replace = TRUE),
                    size = rep(s * 0.7, n * 30))
  df <- rbind(tmp, df)
  df$sh <- 24
  t <- rep(1:3, each = nrow(df))
  df <- rbind(df, df, df)
  df$time <- t
  
  # bombki
  ind <- sample(x = which(df$size == s), size = 20 * 3, replace = FALSE)
  colors_bombki <- c("#92FFF9", "#FF8C97")
  tmp <- df[ind, 1:2]
  tmp$size <- s * 0.9
  tmp$col <- sample(x = colors_bombki, size = nrow(tmp), replace = TRUE)
  tmp$sh <- 19
  tmp$time <- rep(1:3, each = nrow(tmp) / 3)
  df <- rbind(df, tmp)
  df <- na.omit(df)
}

# rysunek
choinka <- ggplot(data = df) +
  xlim(-max_width*1.3-10, max_width*1.3+10) +
  ylim(min(y)-3, max(y)+3) +
  geom_point(aes(x = x, y = y), shape = df$sh, fill = df$col, color =df$col, size = df$size) +
  geom_point(aes(x = 0, y = 41), shape = 24, fill = '#fec44f', color = '#fec44f', size = 12) +
  geom_point(aes(x = 0, y = 41), shape = 25, fill = '#fec44f', color = '#fec44f', size = 12) +
  theme_void()
choinka

# animacja gif
choinka_gif <- choinka +
  transition_states(
    time,
    transition_length = 0,
    state_length = 0.0005
  )

animate(choinka_gif, nframes = 20, fps = 30, width = 1000, height = 1200,
        renderer = gifski_renderer('choinka_dynamiczna.gif'))

