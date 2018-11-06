library(ggplot2)
Niepełnosprawni <- 1 - c(3, 9.2, 33.8) / 100
Ludność <- c(18.7, 64.4, 16.9) / 100

Ludność <- cumsum(Ludność)
Ludność <- c(0, Ludność)
Niepełnosprawni <- c(0, Niepełnosprawni)


Ludność
Niepełnosprawni2 <- c(4.1, 17.6) 
Ludność2 <- c(40, 24.4)


df1 <- data.frame(Niepełnosprawni, Ludność)
df2 <- data.frame(Niepełnosprawni2, Ludność2)

df1

ggplot(data = df1, aes(Ludność, Niepełnosprawni)) +
  geom_rect(aes(xmin = Ludność[1], xmax = Ludność[2], ymin = Niepełnosprawni[1], ymax = 1 ) )+ 
  geom_rect(aes(xmin = Ludność[2], xmax = Ludność[3], ymin = Niepełnosprawni[2], ymax = 1) ) + 
  geom_rect(aes(xmin = Ludność[3], xmax = Ludność[4], ymin = Niepełnosprawni[3], ymax = 1) ) 
  