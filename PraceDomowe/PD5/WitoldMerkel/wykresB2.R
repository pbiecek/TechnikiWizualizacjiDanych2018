library(ggplot2)
library(ggthemes)

kto <- c("Daniel Ek", "Martin Lorentzon", "Tencent", "Tiger Global", "Sony", "ITCM",
         "Northzone Ventures", "Sean Parker", "Felix Hagno", "Wellington Partners", "Creandum", "Ludvig Strigues",
         "Universal Music", "Warner Music", "EMI", "Daniel Ek Extra", "Shishir Mehrotra", "Meriln")

udzialy <- c(25.7, 13.2, 7.5, 6.9, 5.7, 5.4,
             6.7, 5, 4.9, 3.8, 3.5, 2.6,
             4.8, 3.8, 1.9, 1.9, 1.4, 1)

stan <- c("Potwierdzone", "Pogloski", "Szacowane")
stan_liczby <- c(64.4, 26.5, 9.1)

dane <- data.frame(kto = factor(kto, levels = rev(kto)), udzialy)
dane_stan <- data.frame(stan = factor(stan, levels = rev(stan)), stan_liczby)

wykres_calosc <- ggplot(data = dane_stan, aes(x = stan, y = stan_liczby)) +
  geom_col(fill = c("#ff3300","#3366ff","#00ff00")) +
  coord_flip()+ ylab("Procent udzialu") + xlab("Rodzaj informacji") +
  ggtitle("Rozklad akcji gieldowych spolki 'Spotify'") +
  theme(plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "black"),
        axis.text.x = element_text(colour = "black", size = 11),
        axis.title.y = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black", size = 9.5),
        panel.grid.major = element_line(colour = "gray20"),
        panel.grid.minor = element_line(colour = "gray20"),
        plot.margin = unit(c(3, 3, 1, 1), "lines")) +
  scale_y_continuous(limits = c(0, 70), breaks = seq(0, 70, 5)) +
  geom_text(aes(label = stan_liczby, size = 10), show.legend = FALSE,
            hjust = -0.3, color = "White")

wykres_calosc
