library(ggplot2)
library(ggthemes)

dane <- read.csv("dane1.csv")
dane <- dane[,c(2,3)]
colnames(dane) <- c("Odcinek", "Wyświetlenia")

wykres <- ggplot(data = dane, aes(x = Odcinek, y = Wyświetlenia)) + geom_point(color = "green") +
  geom_text(aes(label = Odcinek), color = "green", vjust = -1, size = 3.5) +
  geom_vline(xintercept = 11.5, lty = 5, color = "green") +
  geom_vline(xintercept = 21.5, lty = 5, color = "green") + 
  labs(x = "Episode number", y = "Viewers [millions]",
                      title = "What is wrong with trend line in season 3?") + 
  scale_x_continuous(limits = c(0, 32), breaks = seq(0, 30, 5), expand=c(0.01,0)) + 
  scale_y_continuous(limits = c(0, 3.5), breaks = seq(0, 3, 0.5), expand=c(0.01,0)) +
  theme(plot.background = element_rect(fill = "black"),
        panel.background = element_rect(fill = "black"),
        axis.title.x = element_text(colour = "green", size = 18),
        axis.text.x = element_text(colour = "green", size = 16),
        axis.title.y = element_text(colour = "green", size = 18),
        axis.text.y = element_text(colour = "green", size = 16),
        title = element_text(colour = "green", size = 22, family = "Calligraphr"),
        panel.grid.major = element_line(colour = "gray25"),
        panel.grid.minor = element_line(colour = "gray25"),
        plot.margin = unit(c(2, 1, 1, 1), "lines")) +
  annotate("text", x=5.5, y=3.2, label= "Season 1", color = "yellow", size = 9, family = "Get Schwifty") +
  annotate("text", x=16.5, y=3.2, label= "Season 2", color = "orange", size = 9, family = "Get Schwifty") +
  annotate("text", x=27.25, y=3.2, label= "Season 3", color = "red", size = 9, family = "Get Schwifty") +
  geom_smooth(data = dane[1:11, ], method = "lm", se = FALSE, color = "yellow") +
  geom_smooth(data = dane[12:21, ], method = "lm", se = FALSE, color = "orange") +
  geom_smooth(data = dane[22:31, ], method = "lm", se = FALSE, color = "red")

wykres
ggsave("wykres1_1.png", wykres)
