library(dplyr)
library(ggplot2)
library(svglite)
#Wczytanie danych
data <- data.frame(read.csv("../data/the-office-lines - scripts.csv", sep=",", header = TRUE))
#Obróbka danych
data <- data[data$deleted == FALSE, c("scene", "speaker")]
data <- data %>% count(speaker)
data <- data[order(data$n, decreasing = TRUE),]
data <- data[1:12,]
#Wygenerowanie wykresu
p <- ggplot(data = data, aes(x = factor(speaker, levels = data$speaker))) +
  geom_bar(aes(y = n), stat = "identity", fill = "midnightblue") +
  labs(x = "Bohater serialu", 
       y = "Liczba linii tekstu",
       title = "Najważniejsi bohaterowie") +
  theme(axis.text.x = element_text(family = "cambria", color="azure4", size=14, angle = 40), 
        axis.text.y = element_text(family = "cambria", color="azure4", size = 14),
        axis.title.x = element_text(family = "cambria", size = 16),
        axis.title.y = element_text(family = "cambria", size = 16),
        plot.title = element_text(family = "cambria", size = 18),
        panel.background = element_rect(fill = "transparent"), 
        plot.background = element_rect(fill = "transparent", color = NA))

#Zapisanie wykresu
#ggsave("lines_share.svg", bg = "transparent", p)
