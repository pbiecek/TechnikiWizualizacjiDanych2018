library(ggplot2)

data <- data.frame(liczebnosc = c(113, 108, 86),
                   date = c("2018-05-16 12:27 CET", "2018-05-16 12:56 CET", "2018-05-16 13:41 CET"))

ggplot(data = data, aes(x = date, y = liczebnosc, fill = "")) + 
  geom_bar(stat = "identity") + 
  geom_text(aes(label = liczebnosc), nudge_y = -5, size = 5) + 
  scale_fill_manual(values = "goldenrod") + 
  xlab(label = "Czas") + 
  ylab(label = "Obecnych") + 
  theme(legend.position = "none") + 
  ggtitle("Obecność studentów w trakcie kolokwium")

ggsave('obecnosc.png')
