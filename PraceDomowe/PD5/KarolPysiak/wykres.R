library(ggplot2)



dolar <- read.csv("Desktop/TWD1/PD5/moneypl-1542460480436.csv")
head(dolar)
d1 <- ggplot(data = dolar, aes(x = dolar$Data, y=dolar$Kurs.średni, group=1)) +
  geom_point() +
  geom_line() +
  theme(axis.text.x = element_text(angle = 90)) +
  ylim(c(0, max(dolar$Kurs.średni))) +
  labs(x="Data", y="Średni kurs") +
  ggtitle("Wykres kursu dolara")
d2 <- ggplot(data = dolar, aes(x = dolar$Data, y=dolar$Kurs.średni, group=1)) +
  geom_point() +
  geom_line() +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(x="Data", y="Średni kurs") +
  ggtitle("Wykres kursu dolara")
cairo_ps("Desktop/TWD1/PD5/dolar_plaski.eps", height = 5, width = 6)
d1
dev.off()

cairo_ps("Desktop/TWD1/PD5/dolar_normalny.eps", height = 5, width = 6)
d2
dev.off()

dolar

funt <- read.csv("Desktop/TWD1/PD5/moneypl-1542462511012.csv")
head(funt)
f1 <- ggplot(data = funt, aes(x = funt$Data, y=funt$Kurs.średni, group=1)) +
  geom_point() +
  geom_line() +
  theme(axis.text.x = element_text(angle = 90)) +
  ylim(c(0, max(funt$Kurs.średni))) +
  labs(x="Data", y="Średni kurs") +
  ggtitle("Wykres kursu funta")
f2 <- ggplot(data = funt, aes(x = funt$Data, y=funt$Kurs.średni, group=1)) +
  geom_point() +
  geom_line() +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(x="Data", y="Średni kurs") +
  ggtitle("Wykres kursu funta")

cairo_ps("Desktop/TWD1/PD5/funt_plaski.eps", height = 5, width = 6)
f1
dev.off()

cairo_ps("Desktop/TWD1/PD5/funt_normalny.eps", height = 5, width = 6)
f2
dev.off()

funt
