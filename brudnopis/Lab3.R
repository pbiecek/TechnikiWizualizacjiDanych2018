library(ggplot2)
library(reshape)

load("box_office.rda")

box_office_long <- melt(box_office, id = "tytul", measure = c("przychod_weekendowy_w_USA", 
                                                              "laczny_przychod_w_USA", 
                                                              "laczny_przychod_na_swiecie"))
box_office_long <- box_office_long[order(box_office_long$variable, decreasing = TRUE), ]
ord <- order(box_office$przychod_weekendowy_w_USA, decreasing = TRUE)

ggplot(data = box_office_long, aes(x = reorder(tytul, rep(ord, 3)), fill = variable, y = value)) +
  geom_col(position = position_identity(), 
           width = (as.integer(factor(box_office_long$variable))+2)/6, 
           col="black") +
  scale_fill_discrete(name="Rodzaj przychodu", 
                      labels=c("Weekendowy w USA", 
                               "Laczny w USA", 
                               "Laczny na swiecie")) + 
  theme_light() +
  theme(legend.position="bottom") +
  xlab("Tytul filmu") +
  ylab("Przychod (w milionach USD)") +
  coord_flip()


ggplot(data = box_office_long, aes(x = reorder(tytul, rep(ord, 3)), fill = variable, y = value)) +
  layer(geom = "bar", stat = "identity", position = position_identity(), 
        params = list(width = (as.integer(factor(box_office_long$variable))+2)/6, col="black")) +
  scale_fill_discrete(name="Rodzaj przychodu", 
                      labels=c("Weekendowy w USA", 
                               "Laczny w USA", 
                               "Laczny na swiecie")) + 
  theme_light() +
  theme(legend.position="bottom") +
  xlab("Tytul filmu") +
  ylab("Przychod (w milionach USD)") +
  coord_flip()



percent <- c(0.46, 0.063, 0.426, 0.051, 0.256, 0.094, 0.599, 0.051)
val <- rep(c(32679.6, 5223.6), each =4)
dat <- percent * val / 1000
age <- rep(c("15 +", "15 - 24"), each = 4)
type <- rep(c("pracujacy", "bezrobotni", "bierni zawodowo", "nieustalony status"), times = 2)

x <- data.frame(age, type, dat)

ord <- order(x[age == "15 +",]$dat, decreasing = TRUE)
ggplot(data = x, aes(x = reorder(type, rep(ord,2)), y = dat, fill = age)) +
  geom_bar(stat = "identity", position = position_identity()) +
  theme_light() + 
  xlab("Aktywnosc zawodowa") +
  ylab("Liczba osób (w milionach)") +
  coord_flip() 
  
















