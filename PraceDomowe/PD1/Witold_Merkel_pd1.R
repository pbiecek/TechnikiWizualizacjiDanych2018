library(ggplot2)

#tworzenie potrzebnego data.fram'u z płacami minimalnymi
minimum_brutto <- c(700, 760, 760, 800, 824, 849, 899.1, 936, 1126, 1276, 1317, 1386, 1500, 1600, 1680, 1750, 1850, 2000, 2100, 2250)
minimum_netto <- c(512.83, 559.12, 561.82, 588.39, 602.77, 617.39, 647.93, 686.45, 845.17, 954.96, 984.15, 1032.34, 1111.86, 1181.38, 1237.20, 1286.16, 1355.69, 1459.48, 1530, 1633.78)
lata <- c(2000:2019)

dane_minimum_prim <- cbind(minimum_brutto, minimum_netto)
dane_minimum <- as.data.frame(cbind(lata, dane_minimum_prim))

#tworzenie potrzebnego data.fram'u z płacami minimalnymi w Uni
unia_brutto <- c(1999, 1563, 1552, 1532, 1498, 1480, 845, 826, 684, 453, 407, 380)
kraje <- c("Luksemburg", "Irlandia", "Holandia", "Belgia", "Niemcy", "Francja", "Srednia w UE", "Hiszpania", "Gracja", "Polska", "Czechy", "Litwa")
kraje_minimum <- as.data.frame(cbind(unia_brutto, kraje))
kraje_minimum

#tworzenie wykresów

#wykres o minimalnych zarobkach w Polsce
wykres_minumum_polska <- ggplot(data = dane_minimum, aes(x = lata, y = minimum_brutto)) + geom_col(fill = lata) +
  geom_text(aes(label = minimum_brutto), size = 4, vjust = 2, color = "white") +
  ylab("Zarobki minimalne w złotówkach brutto") + xlab("Lata") + 
  ggtitle("Wykres zarobków Polaków na przestrzeni ostatnich lat") +
  theme(plot.title = element_text(hjust = 0.5))

#wykres o minimalnych zarobkach w Uni

wykres_minumum_unia_1 <- boxplot(unia_brutto, main = "Rozklad minimalnych zarobkow \n w Uni Europejskiej",
                                 ylab = "Zarobki w Euro")
wykres_minimum_unia_2 <- abline(h = 453, col = "red", lwd = 4, lty = 5)

