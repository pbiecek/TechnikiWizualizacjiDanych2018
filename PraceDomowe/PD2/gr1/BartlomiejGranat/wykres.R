library(ggplot2)
library(ggthemes)
#tworzenie potrzebnego data.fram'u z p³acami minimalnymi
minimum_brutto <- c(700, 760, 760, 800, 824, 849, 899.1, 936, 1126, 1276, 1317, 1386, 1500, 1600, 1680, 1750, 1850, 2000, 2100, 2250)
lata <- c(2000:2019)
dane_min <- as.data.frame(cbind(lata, minimum_brutto))

#tworzenie potrzebnego data.fram'u z p³acami minimalnymi w Uni
unia_brutto <- c(1999, 1563, 1552, 1532, 1498, 1480, 1397, 1192, 845, 826, 791,736,684,650,470, 453, 433, 412, 407, 380, 380, 318, 235)
unia_brutto_df <- data.frame(unia_brutto)
#tworzenie wykresów

#wykres o minimalnych zarobkach w Polsce
wykres_minumum_polska <- ggplot(data = dane_min, aes(x = lata, y = minimum_brutto, fill = minimum_brutto)) + geom_bar(stat = "identity") +
  geom_text(data = subset(dane_min, minimum_brutto == 700 | minimum_brutto == 2250 | minimum_brutto == 1126 | minimum_brutto == 1276 | minimum_brutto == 936 |minimum_brutto == 1850 | minimum_brutto == 2000 ),aes(label = minimum_brutto), size = 3, vjust = 1, color = "white") +
  ylab("P³aca minimalna brutto(w z³.)") + xlab("Rok") + 
  ggtitle("Wykres wysoko¶ci p³acy minimalnej Polaków na przestrzeni lat") +
  theme_dark() +
  theme(plot.title = element_text(hjust = 0.5))
wykres_minumum_polska
#wykres o minimalnych zarobkach w Unii

wykres_tlo <- ggplot(data = unia_brutto_df, aes(x = "", y = unia_brutto)) +
  geom_boxplot(fill="#56B4E9") +
  theme_dark() + 
  geom_hline(yintercept = 453, linetype="dashed", color = "red", size = 2) +
  annotate("text", label = "p³aca minimalna w PL", x = 0.8, y = 500, size = 7, color = "red") +
  geom_point(position = position_jitter(width = 0.05), alpha = 0.4, color = "white", size = 2) + 
  ylab("placa minimalna(w EUR)") +
  xlab("") +
  ggtitle("P³aca minimalna w Polsce na tle pozosta³ych krajów Unii") +
  theme(plot.title = element_text(hjust = 0.5))

wykres_tlo
