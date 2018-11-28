##########################################################################
##################### Techniki Wizualizacji Danych #######################
################ Kowalczyk Dawid, Kozak Anna #############################
#biblioteki
library(ggplot2)
library(dplyr)

#Wykres zależności dochodu i liczby nagród
df2 <- read.csv("../filmy_nagrody.csv")
names(df2)[8:9] <- c("Boxoffice", "Number of awards")

ggplot(df2, aes(x = Boxoffice, y = `Number of awards`)) +
  geom_text(label = df2$Polish.name, size = 12) +
  geom_point(size = 13) +
  xlab("Boxoffice in USD") +
  ylab("Liczba wygranych lub nominowanych nagród") +
  ggtitle("Wykres")

#Wykres podziału wypowiedzi bohaterów
df <- read.csv("../postaci_jak_czesto.txt", sep = "|", header = FALSE)
names(df) <- c("Postać", "Liczba wypowiedzi", "Płeć")
df$Postać <- factor(df$Postać, level = df$Postać[order(df$`Liczba wypowiedzi`)])

ggplot(df, aes(x = Postać, y = `Liczba wypowiedzi`, fill = Płeć)) + geom_bar(stat ="identity") + coord_flip()


#Wykres podziału wypowiedzi na płeć

df %>% select(V2, V3) %>% group_by(V3) %>% summarize(suma = sum(V2)) -> df1
df1$V3 <- ifelse(df1$V3 == "F", "Kobiety", "Mężczyźni")


wykres_jeden_slupek <- function(dane, title, legend.row = 1){
  tmp <- dane
  names(tmp) <- c("Płeć", "Freq")
  tmp$cum <- cumsum(tmp$Freq)
  colnames(tmp)[1] <- "Var1"
  ggplot(tmp, aes(x = 1, y = -Freq, fill = Var1)) +
    geom_col(width = 0.4)+
    geom_text(aes(y = cum -2750 - (Freq/2), label = paste0(round(Freq/sum(Freq)*100,2),"%")), size = 4) +
    ggtitle(title) +
    theme_bw() +
    theme(legend.position = "bottom",
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_blank(),
          legend.title = element_blank())+
    guides(fill = guide_legend(nrow = 1)) +
    scale_fill_manual(values = c("#e41a1c", "#377eb8"))+
    coord_flip()
  
}
wykres_jeden_slupek(df1,"Podział wypowiedzi podczas filmu względem płci")
