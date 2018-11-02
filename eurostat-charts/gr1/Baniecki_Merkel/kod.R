library(eurostat)
library(dplyr)
library(ggplot2)
library(tidyr)
library(RColorBrewer)
kolory <- brewer.pal(9,"Set1")[c(1,6,3)]
data <- get_eurostat(id = "sdg_11_40", time_format = "num")
data <- data[data$unit=="RT",]
data <- data[,2:4]
data <- data[order(data$geo),]
data <- data[data$geo!="EU28",]
data <- data %>% group_by(geo) %>% summarise(mean(values),min(values),max(values))

colnames(data) <- c("Country", "mean", "min", "max")
kraje <- c("Austria", "Belgia", "Bułgaria", "Cypr", "Czechy", "Niemcy","Dania","Estonia","Grecja",
           "Hiszpania","Finlandia","Francja","Chorwacja","Węgry","Irlandia",
           "Włochy","Litwa","Luksemburg","Łotwa","Malta","Holandia","Polska","Portugalia","Rumunia",
           "Szwecja","Słowenia","Słowacja","Wielka Brytania")
data$Country<-kraje
data$max <- data$max-data$mean
data$mean <- data$mean-data$min
data <- data %>% gather(Statystyki,values,-Country)
data
ggplot(data, aes(x=Country, y=values,fill=Statystyki)) + 
   geom_col(position="stack") +
  labs(title="Stosunek zgonów dla poszczególnych krajów",
       subtitle="od 2001 do 2016 roku", 
       x="Kraj", y="Stosunek na 100 000 mieszkańców") + coord_flip() +
  scale_y_continuous(limits = c(0,25), breaks=seq(0,30,5), expand=c(0.01,0)) +
  theme_dark() +
  theme(axis.ticks.y = element_blank(), panel.grid = element_line(colour = "gray65", size = 0.4),
        legend.position="bottom", axis.text.y = element_text(colour = "black", size = 11),
        axis.title.y = element_blank(), axis.text.x = element_text(colour = "black", size = 11)) +
  scale_fill_manual(name="Statystyka",breaks=c("min", "mean", "max"),
                    values=kolory, labels=c("MIN", "MEAN", "MAX"))

