library(ggplot2)
dane <- as.data.frame(cbind(c("Mężczyźni", "Mężczyźni", "Mężczyźni", "Mężczyźni", "Kobiety", "Kobiety",
                              "Kobiety","Kobiety"), 
                            c("przedprodukcyjny", "produkcyjny mobilny", "produkcyjny niemobilny",
                              "poprodukcyjny", "przedprodukcyjny", "produkcyjny mobilny", 
                              "produkcyjny niemobilny", "poprodukcyjny"),
                            c(0.2, 62.4, 35.6, 1.8, 0.1, 62.3, 33.6, 4.0)))

dane$V3 <- as.numeric(as.character(dane$V3))
ggplot(data=dane, aes(x=V1, y=V3, fill=V2, color=V2)) +
  geom_bar(stat="identity", width = 0.8, position = position_dodge(width = 0.9), color = "black")+
  geom_text(aes(label=V3), vjust=-1.0, 
            position = position_dodge(0.9), size=3.5)+
  ylim(0,max(dane$V3)+5)+
  scale_fill_brewer(palette="Set1")+ scale_color_brewer("", palette = "Set1")+
  labs(x="", y="%", fill = "")+
  ggtitle("Struktura pracujących według ekonomicznych grup, wieku i płci")+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))
