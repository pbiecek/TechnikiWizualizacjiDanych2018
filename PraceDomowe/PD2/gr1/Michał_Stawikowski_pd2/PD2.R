library(tidyverse)
library(ggplot2)
data <- read.csv("PD2Data")
colorss <- rep(c("a","b","c","d"), 5)
ggplot(data, aes(data$Skoczek, data$Skok, label = paste0(data$Punkty, " pkt."))) +
  geom_segment( aes(x=reorder(data$Skoczek, data$Punkty), xend=data$Skoczek, y=data$Skok, yend=120, color=colorss)) +
  geom_point( aes(x=data$Skoczek, y=data$Skok), color="black", size=2 )+
  geom_label(color = "gray31", size = 3, nudge_y = 3) +
  scale_color_brewer(type="div", palette = "Spectral")+
  scale_y_continuous(breaks = seq(120, 150, by = 5), limits = c(120,150)) +
  coord_flip()+
  theme_light()+
  labs(title = "Klasyfikacja skoczków - Trening w Klingethal - 2.10.2018") +
  theme(axis.title.x = element_text(size = 9),
        axis.title.y = element_text(size = 9),
    legend.position = "none",
    panel.border = element_blank(),
    axis.text.y = element_text(size = 8),
    plot.title = element_text(size = 15, colour = "gray25")) +
    xlab("Skoczkowie oraz ilość zdobytych punktów") + ylab("Długość skoku w metrach")
  