library(ggplot2)
library(ggrepel)
library(RColorBrewer)
library(ggimage)
library(extrafont)
library(scales)

# DO STWORZENIA WYKRESU TRZEBA POBRAĆ I WGRAĆ CZCIONKĘ DO CZCIONEK NA KOMPUTERZE

windowsFonts(k = windowsFont("Star Jedi Outline"))
loadfonts()
windowsFonts(sans="Star Jedi Outline")
loadfonts(device="win")
loadfonts(device="postscript")

KOLORY <- brewer.pal(9,"Set1")
dane <- read.csv("dane2.csv")


p <- ggplot(dane, aes(dane$population, y = dane$X, label = dane$names, color=dane$climat))+
  geom_point( stat='identity', size=(dane$diameter)/7.5e2)+
  geom_point( stat='identity', size=(dane$diameter)/17e2, color= "white")+
  geom_point( stat='identity', size=(dane$diameter)/21e2, color = "yellow")+
  geom_point( stat='identity', size=(dane$diameter)/25e2, color = "orange")+
  geom_point( stat='identity', size=(dane$diameter)/29e2, color = "red")+
  geom_label_repel(color="black", size=rel(7), force=2, nudge_y = 0.5, segment.color = NA)+
  scale_color_brewer(type="div", palette = "Accent")+
  scale_y_continuous(breaks = 0)+
  scale_x_log10(labels = comma)+
  labs(x="Population", y="", color = "Climate")+
  guides(colour = guide_legend(override.aes = list(size=10)))+
  theme(panel.grid.minor = element_line(colour="grey20", size=0.5),
        panel.grid = element_line(colour = "darkgray", size = 0.4),text = element_text(size = rel(5.5), colour = "yellow",family="Star Jedi Outline"),
                          legend.text = element_text(colour = "white", family = "Star Jedi Outline", size = rel(5)),
                          axis.text.y = element_text(colour = "white",
                                                     family = "Star Jedi Outline", size = rel(4.5)),
                          axis.text.x = element_text(colour = "white",family = "Star Jedi Outline",size = rel(4.5)))


img = "https://upload.wikimedia.org/wikipedia/commons/thumb/a/a7/Flag_of_Afghanistan_%281880%E2%80%931901%29.svg/900px-Flag_of_Afghanistan_%281880%E2%80%931901%29.svg.png"
p <- ggbackground(p, img) 
p

# panel.background=element_rect(fill="transparent",colour=NA),
# plot.background=element_rect(fill="transparent",colour=NA),
# legend.key = element_rect(fill = "transparent", colour = "transparent")  
#ggsave(p, filename = "tr_tst2.png",  bg = "transparent", width = 45, height = 25, units = "cm")
  