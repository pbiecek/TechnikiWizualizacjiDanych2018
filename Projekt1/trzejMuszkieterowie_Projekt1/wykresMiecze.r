library(ggplot2)
library(ggimage)
library(extrafont)

# DO STWORZENIA WYKRESU TRZEBA POBRAĆ I WGRAĆ CZCIONKĘ DO CZCIONEK NA KOMPUTERZE

windowsFonts(k = windowsFont("starjout"))
loadfonts()
windowsFonts(sans="starjout")
loadfonts(device="win")
loadfonts(device="postscript")

colors <- c("Blue", "Red", "Green", "Yellow", "Purple", "Violet", "White", "orange", "others")
numbers <- c(40,38,27,7,6,4,1,1,4)
numbers <- numbers +5
data <- data.frame(colors,numbers)
order <- c(1,6,2,9,5,6,8,3,4)
data$colors <- factor(data$colors, levels = rev(data$colors))


p <- ggplot(data, aes(data,x=colors,y=numbers)) + 
  geom_segment(aes(x=data$colors, xend = data$colors,
                   y=rep(5, 9), yend = 0), color = "gray", size = 15) +
  geom_segment(aes(x=data$colors, xend = data$colors,
                   y=data$numbers+0.5, yend = 5), color = c("blue", "red", "green", "yellow",
                                                            "purple", "violet", "white","orange", "gray"), size = 20, alpha = 0.1) +
  geom_segment(aes(x=data$colors, xend = data$colors,
        y=data$numbers, yend = 0), color = c("blue", "red", "green", "yellow",
        "purple", "violet", "white","orange", "gray"), size = 11, alpha = 0.65) + 
  geom_segment(aes(x=data$colors, xend = data$colors,
                   y=data$numbers-0.5, yend = 0), color = "white", size = 8, alpha = 0.9) +
  geom_segment(aes(x=data$colors, xend = data$colors,
                   y=data$numbers+0.2, yend = 0), color = c("blue", "red", "green", "yellow",
                                                        "purple", "violet", "white","orange", "gray"), size = 15, alpha = 0.2) +
  geom_segment(aes(x=data$colors, xend = data$colors,
                   y=data$numbers+0.6, yend = 5), color = c("blue", "red", "green", "yellow",
                                                             "purple", "violet", "white","orange", "gray"), size = 13, alpha = 0.1) +
  geom_segment(aes(x=data$colors, xend = data$colors,
                   y=data$numbers-0.5, yend = 0), color = "white", size = 6, alpha = 0.8) +
  coord_flip()+xlab("") + ylab("") +
  scale_y_continuous(breaks = seq(5,45,5),labels = c("0","5","10","15","20","25","30","35","40")) +
  geom_segment(aes(x=data$colors, xend = data$colors,
                   y=rep(5, 9), yend = 0), color = "gray17", size = 14) +
  geom_segment(aes(x=data$colors, xend = data$colors,
                   y=rep(5, 9), yend = 1), color = "gray", size = 5) +
  geom_segment(aes(x=data$colors, xend = data$colors,
                   y=rep(5, 9), yend = 4.5), color = "gray", size = 16) +
  geom_segment(aes(x=data$colors, xend = data$colors,
                   y=rep(-2, 9), yend = 0), color = "gray", size = 12) +
  geom_segment(aes(x=data$colors, xend = data$colors,
                   y=rep(-1.5, 9), yend = -1), color = "black", size = 12) +
  geom_segment(aes(x=data$colors, xend = data$colors,
                   y=data$numbers+0.15, yend = 5), color = c("blue", "red", "green", "yellow",
                                                            "purple", "violet", "white","orange", "gray"), size = 20, alpha = 0.1) +
  
  theme_dark() + theme(panel.grid.minor = element_line(colour="grey20", size=0.5),
                         panel.grid = element_line(colour = "darkgray", size = 0.4),
                       text = element_text(size = rel(5.5), colour = "yellow",family="Star Jedi Outline"),
                          axis.text.y = element_text(colour = "white",
                                                     family = "Star Jedi Outline", size = rel(4.5)),
                       axis.text.x = element_text(colour = "white",family = "Star Jedi Outline",size = rel(4.5)))


img = "https://upload.wikimedia.org/wikipedia/commons/thumb/a/a7/Flag_of_Afghanistan_%281880%E2%80%931901%29.svg/900px-Flag_of_Afghanistan_%281880%E2%80%931901%29.svg.png"
p <- ggbackground(p, img)
p

# panel.background = element_rect(fill = "transparent") # bg of the panel
# , plot.background = element_rect(fill = "transparent", color = NA), 
# ggsave("plot.png",p)
# ggsave(p, filename = "tr_tst2.png",  bg = "transparent")  
  

