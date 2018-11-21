library(ggimage)
library(ggplot2)
library(RColorBrewer)
library(extrafont)

# BMI
# 25 > data$Weight/((data$Height/100)^2) > 18.5
# data$Weight = 25*((data$Height/100)^2)

# DO STWORZENIA WYKRESU TRZEBA POBRAĆ I WGRAĆ CZCIONKĘ DO CZCIONEK NA KOMPUTERZE

windowsFonts(k = windowsFont("starjout"))
loadfonts()
windowsFonts(sans="starjout")
loadfonts(device="win")
loadfonts(device="postscript")

kolory <- brewer.pal(9, "Set1")
ludzie <- brewer.pal(3, "Set2")[2]
reszta <- brewer.pal(3, "Set2")[3]

data <- read.csv("datav2.csv")
data <- data[,-1]
data$Weight <- as.numeric(as.character(data$Weight))
data$Height <- as.numeric(as.character(data$Height))
Jabba <- data[16,]
data <- data[-16,]
levels(data$Species) <- c(levels(data$Species), "other")
data$Species[data$Species!="Human" & data$Species!="Droid"] <- "other"


p <- ggplot(data, aes(x=Height, y=Weight)) +
      geom_point(data = data, aes(x=Height, y=Weight, color = Species), size = rel(5)) + 
      stat_function(fun = function(x) 25*((x/100)^2), color = kolory[6], size = 0.8) +
      stat_function(fun = function(x) 18.5*((x/100)^2), color = kolory[6], size = 0.8) +
      geom_rect(xmin = 170, xmax = 180, ymin = 170, ymax = 180, color = "white", fill = "transparent") +
      geom_point(data = Jabba, aes(x=Height, y=Weight-1183), size = rel(5.3), color = kolory[8]) +
      geom_text(aes(x=190, y=175, label="Jabba \n 1358kg"), fontface="bold", size = rel(5.3), color = kolory[8]) +
      geom_point(data = data[data$Name=="Yoda",], aes(x=Height, y=Weight), color = kolory[3], size = rel(5.3)) +
      geom_text(aes(x=66, y=38, label="Yoda \n 17kg"), fontface="bold", size = rel(5.3), color = kolory[3]) +
      xlim(50,225) + ylim(0,180) +
      scale_color_manual(values = c(reszta, ludzie, kolory[1])) + 
      theme(text = element_text(size = rel(5.5), colour = "yellow",family="Star Jedi Outline"),
            axis.text.y = element_text(colour = "white",family = "Star Jedi Outline", size = rel(4.5)),
            axis.text.x = element_text(colour = "white",family = "Star Jedi Outline",size = rel(4.5))) +
      labs(x = "height (cm)", y = "weight (kg)") +
  theme(panel.grid.minor = element_line(colour="grey20", size=0.5),
    panel.grid = element_line(colour = "darkgray", size = 0.4), legend.position = "bottom",
        legend.title = element_blank(), legend.text = element_text(size=rel(3), colour = "white")
        ) 


img = "https://upload.wikimedia.org/wikipedia/commons/thumb/a/a7/Flag_of_Afghanistan_%281880%E2%80%931901%29.svg/900px-Flag_of_Afghanistan_%281880%E2%80%931901%29.svg.png"
p <- ggbackground(p, img)
p

# panel.background = element_rect(fill = "transparent")
# , plot.background = element_rect(fill = "transparent", color = NA)
# , legend.background = element_rect(fill = "transparent", colour = "transparent")
# , legend.box.background = element_rect(fill = "transparent", colour = "transparent"),
#   legend.key = element_rect(fill = "transparent", colour = "transparent")
#ggsave("plot.png",p,  bg = "transparent")


