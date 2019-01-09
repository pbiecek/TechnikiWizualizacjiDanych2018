library(randomcoloR)
library(circlize)
#----------------------------------BOMBKI----------------------------------
factors = 1:8
circos.par(cell.padding = c(0, 0, 0, 0))
circos.initialize(factors, xlim = c(0, 1))
circos.trackPlotRegion(ylim = c(0, 1), track.height = 0.05, bg.col =randomColor(count = 8, hue = c(" ", "random", "red", "orange", "yellow","green", "blue", "purple", "pink", "monochrome"), luminosity = c(" ","random", "light", "bright", "dark")), bg.border = NA )

# add links
for(i in 1:20) {
  se = sample(1:8, 2)
  circos.link(se[1], runif(2), se[2], runif(2), col =randomColor(count = 1, hue = c(" ", "random", "red", "orange", "yellow",
                                                                                    "green", "blue", "purple", "pink", "monochrome"), luminosity = c(" ","random", "light", "bright", "dark")) )
}
circos.clear()
library(magick)
b1<-image_read2("C:/Users/Magda/Desktop/TWD/pd8/bombka1.svg", cut_empty_space = TRUE)
b2<-image_read2("C:/Users/Magda/Desktop/TWD/pd8/bombka2.svg", cut_empty_space = TRUE)
b3<-image_read2("C:/Users/Magda/Desktop/TWD/pd8/bombka3.svg", cut_empty_space = TRUE)
b4<-image_read2("C:/Users/Magda/Desktop/TWD/pd8/bombka4.svg", cut_empty_space = TRUE)
b5<-image_read2("C:/Users/Magda/Desktop/TWD/pd8/bombka5.svg", cut_empty_space = TRUE)
b6<-image_read2("C:/Users/Magda/Desktop/TWD/pd8/bombka6.svg", cut_empty_space = TRUE)

#-----------------------------CHOINKA--------------------------
dane=data.frame("indeks"=rep(1,2000), "wartosc"=rnorm(2000, 0, 1))
ggplot(data=dane, aes(dane$wartosc)) + 
  geom_histogram(breaks=seq(-2, 2, by = 0.2), 
                 col="green", 
                 fill="green", 
                 alpha = .2) + 
  theme_minimal() +
  theme(panel.border = element_blank(),
        panel.grid=element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank()
  )+
  annotation_raster(as.raster(b1), 0.05, 0.30, 1,20)+
  annotation_raster(as.raster(b2), -0.9, -0.45, 100,120)+
  annotation_raster(as.raster(b3), -1.6, -1.2, 11,30)+
  annotation_raster(as.raster(b4), -0.55, 0.0, 50,90)+
  annotation_raster(as.raster(b5), 1.75, 2, 1,20)+
  annotation_raster(as.raster(b6), 0.75, 1.25, 10,30)+
  annotation_raster(as.raster(b3), -0.55, -0.25, 1,20)+
  annotation_raster(as.raster(b5),0.45, 0.9, 100,120)+
  annotation_raster(as.raster(b4), 0.3, 0.75, 50,90)+
  annotation_raster(as.raster(b2), 1, 1.25, 40,60)+
  annotation_raster(as.raster(b1), -1.25, -0.75, 50,70)

