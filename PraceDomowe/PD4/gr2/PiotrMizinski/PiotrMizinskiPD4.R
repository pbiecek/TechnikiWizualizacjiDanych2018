library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)
library(dplyr)
library(gridExtra)
# library(geofacet)

dane <- read.csv("./dane.csv", sep = ";")
dane$NAME <- sapply(dane$NAME, tolower)
dane$Top.Candy.pounds <- dane$Top.Candy.pounds/1000

states <- map_data("state")

tabelka <- dane[,c("State","NAME")]
tabelka <- tabelka[order(tabelka$State),]
tabelka1 <- tabelka[1:10,]
tabelka2 <- tabelka[11:20,]
tabelka3 <- tabelka[21:30,]
tabelka4 <- tabelka[31:40,]
tabelka5 <- tabelka[41:51,]

tt <- ttheme_default(colhead=list(fg_params = list(parse=TRUE)))

tbl <- tableGrob(tabelka, rows=NULL, theme=tt)
tbl1 <- tableGrob(tabelka1, rows=NULL, theme=tt)
tbl2 <- tableGrob(tabelka2, rows=NULL, theme=tt)
tbl3 <- tableGrob(tabelka3, rows=NULL, theme=tt)
tbl4 <- tableGrob(tabelka4, rows=NULL, theme=tt)
tbl5 <- tableGrob(tabelka5, rows=NULL, theme=tt)

  b <- left_join(states, dane, by = c("region" = "NAME"))
  
  c <- b %>% group_by(State) %>% 
    summarise(long = mean(long),
              lat = mean(lat))
  
  mapaColor <- ggplot(data = b) + 
    geom_polygon(aes(x = long, y = lat, fill = Top.Candy.pounds, group = group), color = "white") + 
    coord_map() +
    scale_fill_distiller(type = "div", palette = 9,name="Waga cukierków \n w tysiącach funtów")+
    geom_text(data = c, aes(x = long, y = lat, label = State), inherit.aes = FALSE,size=5,check_overlap=TRUE) +
    theme(axis.title.x = element_blank(),        
          axis.title.y = element_blank(),
          plot.title = element_text(hjust = 0.5))+
    ggtitle("Liczba funtów najpopularniejszych cukierków zakupionych w poszczególnych stanach podczas Haloween.")
  
  mapaSzara <- ggplot(data = b) + 
    geom_polygon(aes(x = long, y = lat, fill = Top.Candy.pounds, group = group), color = "white") + 
    coord_map() +
    scale_fill_distiller(type = "div", palette = "Greys",name="Waga cukierków \n w tysiącach funtów")+
    geom_text(data = c, aes(x = long, y = lat, label = State), inherit.aes = FALSE,size=5,check_overlap=TRUE) +
    theme(axis.title.x = element_blank(),        
          axis.title.y = element_blank(),
          plot.title = element_text(hjust = 0.5))+
    ggtitle("Liczba funtów najpopularniejszych cukierków zakupionych w poszczególnych stanach podczas Haloween.")
  
  lay <- rbind(c(1,2,3,4,5),
               c(6,6,6,6,6),
               c(7,7,7,7,7))
  grid.arrange( tbl1,tbl2,tbl3,tbl4,tbl5,mapaColor,mapaSzara, layout_matrix = lay)
  

# facet_geo(~ State)
