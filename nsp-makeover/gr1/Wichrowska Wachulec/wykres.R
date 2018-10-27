install.packages("ggplot2")
library(ggplot2)
library(data.table)

miasto <- data.table(read.csv("Miasto.csv"))
wies <- data.table(read.csv("Wieś.csv"))

miasto[,3] <- miasto[,3]/miasto[,2]*100
miasto[,4] <- miasto[,4]/miasto[,2]*100
miasto[,5] <- miasto[,5]/miasto[,2]*100
miasto[,6] <- miasto[,6]/miasto[,2]*100


wies[,3] <- wies[,3]/wies[,2] *100
wies[,4] <- wies[,4]/wies[,2]*100
wies[,5] <- wies[,5]/wies[,2]*100
wies[,6] <- wies[,6]/wies[,2]*100

dt <- data.table()
typ <- c("panny","kawalerowie", "zamezne", "zonaci", "wdowy", "wdowcy", "rozwiedzione","rozwiedzeni")
proc_miasto <- c(miasto[2,3],miasto[1,3],miasto[2,4],miasto[1,4],miasto[2,5],miasto[1,5],miasto[2,6],miasto[1,6])
proc_wies <- c(wies[2,3],wies[1,3],wies[2,4],wies[1,4],wies[2,5],wies[1,5],wies[2,6],wies[1,6])
color<- rep(c("red","blue"),4)

dt <- data.frame(typ,miasto = unlist(proc_miasto),wies = unlist(proc_wies),color)
dt$typ <- factor(dt$typ,levels = rev(dt$typ))
ggplot(dt,aes(y=typ,x=seq(0,70,10))) +
  geom_point(aes(x=dt$miasto,y=dt$typ),shape=1, col=color) +
  geom_point(aes(x=wies,y=typ),shape=4, col=color) +
  geom_segment(aes(x=miasto, xend= wies, y=typ, yend = typ), col=color)+
  scale_x_continuous("Procent") + 
  scale_y_discrete("Stan cywilny") +
  legend()

# title("Stan cywilny ludnosci powyżej 15 lat w miastach i na wsi według płci(w %)")


       