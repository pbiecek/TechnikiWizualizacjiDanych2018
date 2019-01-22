library(ggplot2)
library(gganimate)
library(dplyr)

f1 <- function(x,y){
  x1 = -0.67*x - 0.02*y
  y1 = -0.18*x + 0.81*y + 10
  c(x1,y1)
}

f2 <- function(x,y){
  x1 = 0.4*x + 0.4*y
  y1 = -0.1*x + 0.4*y
  c(x1,y1)
}

f3 <- function(x,y){
  x1 = -0.4*x - 0.4*y
  y1 = -0.1*x + 0.4*y
  c(x1,y1)
}

f4 <- function(x,y){
  x1 = -0.1*x
  y1 = 0.44*x + 0.44*y - 2
  c(x1,y1)
}

tworzenieDanych <- function(n){
  x <- 0
  y <- 0
  
  for (i in 2:n){
    los = runif(1)
    if (los<=0.32){
      x[i] = f1(x[i-1],y[i-1])[1]
      y[i] = f1(x[i-1],y[i-1])[2]
    }
    else if (los<=0.64){
      x[i] = f2(x[i-1],y[i-1])[1]
      y[i] = f2(x[i-1],y[i-1])[2] 
    }
    
    else if (los<=0.96){
      x[i] = f3(x[i-1],y[i-1])[1]
      y[i] = f3(x[i-1],y[i-1])[2]  
    } 
    
    else{
      x[i] = f4(x[i-1],y[i-1])[1]
      y[i] = f4(x[i-1],y[i-1])[2] 
    }
    
  }
  
  l <- sample(1:10, n, replace=TRUE,prob = c(10:4,0.1,0.025,0.025))
  z <- data.frame(x,y,l)
  
  #swiatelka
  s <- z[z$l==8,]
  
  s$l <- 12
  z<-bind_rows(z,s)
  s$l <- 13
  z<-bind_rows(z,s)
  s$l <- 14
  z<-bind_rows(z,s)
  s$l <- 15
  z<-bind_rows(z,s)
  s$l <- 16
  z<-bind_rows(z,s)
  s$l <- 17
  z<-bind_rows(z,s)
  s$l <- 18
  z<-bind_rows(z,s)
  s$l <- 19
  z<-bind_rows(z,s)
  
  #gwiazdka
  z[z$y==max(z$y),3]<-11
  names(z) <- c("x","y","l")
  z
}

z <- tworzenieDanych(100000)

ggplot(data = z, aes(x = x, y = y,color=l)) + 
  geom_point(size = 3,alpha=0.7)+
  scale_color_gradient(low = "#003300", high = "#66FF00")+
  geom_point(size = 6, color = ifelse(z$l==9,"red",NA))+
  geom_point(size = 6, color = ifelse(z$l==10,"orange",NA))+
  geom_point(size=40,color = ifelse(z$l==11,"yellow",NA), shape="*")+
  geom_point(size = 3, color = ifelse(z$l==12,"lightblue",NA))+
  geom_point(size = 3, color = ifelse(z$l==13,"violetred1",NA))+
  geom_point(size = 3, color = ifelse(z$l==14,"lightblue",NA))+
  geom_point(size = 3, color = ifelse(z$l==15,"violetred1",NA))+
  geom_point(size = 3, color = ifelse(z$l==16,"lightblue",NA))+
  geom_point(size = 3, color = ifelse(z$l==17,"violetred1",NA))+
  geom_point(size = 3, color = ifelse(z$l==18,"lightblue",NA))+
  geom_point(size = 3, color = ifelse(z$l==19,"violetred1",NA))+
  theme_bw()+
  ggtitle("Merry Christmas")+
  theme(
    plot.title=element_text(size = 25, colour ="gold1",hjust = 0.5),
    legend.position="none",
    axis.title.x=element_blank(),
    axis.title.y=element_blank())+
  transition_manual(l,cumulative = TRUE)

anim_save("tree4.gif")




