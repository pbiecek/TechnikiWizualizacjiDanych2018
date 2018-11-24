library("png")
library(ggplot2)
library(zoo)

difference <- function(x){
  #Liczy normę w R^3 między obecnym i poprzednim obrazkiem dla listy plików png
  prev <- readPNG(x[1])
  dim(prev) <- c(dim(prev)[1] * dim(prev)[2], dim(prev)[3])
  
  dist <- rep(0, times=(length(x)-1))
  
  for(i in 2:length(x)){
    cur <- readPNG(x[i])
    dim(cur) <- c(dim(cur)[1] * dim(cur)[2], dim(cur)[3])
    diff <- prev-cur
    dist[i-1] <- sum(abs(diff))
    prev <- cur
  }
  
  return(dist)
}

files <- c("HP1/HPKF00001.png",
           paste("HP1/HPKF000", seq(from=21, to=81, by=20), ".png", sep=""),
           paste("HP1/HPKF00", seq(from=101, to=981, by=20), ".png", sep=""),
           paste("HP1/HPKF0", seq(from=1001, to=9981, by=20), ".png", sep=""),
           paste("HP1/HPKF", seq(from=10001, to=208381, by=20), ".png", sep=""))
out <- difference(files)
#Średnia ruchoma
rm <- rollmean(out/max(out), 200)

df <- data.frame(value = c(rep(0, times=100), rm, rep(0, times=99)), value2 = out/max(out), x = 1:length(out))
ggplot(df, aes(x=x, y=value)) + 
  geom_point(aes(y = value2)) +
  geom_point(aes(color = "red"))
