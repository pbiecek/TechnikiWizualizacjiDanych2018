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
  
  z <- data.frame(x,y)
  names(z) <- c("x","y")
  z
}

z <- tworzenieDanych(10000)
yGwiazdka <- max(z$y)

zGwiazdka <- data.frame(c(0),yGwiazdka)
names(zGwiazdka) <- c("x","y")

ggplot(data = z, aes(x = x, y = y)) + 
  geom_point(size = 5)+
  geom_point(data = zGwiazdka, aes(x = x, y = y),color="yellow",size=20)
