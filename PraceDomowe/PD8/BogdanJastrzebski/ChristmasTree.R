library(vegalite)
library(tidyverse)
set.seed(12)
n <- 100
m <- 9
d <- data.frame(1:n)

# abs(sin(i/m*pi)) * 

for(i in 1:m) {
  d <- cbind(d, x = (100-abs(1:n + 5*cumsum(2*runif(n) - 1))))
}

d <- d[,2:(m+1)]
colnames(d) <- 1:m

for(i in 1:m) {
  d[,i] <- d[,i]-min(d[,i])
  d[,i] <- d[,i] * 100/max(d[,i])
  d[,i] <- d[,i] * abs(sin(i/m*pi))
}


pien <- 0.3
for(i in 1:m) {
  d[,i] <- d[,i] * c(rep(pien, length.out = 5),
                     (pien + (1-pien)*sin(1:5/(5*pi/4))),
                     rep(1, length.out = 82),
                     c(5,4,3,4,5,1,1,1))
}

length(c(rep(pien, length.out = 5),
         (pien + (1-pien)*sin(1:5/(5*pi/4))),
         rep(1, length.out = 80,
         c(10,7,4,6,10,4,3,2,1,0))))
  
d <- gather(d)
d[,"key"] <- as.numeric(as.character(d[,"key"]))
d[1:5,"key"] <- 10
d[100 + 1:5,"key"] <- 10
d[200 + 1:5,"key"] <- 10
d[300 + 1:5,"key"] <- 10
d[400 + 1:5,"key"] <- 10
d[500 + 1:5,"key"] <- 9
d[600 + 1:5,"key"] <- 9
d[700 + 1:5,"key"] <- 9
d[00 + 96:100,"key"] <- 11
d[100 + 93:100,"key"] <- 11
d[200 + 93:100,"key"] <- 11
d[300 + 93:100,"key"] <- 11
d[400 + 93:100,"key"] <- 11
d[500 + 93:100,"key"] <- 12
d[600 + 93:100,"key"] <- 12
d[700 + 93:100,"key"] <- 12
d[800 + 93:100,"key"] <- 12
d <- cbind(d, y = rep(1:n, times=m))


vegalite() %>%
  cell_size(200, 400) %>%
  add_data(d) %>%
  encode_x("value", "quantitative", aggregate="sum") %>%
  encode_y("y", "temporal") %>%
  timeunit_y("yearmonth") %>%
  encode_color("key", "nominal") %>%
  scale_color_nominal(range="category20b") %>%
  legend_color(remove=TRUE) %>%
  axis_y(remove=TRUE) %>%
  axis_x(remove=TRUE) %>%
  mark_area(interpolate="basis",stack="center")
