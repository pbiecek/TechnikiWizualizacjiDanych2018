library(vegalite)
library(magrittr)
library(data.table)


alpha <- 0.5
A <- 30
n <- 1000
#Nasionko choinki:
set.seed(1524746)


X <- function() abs(alpha*(1:n + cumsum(A*2*(runif(n) - 0.5))))

dfr <- data.frame(index = 1:1000)


for (i in 1:10) {
  dfr <- cbind(dfr,X())
}
colnames(dfr) <- c("index", 1:10)
dane <- melt(dfr, id.vars="index")

#Gdyby ktoś kiedyś zastanawiał się, dlaczego oś y jest czasem i to z zakresu dokładnie jednej godziny, to dlatego, że tylko wtedy rysunek dobrze wpasowywał się w wykresie
dane$index <- as.POSIXct(3600-dane$index*3.6, origin='2018-12-25')
dane[dane$variable%in%c(1, 2, 3, 10, 8, 9), "value"] <- 0
dane[!(dane$variable%in%c(8,9))&dane$index<as.POSIXct('2018-12-25 01:05:00'), "value"] <- 0
dane[dane$variable%in%c(8,9)&dane$index<as.POSIXct('2018-12-25 01:05:00'), "value"] <- 200



vegalite() %>%
  cell_size(200, 450) %>%
  add_data(dane) %>%
  encode_y("index", "temporal") %>%
  encode_x("value", "quantitative", aggregate = "sum") %>%
  encode_color("variable", "nominal") %>%
  scale_color_nominal(range="category20b") %>%
  timeunit_y("minutes") %>%
  scale_y_time(range=c(min(dane$index), max(dane$index))) %>%
  axis_y(title="Poziom imponowania gościom") %>%
  axis_x(title="Powierzchnia magazynowa prezentów") %>%
  legend_color(remove=TRUE) %>%
  mark_area(interpolate="basis", stack="center")
