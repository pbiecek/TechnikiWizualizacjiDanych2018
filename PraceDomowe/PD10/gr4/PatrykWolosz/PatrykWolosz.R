library(archivist)
library(dplyr)
library(r2d3)

data <- archivist::aread("pbiecek/Przewodnik/arepo/609491e5ec491f240cbeafe377743e21")  

d <- data %>% 
  filter(Year == 2009) %>%
  select(x = Age, value = Tx, type = Gender)

write.csv(d, file = "D:/TW/PD10/data.csv")
