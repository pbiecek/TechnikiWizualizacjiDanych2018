library(dplyr)
przezycia <- archivist::aread("pbiecek/Przewodnik/arepo/609491e5ec491f240cbeafe377743e21")  

przezycia$Age <- as.numeric(as.character(przezycia$Age) )

p <- przezycia %>% filter(Year == 2009) %>% group_by(Gender, Age) %>% summarise(tx = mean(Tx))

p <- inner_join(p %>% filter(Gender == "Female"),
                p %>% filter(Gender == "Male"),
                by = c("Age" = "Age"))

p <- p %>% ungroup() %>% select(Age, female = tx.x, male = tx.y)  
r2d3::r2d3("C:/Users/Piotr/Documents/Bogdan/BogdanJastrzêbskipd10/p.js", data=p)


