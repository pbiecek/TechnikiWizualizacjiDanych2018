library(data.table)
library(r2d3)
przezycia <- archivist::aread("pbiecek/Przewodnik/arepo/609491e5ec491f240cbeafe377743e21")

przezycia <- data.table(przezycia)
przezycia <- przezycia[Year==2009] 
przezycia2009$Age <- as.numeric(as.character(przezycia2009$Age))
przezycia2009 <- na.omit(przezycia2009)

przezycia2009 <- data.table(przezycia2009$Age, przezycia2009$Tx,przezycia2009$Gender)
colnames(przezycia2009) <- c("Age", "Tx", "Gender") 

przezycia2009_kobiety <- przezycia2009[przezycia2009$Gender=="Female"]
przezycia2009_mezczyzni <- przezycia2009[przezycia2009$Gender=="Male"]

przezycia <- merge(przezycia2009_kobiety, przezycia2009_mezczyzni, by="Age")
przezycia <- data.table(przezycia$Age, przezycia$Tx.x, przezycia$Tx.y)
colnames(przezycia) <- c("Age", "Female", "Male")
przezycia$roznica <- przezycia$Female-przezycia$Male
przezycia_json <- jsonlite::toJSON(przezycia)

r2d3::r2d3("skrypt.js", data=przezycia_json)
