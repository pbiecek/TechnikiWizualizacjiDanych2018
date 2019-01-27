library(dplyr)

przezycia <- archivist::aread("pbiecek/Przewodnik/arepo/609491e5ec491f240cbeafe377743e21")
przezycia <- filter(przezycia, Year == 2009)[, c("Age", "Tx", "Gender")]
write.csv(przezycia, "przezyciaUciete.csv", row.names = FALSE)
