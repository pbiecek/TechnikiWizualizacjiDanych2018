library(dplyr)
library(r2d3)
library(reshape2)
unique(przezycia$Age)
przezycia <- archivist::aread("pbiecek/Przewodnik/arepo/609491e5ec491f240cbeafe377743e21")
przezycia <- filter(przezycia, Year == 2009)[, c("Age", "Tx", "Gender")]
przezycia <- przezycia %>% mutate(FTx = ifelse(Gender == "Female", Tx, 0), 
                     MTx = ifelse(Gender == "Male", Tx, 0)) %>% 
  select(Age, FTx, MTx)
przezycia <- 
  aggregate(przezycia[,c("MTx", "FTx")], by=list(Age=przezycia$Age), FUN=sum)
przezycia <- przezycia %>% mutate(roznica = FTx - MTx)
changed <- przezycia[przezycia$Age != "110+",]
changed$Age <- as.numeric(changed$Age) - 1
przezycia <- rbind(changed, przezycia[przezycia$Age == "110+",])
write.csv(przezycia, file = "/Users/dasha/Documents/twd/przezycia2009.csv")
