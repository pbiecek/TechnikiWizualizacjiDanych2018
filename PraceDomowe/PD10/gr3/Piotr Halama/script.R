library(dplyr)
library(tidyr)

przezycia <- archivist::aread("pbiecek/Przewodnik/arepo/609491e5ec491f240cbeafe377743e21")

data <- filter(przezycia, Year == 2009) %>%
  select(Age, Gender, Tx) %>%
  spread(Gender, Tx) %>%
  mutate(Age=if_else(Age=='110+', 111, as.numeric(levels(Age)[Age]))) %>%
  group_by(AgeGroup=cut(Age, breaks=seq(0, 120, by=10), include.lowest = T)) %>%
  summarise(Female=sum(Female), Male=sum(Male)) %>%
  mutate(Diff=Female-Male)

write.csv(data, "data.csv")
