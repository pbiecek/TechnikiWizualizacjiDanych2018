library(lubridate)
library(data.table)
library(ggplot2)
library(datasets)

dane <- data.table(X2017_PM25_24g)
wybrane <- dane[, .(Dzien = `Kod stanowiska`,Wroclaw = `DsWrocNaGrob-PM2.5-24g`,
                    Lodz = `LdLodzLegion-PM2.5-24g`,Krakow = `MpKrakBujaka-PM2.5-24g`,
                    Warszawa = `MzWarWokalna-PM2.5-24g`,Poznan = `WpPoznPolank-PM2.5-24g`)] 
wybrane$Dzien <- parse_date_time(wybrane$Dzien,"ymd")
write.csv2(wybrane, file = "dane25.csv")

dane2 <- data.table(X2017_PM10_24g)
wybrane2 <- dane2[, .(Dzien = `Kod stanowiska`,Wroclaw = `DsWrocWybCon-PM10-24g`,
                    Lodz = `LdLodzLegion-PM10-24g`,Krakow = `MpKrakBujaka-PM10-24g`,
                    Warszawa = `MzWarAlNiepo-PM10-24g`,Poznan = `WpPoznSzyman-PM10-24g`)]
wybrane2$Dzien <- parse_date_time(wybrane$Dzien,"ymd")
write.csv2(wybrane2, file = "dane10.csv")





