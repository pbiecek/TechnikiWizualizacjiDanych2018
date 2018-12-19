library(openxlsx)

kodyStacji <- read.xlsx("Kody_stacji_pomiarowych.xlsx")
metadaneStacji <- read.xlsx("Metadane_wer20180829.xlsx", sheet = 1)
metadaneStanowisk <- read.xlsx("Metadane_wer20180829.xlsx", sheet = 2)
statystykiSO2 <- read.xlsx("Statystyki_2000-2018_wer20180828.xlsx", sheet = 1)[, -c(9, 12:17)]
statystykiNO2 <- read.xlsx("Statystyki_2000-2018_wer20180828.xlsx", sheet = 2)[, -(11:13)]
statystykiNOx <- read.xlsx("Statystyki_2000-2018_wer20180828.xlsx", sheet = 3)
statystykiCO <- read.xlsx("Statystyki_2000-2018_wer20180828.xlsx", sheet = 4)[, -11]
statystykiO3 <- read.xlsx("Statystyki_2000-2018_wer20180828.xlsx", sheet = 5)[, -c(11:15, 17:19)]
statystykiC6H6 <- read.xlsx("Statystyki_2000-2018_wer20180828.xlsx", sheet = 6)
statystykiPM10 <- read.xlsx("Statystyki_2000-2018_wer20180828.xlsx", sheet = 7)
statystykiPM25 <- read.xlsx("Statystyki_2000-2018_wer20180828.xlsx", sheet = 8)
statystykiPb <- read.xlsx("Statystyki_2000-2018_wer20180828.xlsx", sheet = 9)
statystykiAs <- read.xlsx("Statystyki_2000-2018_wer20180828.xlsx", sheet = 10)
statystykiCd <- read.xlsx("Statystyki_2000-2018_wer20180828.xlsx", sheet = 11)
statystykiNi <- read.xlsx("Statystyki_2000-2018_wer20180828.xlsx", sheet = 12)
statystykiBaP <- read.xlsx("Statystyki_2000-2018_wer20180828.xlsx", sheet = 13)
#statystykiInne <- read.xlsx("Statystyki_2000-2018_wer20180828.xlsx", sheet = 14)
statystykiPM <- rbind(statystykiPM10[, -(11:14)], statystykiPM25)
kolumny <- c("Rok", "Wojewodztwo", "Kod.strefy", "Nazwa.strefy", "Kod.stacji", "Wskaznik", "Czas.usredniania", "Srednia", "Min", "Maks",
             "Liczba.pomiarow", "Kompletnosc", "Lato/Zima")
colnames(statystykiAs) <- kolumny
colnames(statystykiBaP) <- kolumny
colnames(statystykiC6H6) <- kolumny
colnames(statystykiCd) <- kolumny
colnames(statystykiCO) <- kolumny
#colnames(statystykiInne) <- kolumny
colnames(statystykiNi) <- kolumny
colnames(statystykiNO2) <- kolumny
colnames(statystykiNOx) <- kolumny
colnames(statystykiO3) <- kolumny
colnames(statystykiPb) <- kolumny
colnames(statystykiPM) <- kolumny
colnames(statystykiSO2) <- kolumny
#statystyki <- rbind(statystykiPM, statystykiAs, statystykiBaP, statystykiC6H6, statystykiCd, statystykiCO, statystykiInne, statystykiNi,
#                    statystykiNO2, statystykiNOx, statystykiO3, statystykiPb, statystykiSO2)
statystyki <- rbind(statystykiPM, statystykiAs, statystykiBaP, statystykiC6H6, statystykiCd, statystykiCO, statystykiNi,
                    statystykiNO2, statystykiNOx, statystykiO3, statystykiPb, statystykiSO2)
statystyki <- merge(statystyki, kodyStacji[, c("KOD.NOWY", "NAZWA.STACJI")], by.x = "Kod.stacji", by.y = "KOD.NOWY")

save.image(file = "app.RData")
