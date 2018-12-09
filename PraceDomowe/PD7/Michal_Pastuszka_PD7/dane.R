library(readxl)
library(data.table)
library(ggplot2)
library(ggmap)
pm10 <- data.table(read_excel("dane/to/2002_PM10_24g.xlsx"))
pm25 <- data.table(read_excel("dane/to/2002_PM2.5_24g.xlsx"))


lata <- seq(2003, 2017)

for(i in 1:15) {
  x = data.table(read_excel(paste("dane/to/", lata[i], "_PM10_24g.xlsx", sep="")))
  y = data.table(read_excel(paste("dane/to/", lata[i], "_PM2.5_24g.xlsx", sep="")))
  pm10 <- rbind(pm10, x, fill=TRUE)
  pm25 <- rbind(pm25, y, fill=TRUE)
}

nazwy <- data.table(read_excel("Kopia Kody_stacji_pomiarowych.xlsx"))
setkey(nazwy, `KOD NOWY`)


colnames(pm10)[1] <- "czas"
colnames(pm25)[1] <- "czas"
pm10$czas <- as.Date(pm10$czas)
pm25$czas <- as.Date(pm25$czas)


meta <- data.table(read_excel("Metadane_wer20180829.xlsx"))
colnames(meta)[c(15,16)] <- c("lat", "lng")
setkey(meta, `Kod stacji`)
nazwy <- meta[nazwy,]
nazwy <- nazwy[,c("Kod stacji", "Nazwa stacji", "lat", "lng")]
pm <- list(pm10, pm25)
setkey(nazwy, `Kod stacji`)
nazwy25 <- nazwy[nazwy$`Kod stacji`%in%colnames(pm25),]
setkey(nazwy25, `Nazwa stacji`)
nazwy10 <- nazwy[nazwy$`Kod stacji`%in%colnames(pm10),]

setkey(nazwy10, `Nazwa stacji`)


pal <- colorNumeric(
  palette = c("green", "red", "darkorchid4"),
  domain = 0:350)

save(pm, nazwy, nazwy25, nazwy10, pal, file="dane.RData")

