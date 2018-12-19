library(readxl)
library(data.table)
library(ggplot2)


kody <- data.table(read_excel("Kopia Kody_stacji_pomiarowych.xlsx"))

PM10 <- fread("2017/2017_PM10_24g.csv",sep = ";",dec = ",")
PM25 <- fread("2017/2017_PM25_24g.csv",sep = ";",dec = ",")


wwa_kody <- kody[MIEJSCOWOSC=="Warszawa", `KOD NOWY`]
krk_kody <- kody[MIEJSCOWOSC=="Kraków", `KOD NOWY`]

PM10 <- PM10[,c("Kod stacji",intersect(colnames(PM10),c(wwa_kody,krk_kody))), with=FALSE]
colnames(PM10)[2:ncol(PM10)] <- kody[`KOD NOWY` %in% colnames(PM10),`NAZWA STACJI`]
PM10 <- melt(PM10, id.vars = 1, variable.name = "nazwa")
PM10[,miasto:= c(rep("Kraków",365*2),rep("Warszawa",365*3))]

PM25 <- PM25[,c("Kod stacji",intersect(colnames(PM25),c(wwa_kody,krk_kody))), with=FALSE]
colnames(PM25)[2:ncol(PM25)] <- kody[`KOD NOWY` %in% colnames(PM25),`NAZWA STACJI`]
PM25 <- melt(PM25,id.vars = 1, variable.name = "nazwa")
PM25[,miasto:= c(rep("Kraków",365),rep("Warszawa",365*2))]

PM10$`Kod stacji` <- as.Date.character(PM10$`Kod stacji`, "%d.%m.%Y")
PM25$`Kod stacji` <- as.Date.character(PM25$`Kod stacji`, "%d.%m.%Y")

save(PM10, file="PM10.RData")
save(PM25, file="PM25.RData")
## wykresy ##
ggplot(PM10_wwa,aes(x=`Kod stacji`, y=value, color=nazwa)) + 
  geom_point() +
  geom_point(data=PM10_krk,aes(x=`Kod stacji`, y=value, color=nazwa))


PM10[miasto=="Warszawa",mean(value,na.rm = TRUE), by=.(month(`Kod stacji`),miasto)]

