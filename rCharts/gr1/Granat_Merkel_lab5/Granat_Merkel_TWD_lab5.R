library(rCharts)
przezycia <- archivist::aread("pbiecek/Przewodnik/arepo/609491e5ec491f240cbeafe377743e21")  

przezycia1969 <- przezycia[przezycia$Year == 1969 &
                             przezycia$Age != "110+",]
przezycia1989 <- przezycia[przezycia$Year == 1989 &
                             przezycia$Age != "110+",]
przezycia2009 <- przezycia[przezycia$Year == 2009 &
                             przezycia$Age != "110+",]
przezycia <- rbind(przezycia1969, przezycia1989, przezycia2009)
przezycia <- cbind(przezycia, paste(przezycia[,"Year"], przezycia[,"Gender"]))
przezycia[,"Age" ] <- as.numeric(as.character(przezycia[,"Age"]))
str(przezycia[,"ex"])
head(przezycia)
przezycia <- cbind(przezycia, przezycia[,"Age"]+przezycia[,"ex"])
colnames(przezycia) <- c("Year","Age","mx","qx","ax","lx","dx","Lx","Tx","ex","Gender","Group","Sum")
p1 <- nPlot(Sum ~ Age, group = "Group", data = przezycia, type = "lineChart")
p1$xAxis(axisLabel = 'Wiek')
p1$yAxis(axisLabel = 'Przewidywana ca³kowita d³ugoœæ ¿ycia')
p1$set(width = 750, height = 590)
p1$save("rCharts1.html", standalone=TRUE)

