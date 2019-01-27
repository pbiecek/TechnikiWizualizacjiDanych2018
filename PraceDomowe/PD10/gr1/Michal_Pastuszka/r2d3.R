library(data.table)
library(r2d3)
przezycia <- archivist::aread("pbiecek/Przewodnik/arepo/609491e5ec491f240cbeafe377743e21")


przezycia <- data.table(przezycia)

przezycia <- przezycia[Year==2009] 


pr <- przezycia[,c(2, 9, 11)]

prm <- pr[pr$Gender=="Male"]
prf <- pr[pr$Gender=="Female"]

setkey(prf, Age)
setkey(prm, Age)

ppr <- prm[prf]

ppr[,"color"] <- "green"

ppr[,"delta"] <- ppr$i.Tx-ppr$Tx
ppr[ppr$delta<0,"color"] <- "red"
ppr[,"delta"] <- abs(ppr$delta)
ppr[,"minrate"] <- pmin(ppr$i.Tx,ppr$Tx)
ppr[,"rate"] <- pmax(ppr$i.Tx,ppr$Tx)
out <- ppr[,c("Age", "color", "delta", "minrate", "rate")]
out[,"Age"] <- as.character(out$Age)
out[,"Age"] <- as.numeric(out$Age)
setkey(out, "Age")
out2 <- out[1:3,]
out <- out[-1,]
outj <- jsonlite::toJSON(out)

r2d3::r2d3("d3.js", data=outj, css="styles.css")

