library(eurostat)
library(ggplot2)
library(ggthemes)
library(gridExtra)
library(grid)

dat <- data.frame(get_eurostat(id = "sdg_11_40", time_format = "num"))
dat <- dat[dat$unit == "RT",]
datW <- dat[dat$geo %in% c("PT", "ES", "FR", "LU","BE", "UK","IE"),] 
datW$unit <- "W"
datC <- dat[dat$geo %in% c("DE", "AT", "CZ", "SK","PL", "HU","RO"),] 
datC$unit <- "C"
datS <- dat[dat$geo %in% c("HR", "BG", "EL", "MT","IT", "SI","CY"),] 
datS$unit <- "S"
datN <- dat[dat$geo %in% c("DK", "SE", "FI", "EE","LV", "LT","NL"),] 
datN$unit <- "N"
datEU <- dat[dat$geo %in% c("EU28"),] 
datEU$unit <- "EU"
datEU
data_full <- rbind(datW, datC, datS, datN)


chartW <- ggplot(data = datW, aes(x = time, y = values, colour = geo)) +
  geom_line() + labs(colour='Kraj') + geom_line(data = datEU, linetype = 2,colour = "red", size = 1.5 )+ 
  ylab("wskaźnik") + xlab("rok") + theme_bw()
chartC <- ggplot(data = datC, aes(x = time, y = values, colour = geo)) +
  geom_line() + labs(colour='Kraj') + geom_line(data = datEU, linetype = 2,colour = "red", size = 1.5 )+ 
  ylab("wskaźnik") + xlab("rok") + theme_bw()
chartS <- ggplot(data = datS, aes(x = time, y = values, colour = geo)) +
  geom_line() + labs(colour='Kraj') + geom_line(data = datEU, linetype = 2,colour = "red", size = 1.5 )+ 
  ylab("wskaźnik") + xlab("rok") + theme_bw()
chartN <- ggplot(data = datN, aes(x = time, y = values, colour = geo)) +
  geom_line() + labs(colour='Kraj') + geom_line(data = datEU, linetype = 2,colour = "red", size = 1.5 )+ 
  ylab("wskaźnik") + xlab("rok") + theme_bw()
grid.arrange(chartW, chartC,chartS,chartN, nrow = 2, top = textGrob("Wskaźnik wypadków śmiertelnych w krajach UE na przestrzeni lat"), bottom = textGrob("Kolorem czerwonym został zaznaczony średni wskaźnik dla wszystkich krajów UE",gp=gpar(fontsize=9))) 

