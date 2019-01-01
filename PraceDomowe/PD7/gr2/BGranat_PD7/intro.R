library(openxlsx)
library(ggplot2)
library(dplyr)
raw_data<-read.xlsx("2017_PM25_24g.xlsx")
raw_data <- raw_data[c(-2,-3,-4,-5),]
colnames(raw_data) <- raw_data[1,]
raw_data <- raw_data[-1,]
df <- raw_data
df$`Kod stacji` <- as.Date(as.numeric(raw_data$'Kod stacji'), origin = "1899-12-30")
df <- df %>% select('Kod stacji', DsWrocNaGrob, LdLodzLegion, MpKrakBujaka, PkRzeszRejta, PmGdaPowWiel, SlKatoKossut, MzWarKondrat, WpPoznPolank)
df <- df[1:31,]
df <- cbind(df[,1],as.data.frame(apply(df[,-1],2, function(x) as.numeric(sub(",", ".", x, fixed = TRUE)))))
colnames(df) <- c("Date", "Wroclaw", "Lodz", "Krakow", "Rzeszow","Gdansk","Katowice","Warszawa","Poznan")
saveRDS(df, "dane_PM25_miasta.rds")



