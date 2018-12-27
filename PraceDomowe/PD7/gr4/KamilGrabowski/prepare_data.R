library(readxl)
library(lubridate)
library(stringr)
library(ggplot2)
df = read_excel(path="/home/kamil/Downloads/2017_PM10_1g.xlsx", skip=5)
data = data.frame(df)
head(data)

without_first = data[,2:124]
without_first <- sapply(without_first, function(i){
  as.numeric(str_replace(i,",","."))
})

sum <- rowSums(without_first, na.rm = TRUE)
month <- months(as.Date(data$Kod.stanowiska))
weekday <- weekdays(as.Date(data$Kod.stanowiska))
hour <- hour(hms(substr(data$Kod.stanowiska, 12, 19)))
new_data = data.frame(month,weekday, hour, sum)
head(new_data)
write.csv(new_data, file = "pd7.csv")
