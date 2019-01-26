library(XML)
library(stringi)
library(dplyr)
library(scales)

options(stringsAsFactors=FALSE)

# Pobierz pliki HTML zawierające dane z messengera 
path = "C:/Users/tomas/Downloads/inbox/"
setwd(path)
out.file<-""
file.names <- dir(path, pattern =".html")

# Utwórz wstępną tabelę
date_list <- seq(as.Date("2018-01-01"), as.Date("2019-01-16"), "days")
date_list <- data.frame(date = stri_datetime_format(date_list,"dd MMM yyyy"))
df_facebook <- data.frame(date = date_list)

# Wyciągamy interesujące nas informacje korzystając z regexów
for(i in 1:length(file.names)){
  print(file.names[i])
  doc.html = htmlTreeParse(file.names[i],
                           useInternal = TRUE)

  doc.text = unlist(xpathApply(doc.html, '//div', xmlValue))
  dane_do_grupowania <- unlist(stri_extract_all_regex(doc.text[1],"([1-9].{5}2019)|(1[0-9].{5}2019)|(2[0-9].{5}2019)|(3[01].{5}2019)|([1-9].{5}2018)|(1[0-9].{5}2018)|(2[0-9].{5}2018)|(3[01].{5}2018)"))

  wiadomosci_per_day <- data.frame(data = dane_do_grupowania) %>% group_by(data) %>% summarise(cnt_msg = n())
  wiadomosci_per_day$data <- stri_datetime_parse(wiadomosci_per_day$data,format = "dd MMM yyyy")
  wiadomosci_per_day$data <- stri_datetime_format(wiadomosci_per_day$data,format = "dd MMM yyyy")

  df_facebook <- left_join(df_facebook,wiadomosci_per_day, by = c("date" = "data"))
}
df_facebook <- df_facebook[!duplicated(df_facebook$date),]
df_facebook <- data.frame(date_list,msg_cnt = rowSums(df_facebook[,2:ncol(df_facebook)], na.rm = TRUE))


#21.08 - poczatek
date_list_kroki <- seq(as.Date("2018-08-21"), as.Date("2019-01-15"), "days")
date_list_kroki <- data.frame(date = stri_datetime_format(date_list_kroki,"dd MMM yyyy"))
kroki <- c(12322,11061,9947,10698,7970,14690,20425,21098,14579,8247,10897,5183,10478,
           15928,12245,8177,22723,19699,14255,20390,10262,8932,20912,7218,0,0,0,0,0,0,0,0,0,0,3149,
           8552,531,1913,6539,2940,6549,0,0,347,5310,3420,11461,7118,8155,15235,7420,11755,6830,12054,
           9474,701,7205,6457,1072,7955,9856,1759,3566,7381,6739,7849,0,0,3208,7118,12641,5996,0,
           1210,4311,4123,0,3126,8249,6541,2755,2685,12504,6000,7585,5650,0,4766,6922,2174,10686,
           4239,8800,6555,12612,8965,12831,2953,10061,9669,6326,12458,11034,5036,7665,6371,7947,8370,
           30823,22634,5487,8828,11067,9044,10486,2418,14408,23170,5205,7119,10648,10836,6751,12224,11354,7418,4164,
           4244,5646,16229,23481,23624,20284,11934,4602,10299,13399,7295,10899,8124,5780,5721,7975,22350,
           13211,14028,12386,11536)
df_kroki <- data.frame(data = date_list_kroki,kroki)

# Ostateczne formy wyjściowych danych
df_all <- left_join(df_facebook,df_kroki, by = c("date" = "date")) %>% rename(day = date)
df_all$day <- as.Date(stri_datetime_parse(df_all$day,format = "dd MMM yyyy"))
df_all2 <- df_all %>% filter(day >= '2018-08-21') %>% filter(day < '2019-01-16')

df_all2$day <- stri_datetime_format(df_all2$day,"dd-MMM-yy")
df_all$day <- stri_datetime_format(df_all$day,"dd-MMM-yy")
write.csv(df_all2,file = "C:/Users/tomas/OneDrive/Desktop/twd/data2.csv", quote = FALSE, row.names = FALSE)
df_all[is.na(df_all)] <- 0
write.csv(df_all %>% filter(day < "2019-01-15"), file = "C:/Users/tomas/OneDrive/Desktop/twd/data.csv", quote = FALSE, row.names = FALSE)


