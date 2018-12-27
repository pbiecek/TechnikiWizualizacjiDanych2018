library(dplyr)
dane <- read.csv2("gios-pjp-data.csv")
#os. Armii Krajowej Opole manualna 4
#ul. Koszyka Opole automat 5
#dopuszczalny 
dane<-dane[c(1,2,11)]

#jednostka<-"ug/m3"

dane<-as_tibble(dane)

dane<-dane[-1,]
dane<-dane[-344,]
dane<-as.data.frame(dane)
xd<-strptime(as.character(dane[[1]]),format="%Y-%m-%d")
xd<-format(xd,"%d-%m-%Y")
dane[,1]<-xd
dane[,2]<-as.numeric(as.character(dane[[2]]))
dane[,3]<-as.numeric(as.character(dane[[3]]))
colnames(dane)<-c("Data","Pył zawieszony PM10(ug/m3)","Pył zawieszony PM2,5(ug/m3)")
dane<-as.data.frame(dane)
write.csv2(file = "Powietrze_w_Opolu.csv",dane,row.names = FALSE)
