 dat <- read.csv2("./dane3.csv", stringsAsFactors = FALSE, sep=",")
 #Funkcja wewnętrzna przygotowująca dane w odpowiednim formacie do wyświetlenia jako kolejka
 printPerson <- function(x, dat) {
   
   if(!( x %in% dat[,1])){
     str <- paste0('{"name":', '"', x, '",', '"parent":', '"",','"Uczelnia":', '"",','"Rok":', '""','}', sep="")
   }else{
     temprow <- dat[match(x, dat[,1]), ]
     if(is.na(temprow[4][1])){
       temprow[4][1] <- ""
     }
     if(is.na(temprow[3][1])){
       temprow[3][1] <- ""
     }
     if(is.na(temprow[2][1])){
       temprow[2][1] <- ""
     }
     str <- paste('{"name":', '"', temprow[1], '",', '"parent":', '"', temprow[4][1],'",', '"Uczelnia":', '"', temprow[2],'",', '"Rok":', '"', temprow[3],'",', '"children": [', printPerson(temprow[4], dat), ']','}')
   }
  str 
 }
roots <- dat[1:41,1]
finalStr <- '['
for(i in roots){
  tempList <- rep(NA, 50)
  tempList[1] <- i
  opiekun <- dat[which(dat[,1]==i), 4]
  tempList[2] <- opiekun[1]
  j <- 3
  while(!is.na(dat[which(dat[,1]==opiekun), 4][1])){
    opiekun <- dat[which(dat[,1]==opiekun), 4][1]
    tempList[j] <- opiekun
    j <- j + 1
  }
  tempList2 <- tempList[!is.na(tempList)]
  if(tempList2[[length(tempList2)]]=="BRAK"){
    tempList2[[length(tempList2)]] <- ""
  }
  str <- printPerson(tempList2[1], dat)
  finalStr <- paste(finalStr, str, ",")
}
fileConn<-file("output.txt")
writeLines(finalStr, fileConn)
close(fileConn)
