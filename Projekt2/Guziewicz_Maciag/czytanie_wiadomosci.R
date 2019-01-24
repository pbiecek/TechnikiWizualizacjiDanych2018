
############################################################################################################

ja = "Michal Maciag"

############################################################################################################

library(rjson)
library(stringi)
library(dplyr)
library(ggplot2)
library(rstudioapi)

path <- getActiveDocumentContext()$path
setwd(dirname(path))

############################################################################################################

source("polskie_znaki.R")

############################################################################################################

folder <- "/Users/michalmaciag/Desktop/TWD Projekt 2/inbox/"
file_list <- list.files(path = folder, pattern="*.") 
nr_of_friends <- length(file_list)
dane <- data.frame()

############################################################################################################

for(i in 1:nr_of_friends){
  
  name <- file_list[i]
  name <- paste(folder, name, sep = '')
  name <- paste(name, '/message.json', sep='')
  
  l <- fromJSON(paste(readLines(as.character(name)), collapse = ''))
  if(length(l$participants) == 2){
    imiona <- polskieznaki(c(l$participants[[1]], l$participants[[2]]))
    
    a1 <- which(imiona == ja)
    a2 <- which(imiona != ja)
    
    Kontakt <- imiona[a2]
    
    m <- lapply(l$messages, function(x) c(x$sender_name, x$timestamp_ms, x$content, x$type))
    m <- do.call(rbind, m)
    m <- data.frame(m)
    colnames(m) <- c('Autor', 'Czas', 'Tresc', 'Typ')
    
    m$Autor <- polskieznaki(m$Autor)
    m$Tresc <- polskieznaki(m$Tresc)
    
    m <- m %>% mutate(Kontakt = Kontakt)
    
    m <- m %>% mutate(Autor = ifelse(Autor == Kontakt, 2, 1))
    m <- m %>% mutate(Czas =  as.POSIXct(as.numeric(as.character(Czas))/1000, origin="1970-01-01") )
      
    dane <- rbind(dane, m)

  }
}

dane$Tresc <- stri_replace_all_regex(dane$Tresc, '[^a-zA-Z ]', '')
dane$Tresc <- stri_replace_all_regex(dane$Tresc, '/s/s+', ' ')
dane$Tresc <- tolower(dane$Tresc)

############################################################################################################

dane = dane[which(dane$Typ == 'Generic'),]
dane = within(dane, rm(Typ))

write.table(dane, file = "wiadomosci.csv",row.names=FALSE, na="",col.names=TRUE, sep=",")

dane = read.csv("wiadomosci.csv")
moje_wiadomosci = dane[which(dane$Autor == 1),]
moje_wiadomosci = within(moje_wiadomosci, rm(Autor))
write.table(moje_wiadomosci, file = "moje_wiadomosci.csv",row.names=FALSE, na="",col.names=TRUE, sep=",")

############################################################################################################

dane = read.csv("wiadomosci.csv")
dane <- dane %>% select(Autor, Czas, Kontakt)
write.table(dane, file = "wiadomosci.csv", row.names = FALSE, na = "", col.names = TRUE, sep = ",")

############################################################################################################
