
###################################################################################

library(qdapRegex)
library(stringr)
library(stringi)
library(dplyr)
library(hashmap)
library(rstudioapi)

path <- getActiveDocumentContext()$path
setwd(dirname(path))

source("polskie_znaki.R")
wiadomosci = read.csv("moje_wiadomosci.csv")

#slownik, hashmapy
###################################################################################

slownik = read.table("polimorfologik.txt", header=F, sep="\t", stringsAsFactors=FALSE, fill=T)
slownik[[1]] = polskieznaki_slownik(slownik[[1]])
slownik[[2]] = polskieznaki_slownik(slownik[[2]])
slownik[[1]] = tolower(slownik[[1]])
slownik[[2]] = tolower(slownik[[2]])

word_cores = hashmap(slownik[[1]], slownik[[2]])
czesci_mowy = hashmap(slownik[[1]], slownik[[3]])

# funkcje pomocnicze
####################################################################################

policz_slowa = function(tresci){
  regex_word = '[a-zA-z]+'
  words = unlist(ex_default(tresci, pattern = regex_word))
  
  core_counts = hashmap(character(), numeric())
  for(word in words){
    if (word_cores$has_key(word)) {
      core = word_cores[[word]]
    }
    else {
      core = "____NOT-PRESENT____"
    }
    if(!core_counts$has_key(core)){
      core_counts[[core]] = 1
    }
    else{
      core_counts[[core]] = core_counts[[core]] + 1
    }
  }
  counts = data.frame(data.frame(core_counts$keys()), data.frame(core_counts$values()))
  colnames(counts) = c("slowo", "liczba")
  return(counts)
}

usun_nieistotne_slowa = function(counts){
  # usuwanie slow krotszych niz 3 znaki
  rows_to_remove = c()
  n = nrow(counts)
  for(i in 1:n){
    if(stri_length(counts$slowo[i]) < 3){
      rows_to_remove[length(rows_to_remove) + 1] = i
    }
  }
  rows_to_remove
  
  counts = counts[-rows_to_remove,]
  counts = counts[!(counts$slowo == "____NOT-PRESENT____"),]
  counts = counts[order(counts$liczba, decreasing = TRUE),]
  
  uciecie_counts = min(200, nrow(counts))
  my_freq_words = counts[1:uciecie_counts,]
  
  nr_wierszy_czesci_mowy_do_usuniecia = c()
  czesci_mowy_do_usuniecia = c("conj", "comp", "prep", "akc", "ppron12", "ppron3", "qub")
  n = nrow(my_freq_words)
  for(i in 1:n){
    slowo = my_freq_words$slowo[i]
    czesc_mowy = czesci_mowy[[slowo]]
    if(!is.na(czesc_mowy) & any(str_detect(czesc_mowy, czesci_mowy_do_usuniecia))){
      nr_wierszy_czesci_mowy_do_usuniecia[length(nr_wierszy_czesci_mowy_do_usuniecia) + 1] = i
    }
  }
  # mozna sobie sprawdzic: my_freq_words[nr_wierszy_czesci_mowy_do_usuniecia,]
  my_freq_words = my_freq_words[-nr_wierszy_czesci_mowy_do_usuniecia,]
  
  slowa_do_wyrzucenia = c("taka", "ktory","jaka", "ten", "moj", "tym", "jakis", "jakos",
                          "oda", "tego", "twoj", "ala", "byc", "wiek", "ojciec")
  #ala usuwam, bo Ala i ale sie mieszaja... 
  my_freq_words = my_freq_words[which(!(my_freq_words$slowo %in% slowa_do_wyrzucenia)),]
  uciecie_my_freq_words = min(100, nrow(my_freq_words))
  my_freq_words = my_freq_words[1:uciecie_my_freq_words,]
  return(my_freq_words)
}

zwroc_najczestsze_slowa_znajomy = function(osoba){
  tresci = wiadomosci[which(wiadomosci$Kontakt == osoba),"Tresc"]
  counts = policz_slowa(tresci)
  my_freq_words = usun_nieistotne_slowa(counts)
  return(my_freq_words)
}

# zbudowanie dataframe ze zliczeniami dla poszczegolnych znajomych i zapisanie do csv
########################################################################################

znajomi = c("Grzegorz Ozimek", "Sebastian Kurpios", "Wojciech Mann", "Bartek Nowak",
            "Dawid Kowalczyk", "Maciek Kurek", "Ewa Barbara Cecylia", "Monika Chudek",
            "Anna Kozak", "Ilona Bednarz")

zliczone_slowa_do_konkretnych_osob = data.frame(character(), integer(), character())

for(i in 1:length(znajomi)){
  print(znajomi[i])
  najczestsze_slowa_znajomy = zwroc_najczestsze_slowa_znajomy(znajomi[i])
  kolumna_kto = rep(znajomi[i], length(najczestsze_slowa_znajomy))
  df = cbind(najczestsze_slowa_znajomy, kolumna_kto)
  zliczone_slowa_do_konkretnych_osob = rbind(zliczone_slowa_do_konkretnych_osob, df)
  print("skonczylem liczyc")
}

write.csv(zliczone_slowa_do_konkretnych_osob, "zliczone_slowa_do_konkretnych_osob.csv")

########################################################################################
