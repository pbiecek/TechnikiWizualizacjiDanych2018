
library(dplyr)
library(rstudioapi)

path <- getActiveDocumentContext()$path
setwd(dirname(path))

przezycia <- archivist::aread("pbiecek/Przewodnik/arepo/609491e5ec491f240cbeafe377743e21")  

WYNIK <- ''

for(j in 2000:2009){
  
  rok <- j
  
  przed <- paste('  {', '\n', '    "name": "', rok, ' rok",', '\n', '    "children": [', '\n', sep = '')
  
  przezycia1 <- przezycia %>% select(Year, Age, Gender, Tx) %>% filter(Year == rok, Gender == 'Female')
  przezycia2 <- przezycia %>% select(Year, Age, Gender, Tx) %>% filter(Year == rok, Gender == 'Male')
  
  wynik1 <- ''
  wynik2 <- ''
  l1 <- dim(przezycia1)[1]
  l2 <- dim(przezycia2)[1]
  
  przed1 <- paste('    {"name": "Kobiety",', '\n', '      "children": [', '\n', sep = '')
  
  for(i in 1:l1){
    ile <- przezycia1$Tx[i]/1000000
    wiek <- przezycia1$Age[i]
    temp <- paste('        {"name": "', wiek, ' lat", "size": ', ile, '}', sep = '')
    if(i < l1){
      temp <- paste(temp, ',', sep = '')
    }
    if(i == 1){
      temp <- paste(' ', temp, sep = '')
      wynik1 <- temp
    }
    if(i > 1){
      wynik1 <- paste(wynik1, '\n', temp)
    }
  }
  
  po1 <- paste('\n', '    ]', '\n', '    },', '\n', sep = '')
  
  przed2 <- paste('    {"name": "Mężczyżni",', '\n', '      "children": [', '\n', sep = '')
  
  for(i in 1:l2){
    ile <- przezycia2$Tx[i]/1000000
    wiek <- przezycia2$Age[i]
    temp <- paste('        {"name": "', wiek, ' lat", "size": ', ile, '}', sep = '')
    if(i < l2){
      temp <- paste(temp, ',', sep = '')
    }
    if(i == 1){
      temp <- paste(' ', temp, sep = '')
      wynik2 <- temp
    }
    if(i > 1){
      wynik2 <- paste(wynik2, '\n', temp)
    }
  }
  
  po2 <- paste('\n', '    ]', '\n', '    }', '\n', sep = '')
  
  if(j < 2009){
    po <- paste('\n', '   ]', '\n', '   },', '\n', sep = '')
  }
  if(j == 2009){
    po <- paste('\n', '   ]', '\n', '   }', '\n', sep = '')
  }
  
  wynik <- paste(przed, przed1, wynik1, po1, przed2, wynik2, po2, po, sep = '')

  if(j == 1){
    WYNIK <- wynik
  }
  if(j > 1){
    WYNIK <- paste(WYNIK, '\n', wynik)
  }

}

PRZED <- paste('{', '\n', '  "name": "Cała populacja",', '\n', '  "children": [', '\n', sep = '')
PO <- paste('  ]', '\n', '}', sep = '')

JSON <- paste(PRZED, WYNIK, PO, sep = '')

sink('dane.json')
cat(JSON)
sink()
