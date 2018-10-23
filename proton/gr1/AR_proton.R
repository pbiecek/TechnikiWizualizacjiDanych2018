#Zad 1.
    proton(action = "login", login="johnins") #217 John Insecure johnins
#Zad 2. 
    for(i in 1:1000) proton(action = "login", login="johnins", password=top1000passwords[i])
#Zad 3.
    #login slap
       x <- logs[logs$login=="slap",]
       library(plyr)
       head(count(x$host), 1)
       #194.29.178.16

