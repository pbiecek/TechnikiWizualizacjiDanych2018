#Zadanie 1
employees[employees$name == "John" & employees$surname == "Insecure", ]
#wynik

##    name  surname   login
##217 John Insecure johnins

proton(action = "login", login= "johnins")

#Zadanie 2
for (i in 1:1000){proton(action = "login", login="johnins", password=top1000passwords[i])}

#Zadanie 3
##login pietraszka
employees[employees$surname == "Pietraszko", ]
##wynik - slap

##lista wszystkich jego logowan
logs[logs$login == "slap", ]
