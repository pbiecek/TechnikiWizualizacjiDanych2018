install.packages("BetaBit")
library(BetaBit)
proton()

head(employees)
employees[employees['name']=='John']

proton(action= "login", login="johnins")

top1000passwords

haslo <- function(i) {
  proton(action = "login", login="johnins", password=top1000passwords[i])
}

lapply(1:1000, haslo) 

head(logs)

#Well done! This is the right password!
#Bit used John Insecure's account in order to log into the Proton server.
#It turns out that John has access to server logs.
#Now, Bit wants to check from which workstation Pietraszko is frequently logging into the Proton server. Bit hopes that there will be some useful data.  

#Logs are in the `logs` dataset. 
#Consecutive columns contain information such as: who, when and from which computer logged into Proton.

#Problem 3: Check from which server Pietraszko logs into the Proton server most often.

#Use `proton(action = "server", host="XYZ")` command in order to learn more  about what can be found on the XYZ server.
#The biggest chance to find something interesting is to find a server from which Pietraszko logs in the most often.

table(logs, logs["host"])
sort(table(logs['host']), TRUE)[1]

proton(action = "server", host="194.29.178.91")
matrix(logs[logs['login']=="johnins"], ncol=3)


# Rozwiązane do zadania 3