library(BetaBit)

proton()

employees[,]
#John Insecure
a <- employees[employees$name=="John" & employees$surname=="Insecure", ]

proton(action = "login", login="johnins")

top1000passwords


proton(action = "login", login="johnins", password=)
lapply(top1000passwords, function(x) proton(action="login", login="johnins", password=x))


#
#Logs are in the `logs` dataset. 
#Consecutive columns contain information such as: who, when and from which computer logged into Proton.
#
#Problem 3: Check from which server Pietraszko logs into the Proton server most often.
#
#Use `proton(action = "server", host="XYZ")` command in order to learn more  about what can be found on the XYZ server.
#The biggest chance to find something interesting is to find a server from which Pietraszko logs in the most often.
#

logs
head(logs)
