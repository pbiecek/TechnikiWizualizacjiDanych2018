install.packages('BetaBit')
library(BetaBit)

login <-  employees[employees$surname=='Insecure',]$login
proton(action="login", login=employees[employees$surname=='Insecure',]$login)

sapply(top1000passwords, function(x) {proton(action='login',login=login,password=x)!="Password or login is incorrect"})

head(logs)

pietraszkologin <- employees[employees$surname=='Pietraszko',]$login

logs[logs$login==pietraszkologin,]

library(dplyr)

(logs %>% filter(login==pietraszkologin) %>% group_by(host) %>% summarize(count=n()))[1]

proton(action = "server", host="194.29.178.16")

sapply(bash_history, function(x) { })