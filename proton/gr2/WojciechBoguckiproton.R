library(BetaBit)
library(dplyr)
proton()

colnames(employees)
#filtrowanie z uzyciem pakietu dplyr
login <- filter(employees, name=="John", surname=="Insecure")["login"]
proton(action="login", login=login)

#problem 2
lapply(top1000passwords, function(x) {a <- proton(action="login", login=login, password=x)
       if (a=="Success! User is logged in!")stop()})


dim(johnlogs)
johnlogs <- logs[logs$login==unlist(login),]
tabulate(johnlogs$host)

