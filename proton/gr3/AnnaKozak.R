#8 minut 
install.packages("BetaBit")
library("BetaBit")
proton()


employees[employees$name == "John" & employees$surname == "Insecure",]

proton(action = "login", login="johnins")

top1000passwords

for(i in 1: length(top1000passwords)){
  proton(action = "login", login="johnins", password=top1000passwords[i])
}

employees[employees$surname=="Pietraszko",]

library(dplyr)
logs %>% filter(login == "slap") %>% group_by(host) %>% summarize(n=n())

proton(action = "server", host="194.29.178.16")

bash_history <- bash_history

pass <- character(length(bash_history))
for(i in 1:length(bash_history)){
  pass[i]<- sub(" .*", "", bash_history[i])
}


pass1 <- unique(pass)

for(i in 1:length(pass1)){
  odp <- proton(action="login", login="slap", password=pass1[i])
}
