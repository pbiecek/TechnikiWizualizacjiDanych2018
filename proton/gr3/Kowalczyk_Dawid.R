
install.packages("BetaBit")
library(BetaBit)
proton()

employees[employees$name == "John" & employees$surname == "Insecure", 3]


proton(action = "login", login="johnins")

top1000passwords


for (i in 1:length(top1000passwords)){
  proton(action = "login", login="johnins", password=top1000passwords[i])
}



head(logs)

sort(table(logs[logs$login == "pietraszko",2]))

proton(action = "server", host="194.29.178.16")

