install.packages("BetaBit")
library(BetaBit)
proton()
install.packages("data.table")
library(data.table)
emp <- as.data.table(employees)
emp[name == "John"]
proton(action = "login", login="johnins")

head(top1000passwords)
proton(action = "login", login="johnins", password=top1000passwords)
for(pswd in top1000passwords){
  proton(action = "login", login="johnins", password=pswd)
  print(pswd)
}
proton(action = "login", login="johnins", password="q1w2e3r4t5")

head(logs)
logs <- as.data.table(logs)
logs[name = "slpietraszko"]
lapply(logs$host, proton, action="server")
proton(action = "server", host="193.0.96.13.15")
logs$host
