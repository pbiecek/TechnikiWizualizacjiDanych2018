install.packages("BetaBit")
library("BetaBit")
proton()

john_login <- employees[which(employees$name == 'John' & employees$surname == 'Insecure'),3]
proton(action='login',login=john_login)

for (pass in top1000passwords) {
  proton(action='login',login=john_login,password=pass)
}

pietraszko_login <- employees[which(employees$name == 'Slawomir' & employees$surname == 'Pietraszko'),3]
logs[logs$login == pietraszko_login,]
data.frame(table(logs[logs$login == pietraszko_login,]$host))
proton(action='server', host='194.29.178.16')
