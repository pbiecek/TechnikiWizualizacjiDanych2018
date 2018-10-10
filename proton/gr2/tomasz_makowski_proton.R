install.packages('BetaBit')
library(BetaBit)
library(data.table)
install.packages('data.table')
proton()
emp <- as.data.table(employees)
login <- emp[name=='John' & surname=='Insecure',login]
proton(action = "login", login=login)
pas <- as.data.table(top1000passwords)
good_i <- 0
bad_mes <- proton(action = "login", login=login, password=pas[3,top1000passwords])
for (i in 1:1000) {
  a <- print(proton(action = "login", login=login, password=pas[i,top1000passwords]))
  if (a!=bad_mes) {
    good_i <- i
  }
}
password <- pas[good_i,top1000passwords]
proton(action = "login", login=login, password=password)

log <- as.data.table(logs)
log[login==login]
