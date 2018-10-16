
install.packages("BetaBit")
library(BetaBit)

# task 1
proton()

head(employees)

login <- employees[(employees$name == "John") & (employees$surname == "Insecure"), 3]
proton(action = "login", login=login)

# task 2
sort(table(top1000passwords))
top1000passwords

unique(lapply(top1000passwords, function(password) {
  r <- proton(action = "login", login=login, password=password)
  if (r != "Password or login is incorrect" ) password
  else NA
}))

password <- 'q1w2e3r4t5'
proton(action = "login", login=login, password=password)

# task 3
head(logs)
employees[employees$surname == "Pietraszko", 3]

log <- logs[logs$login == "slap", ]$host
t <- table(log)

log[log == which.max(table(logs[logs$login == login, 2]))]
ip <- names(which.max(table(logs[logs$login == login, 2])))

proton(action = "server", host=ip)
