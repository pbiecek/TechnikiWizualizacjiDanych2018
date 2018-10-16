install.packages("BetaBit")
library(BetaBit)
proton()
head(employees)
employees[employees["surname"] == "Insecure"]
login <- "johnins"
proton(action = "login", login = login)

head(top1000passwords)
msg <- "Success! User is logged in!"

for(haslo in top1000passwords) {
  if (proton(action = "login", login=login, password=haslo) == msg) {
    print(haslo)
  }
}

pass <- "q1w2e3r4t5"
head(logs)

