library(BetaBit)
proton()
head(employees)
login <- employees[employees$name=="John" & employees$surname=="Insecure",]
login
login <- "johnins"
proton(action = "login", login="johnins")
proton(action = "login", login = "johnins")
vector <- top1000passwords
vector
proton(action = "login", login="johnins", password="")

funkcja <- function(c) {
  pass <- toString(c)
  proton(action = "login", login="johnins", password=pass)
}

v <- lapply(vector, funkcja)
head(v)
logs