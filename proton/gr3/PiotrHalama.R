install.packages("BetaBit")
library(BetaBit)
proton()

login <- employees[employees$name == "John" & employees$surname == "Insecure", "login"]
proton(action="login", login=login)

password <- top1000passwords[lapply(top1000passwords, function(pass) proton(action="login", login=login, password=pass)) == "Success! User is logged in!"][1]
proton(action="login", login=login, password=password)

login <- employees[employees$surname == "Pietraszko", "login"]
host <- names(which.max(table(logs[logs$login == login, "host"])))
proton(action="server", host=host)
