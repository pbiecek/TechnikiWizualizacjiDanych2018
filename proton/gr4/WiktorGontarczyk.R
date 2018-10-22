library(BetaBit)
BetaBit::proton()
emp <- BetaBit::employees
emp[emp[,"surname"]=="Pietraszko",]
proton(action="login", login="slap")
emp[emp[,"surname"]=="Insecure",]
proton(action="login", login="johnins")

unique(lapply(top1000passwords, function(pass) { ifelse(proton(action = "login", login="johnins", password=pass) != "Password or login is incorrect", pass, NA) }))
proton(action="login", login="johnins", password="q1w2e3r4t5")
