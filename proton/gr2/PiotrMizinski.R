library(BetaBit)
proton()

##1
head(employees)
typeof(employees)

employees$login[employees$surname=="Insecure"]
proton(action = "login", login="johnins")

##2
head(top1000passwords)

for (pass in top1000passwords){
  proton(action = "login", login="johnins", password=pass)
}

##3
head(logs)
logs%>%count(host)
