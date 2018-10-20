library(BetaBit)
proton()
login<-employees[employees$name=="John" & employees$surname=="Insecure",3]
proton(action="login",login=login)
passwd<-top1000passwords[1]
i<-1
while (proton(action = "login", login=login, password=passwd)!="Password or login is incorrect!") {
  i<-i+1
  passwd<-top1000passwords[i]

  }


