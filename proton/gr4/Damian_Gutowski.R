install.packages("BetaBit")
library(BetaBit)
proton()

#1 login John Insecure
login <- employees[(employees$name == "John") & (employees$surname == "Insecure"),3]
login
proton(action = "login", login=login)

#2

finalPassword = "";
for (password in top1000passwords) {
  result <- proton(action = "login", login=login, password=password)
  if(result != "Password or login is incorrect") {
    finalPassword <- password
    break
  }
}

#3
#3.1 Ligin Pietraszko
logs
login2 <-  employees[employees$surname == "Pietraszko",3]
login2
logs2 <- logs[logs$login == login2,2]
logs2
sort(summary(as.factor(logs2)),decreasing=T)[1]
 
          
         