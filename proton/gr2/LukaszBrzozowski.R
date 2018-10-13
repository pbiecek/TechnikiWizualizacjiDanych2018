install.packages("BetaBit")
library(BetaBit)
proton()
#Zadanie 1
which(employees[,2]=="Insecure")
employees[217,]
proton(action = "login", login = "johnins")
#Zadanie 2
top1000passwords
for(haslo in top1000passwords){
  proton(action = "login", login = "johnins", password=haslo)
}
#Zadanie 3
logs
tabela <- as.data.frame(table(logs[1:2]))
head(tabela)
