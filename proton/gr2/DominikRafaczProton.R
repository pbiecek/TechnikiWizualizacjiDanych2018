install.packages("BetaBit")
library(BetaBit)

#ZADANIE1

log <- employees[employees$name == "John" & employees$surname == "Insecure", 3][[1]]
proton(action = "login", login = log)

#ZADANIE2

i <- 1
t <- proton(action = "login", login = log, password = top1000passwords[1])
while(proton(action = "login", login = log, password = top1000passwords[i])== t){
  i <- i+1
}
passwd <- top1000passwords[i]

##p <- lapply(as.list(top1000passwords), function(x) {ifelse(proton(action = "login", login = log, password = x) == t, 0, 1)})


#ZADANIE3

logs2 <- logs[logs$login ==log,]
lll <- split.data.frame(logs2, f = factor(logs2$host))
h <- lapply(X = lll, FUN = ) # nie zdążyłem napisać funkcji sumującej

proton(action="server", host=h)
