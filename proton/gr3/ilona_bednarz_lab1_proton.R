install.packages('BetaBit')
library(BetaBit)

proton()

# zad 1
head(employees)
proton(action = "login", login=employees[employees$surname=='Insecure',3])

# zad 2
top1000passwords[1:10]
sapply(top1000passwords, function(x) {
  # print(x)
  proton(action = "login", login="johnins", password=x)
})

# zad 3

head(logs)
tab <- logs[logs$login == employees[employees$surname=='Pietraszko',3],]
sort(table(tab$host))
proton(action = "server", host="194.29.178.16")



# zad 4
commands <- sapply(bash_history, function(z) strsplit(z,' ')[[1]][1])
typeof(commands)
commands <- unlist(commands)
commands[commands %in% top1000passwords]

