install.packages('BetaBit')
library(BetaBit)
proton()
head(employees)
employees[employees$surname == "Insecure",]

proton(action="login", login="johnins")

head(top1000passwords)

output <- sapply(top1000passwords, function(x){
  output = proton(action='login', login='johnins', password=x)
})

output_sort <- ifelse(output == 'Success! User is logged in!', output, 0)
output_sort

output = proton(action='login', login='johnins', password='q1w2e3r4t5')

head(logs)
library(plyr)
logsp <- logs[logs$login == 'johnins',]
counted <- count(logsp, "host")
head(counted)
counted[order(counted$freq),]

proton(action='server', host='194.29.178.13')
