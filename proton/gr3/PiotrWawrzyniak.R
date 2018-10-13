install.packages("BetaBit")
library(BetaBit)
proton()

employees[employees["surname"]=="Insecure"]
employees[employees["surname"]=="Pietraszko"]

proton(action = "login", login="johnins")

for(password in top1000passwords){
  proton(action = "login", login="johnins", password=password)
}


hosts = logs[logs["login"]=='slap',]['host']

sort(table(hosts),decreasing=TRUE)[1:3]


proton(action = "server", host="194.29.178.16")

bash_history

for(pass in unlist(strsplit(bash_history, " "))){
  proton(action = "login", login="slap", password=pass)
}
