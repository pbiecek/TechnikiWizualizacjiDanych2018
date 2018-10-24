employees$login
employees$login[employees$surname=='Insecure']
proton(action='login', login='johnins')
top1000passwords
for(pwd in top1000passwords)
proton(action='login', login='johnins', password=pwd)
table(logs_p$host)
logs_p
table(logs_p$host)
table(factor(logs_p$host))
proton(action = "server", host="194.29.178.13")
employees$login[employees$surname=='Pietraszko']
logs_p <- logs[logs$login=='slap',]
table(factor(logs_p$host))
proton(action = "server", host="194.29.178.16")
nrow(bash_history)
length(bash_history)
head(bash_history)
cmd <- strsplit(bash_history, " ")
pwd <- sapply(cmd, function(x)
  return(x[0])
)
