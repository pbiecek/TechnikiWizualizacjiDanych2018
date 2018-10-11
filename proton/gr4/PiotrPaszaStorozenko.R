library('proton')

proton()
name <- "John Insecure"
login <- employees$login[employees$name == "John" & employees$surname == "Insecure"]
proton(action = "login", login=login)

pass <- top1000passwords[grep("john", top1000passwords)]
pass
proton(action = "login", login=login, password ="john")
r <- sapply(top1000passwords, function(p) proton(action = "login", login=login, password =p))
llgs <- logs[grep("slap", logs$login),]$host

llgs[table(llgs) == max(table(llgs))]
proton(action="server", host = "194.29.178.16")
