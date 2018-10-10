library("BetaBit")
library("stringi")
proton()

johnsLogin <- employees[employees$name=="John" & employees$surname=="Insecure", ]$login
proton(action = "login", login = johnsLogin)

for (i in 1:length(top1000passwords)) {
  proton(action = "login", login = johnsLogin, password = top1000passwords[i])
}
slaweksLogin <- employees[employees$surname=="Pietraszko", ]$login
serverIP <- names(which.max(table(logs[logs$login==slaweksLogin, ]$host)))
proton(action = "server", host = serverIP)

bashe <- as.vector(stri_match(bash_history, regex = "^[A-z]+"))
komendy <- c("mcedit", "pwd", "vim", "rm", "cat", "ls", "vi", "cp")
bashe[!bashe %in% komendy]
