library(BetaBit)
proton()

employees.data <- as.data.frame(employees)
john <- employees.data[data$name == 'John' & data$surname == 'Insecure',]
proton(action="login", login=john$login)

results <- lapply(top1000passwords, function (password) {proton(action="login", login=john$login, password=password)})unique(messages)
index <- which(messages == "Success! User is logged in!")
proton(action="login", login="johnins", password=top1000passwords[index])

pietraszko <- data[data$surname == 'Pietraszko', "login"]
pietraszkoLogs <- logs[logs$login == login,]
