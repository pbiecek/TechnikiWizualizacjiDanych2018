library(BetaBit)
proton()

# zadanie 1
data <- as.data.frame(employees)
insecure_data <- data[data$name == 'John' & data$surname == 'Insecure',]
proton(action = 'login', login=insecure_data$login)

# zadanie 2
logins <- lapply(top1000passwords, function(x){proton(action = 'login', login=insecure_data$login, password=x)})

# zadanie 3
login <- data[data$surname == 'Pietraszko', "login"]
hosts <- logs[logs$login == login,]
host <- hosts[which.max(hosts$data),]
proton(action='server', host=host$host)
