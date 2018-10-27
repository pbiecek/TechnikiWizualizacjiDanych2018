library(dplyr)
x <- employees
x[x["name"]=="John",]
proton(action="login", login="johnins")



a <- lapply(top1000passwords, function(x) proton(action = "login", login="johnins", password=x))

loginp <- employees[employees$name=="Slawomir",]

l <- logs[logs$login==loginp$login,]

library(data.table)

agg <- data.table(l)

agg <- agg[, .N, host]

host <- agg[1,1]

proton(action = "server", host=host)

