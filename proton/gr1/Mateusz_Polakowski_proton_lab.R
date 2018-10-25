library(dplyr)

# Zadanie 1 - login
x <- as.data.frame(employees)
x[x$name == 'John' & x$surname == 'Insecure', ]
proton(action = "login", login = "johnins")

# Zadanie 2 - haslo
pass <- top1000passwords
# for(i in 1:1000) {
  proton(action = "login", login = "johnins", password = "q1w2e3r4t5")
# }

# Zadanie 3 - logi
head(logs)
logs %>% 
  filter(login == 'johnins') %>%
  count(host)

proton(action = "server", host = "194.29.178.13")
