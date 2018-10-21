install.packages("BetaBit")
install.packages("data.table")
library(data.table)
library(BetaBit)
proton()

df <- data.table(employees)

df1 <- df[name == "John" & surname == "Insecure"]

vec <- top1000passwords
head(vec)