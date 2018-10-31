BetaBit::proton()
login <- proton::employees[proton::employees$name == "John" & proton::employees$surname == "Insecure", "login"]
proton::proton(action = "login", login="johnins")
result = 1
for(i in seq(1, length(proton::top1000passwords), 1)) {
  if ((proton::proton(action = "login", login="johnins", 
                     password=proton::top1000passwords[i])) == 'Success! User is logged in!')
  {
    result <- i
  }
}
proton::top1000passwords[result]