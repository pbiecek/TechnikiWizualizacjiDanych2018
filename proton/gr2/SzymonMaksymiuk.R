proton()
b <- employees[employees$name == "John" & employees$surname == "Insecure",]["login"]
proton(action = "login", login=b)

for (i in top1000passwords) {
  m <- proton(action = "login", login=b, password=i)

  if (m == "Success! User is logged in!") {

    break()
  }
}
y <- employees[employees$name == "Slawomir" & employees$surname == "Pietraszko",]["login"]

    


