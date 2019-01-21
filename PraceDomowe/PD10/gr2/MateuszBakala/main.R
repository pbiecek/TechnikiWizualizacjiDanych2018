library(r2d3)

przezyciaTrimmed <- read.csv("przezyciaUciete.csv")

r2d3(data = przezyciaTrimmed, script = "gender.js")
