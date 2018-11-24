source('./data.R')

library(data.table)
library(ggplot2)

characters <- as.data.table(characters)
characters[gender=="","gender"] <- NA
characters[race=="","race"] <- NA
characters <- na.omit(characters, cols = c("gender", "race"))

rollup(characters, j = list(count = .N), by = "gender")  # ile kobiet, ile mezczyzn

race_gender <- rollup(na.omit(characters), j = list(count = .N), by = c("race","gender"))[!is.na(gender)]
race_gender <- race_gender[gender == "Male" | gender == "Female",]

ggplot(data = race_gender, aes(x = gender,y = count)) + geom_col() + facet_wrap(~race)
