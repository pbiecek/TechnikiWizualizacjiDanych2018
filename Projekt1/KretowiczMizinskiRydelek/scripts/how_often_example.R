source('./data.R')
source('./how_often.R')

library(data.table)
library(ggplot2)
library(stringi)

temp <- how_often_per_word(scripts, regex = "(ring|treasure|precious)")

ggplot(data = temp, aes(x = char, y = average)) + geom_col() + coord_flip()
