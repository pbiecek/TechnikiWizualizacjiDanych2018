data <- read.csv('data_apper.csv')
rownames(data) = data[,1]
#data = data[,-1]
#dt = t(data)
#dt = as.data.frame(dt)
#films = rownames(dt)
#dt$films = films
library(ggplot2)
library(xkcd)
library(ggthemes)
library(reshape2)
library(dplyr)
library(grid)
colnames(data) <- c("Actor", 1:12)
res = melt(data, id.vars = 'Actor', variable.name = "Film")

actors = c("Sherlock Holmes", "John Watson", "Mrs Hudson", "Greg Lestrade", "Molly Hopper", "Mycroft Holmes", "Jim Moriarty", "Mary Morstan", "Irane Adler", "Charles Augustus Magnussen", "Eurus Holmes","Bill Wiggins")
films = c("A Study in Pink", "The Blind Banker", "The Great Game", "A Scandal in Balgravia", "The Hounds of Baskerville", "The Reichenbach Fall", "The Empty Hearse", "The Sign of Three", "His Last Vow", "The Six Thatchers", "The Lying Detective", "The Final Problem")
#library(dplyr)

lab = as.factor(rep(0, 192))
ggplot(res, aes(Actor, value)) + 
  geom_col(aes(fill = Film, color = lab)) +
  coord_flip() +
  xlim(rev(actors)) +
  scale_fill_discrete(labels = films) + 
  scale_color_manual(values = c("black"), breaks=NULL) +
  theme_bw() +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        ) 

library(reshape2)

filmnames = c("S1E1", "S1E2", "S1E3", "S2E1", "S2E2", "S2E3","S3E1", "S3E2", "S3E3", "S4E1", "S4E2", "S4E3")
res$FilmNames = sapply(1:nrow(res), function(i) if_else(res$value[i] == 1, filmnames[res$Film[i]], NULL))

svg("sherlock.svg", height = 12, width = 18)
ggplot(res, aes(Actor, Film, fill = Film,label = FilmNames)) + 
  geom_label(aes(colour = "white" , fontface = "bold",family = "mono"), size = 4.8) +
  geom_text(aes(label=""), show.legend = FALSE) +
  coord_flip() +
  xlim(rev(actors)) +
  scale_fill_grey(labels = films, end = 0.7) + 
  scale_size_continuous(breaks = NULL) +
  scale_color_manual(values = c("white"), breaks=NULL) +
  #theme_xkcd() +
  ylab("Episode") +
  theme_minimal() +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        text=element_text(size=28, family = "mono"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank())
dev.off()
