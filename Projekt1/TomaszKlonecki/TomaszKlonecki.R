rm(list=ls())
options("stringsAsFactors"=FALSE)
library(subtools) # biblioteka do obslugi napisow
library(tm) # biblioteka do obslugi formatu TM
library(dplyr) 
library(ggplot2)
library(sqldf)
library(reshape2)

### Get subtitles ready

a <- read.subtitles.season(dir = "/subs/all")

c <- tmCorpus(a)

c <- tm_map(c, content_transformer(tolower))
c <- tm_map(c, removeWords, stopwords("english"))
c <- tm_map(c, removePunctuation)
c <- tm_map(c, removeNumbers)
c <- tm_map(c, removeWords, words = c("", "i"))
c <- tm_map(c, stripWhitespace)
c <- tm_map(c, removeWords, stopwords("english"))

t <- as.vector(c$content)
t2 <- strsplit(t," ")

names(t2)[1] <- "season_1"
episode_nr <- rep(1,length(t2[[1]]))


for(i in 2:length(t2)){
  names(t2)[i] <- paste("season_", i, sep = "")
  episode_nr <- c(episode_nr,rep(i,length(t2[[i]])))
}

df <- data.frame(matrix(unlist(t2, use.names = TRUE), byrow=T))
colnames(df) <- "word"
df <- cbind(df, episode_nr)
rm(a,c,episodes_iterator,t2)

### Laura occurrences
df_laura <- df %>%
  group_by(episode_nr,
           word) %>%
  summarise(Laura = n()) %>%
  arrange(desc(Laura)) %>%
  filter(#Laura >= 50
    word == "laura"
  )

episodes_iterator <- data.frame(1:30)
colnames(episodes_iterator) <- "episode_nr"
df_laura <- left_join(episodes_iterator, df_laura)
df_laura$word <- "laura"
df_laura[is.na(df_laura)] <- 0
rm(episodes_iterator,episode_nr,i)

### Bob occurrences
df_ben <- df %>%
  group_by(episode_nr,
           word) %>%
  summarise(Laura = n()) %>%
  arrange(desc(Laura)) %>%
  filter(#tot >= 50
    word == "bob"
  )
episodes_iterator <- data.frame(1:30)
colnames(episodes_iterator) <- "episode_nr"
df_ben <- left_join(episodes_iterator, df_ben)
df_ben$word <- "bob"
df_ben[is.na(df_ben)] <- 0

colnames(df_ben) <- c("episode_nr","series","value")
#df_ben2 <- rbind(df_ben,df_laura)


### Coffee and food

episode_nr <- 1:30
coffee <- c(17,14,17,10,7,12,6,3,13,10,4,8,3,4,9,3,1,4,5,4,5,9,5,11,5,3,14,3,4,3)
pie <- c(0,1,7,3,0,0,0,0,1,0,0,0,0,2,1,1,1,0,3,1,0,1,1,0,1,2,14,1,0,1)
donut <- c(2,3,0,0,0,4,0,0,0,0,0,0,1,0,6,0,0,1,0,1,2,0,0,1,0,0,0,0,0,0)
beer <- c(17,0,0,4,0,0,5,2,0,0,0,1,0,0,0,1,0,0,0,2,1,0,0,3,0,0,0,0,3,0)
coop_coffee <- c(1,2,1,1,1,1,1,0,
                       0,0,0,0,0,0,0,0,0,0,
                       0,0,0,1,1,0,0,0,1,1,0,1)
quant <- data.frame(episode_nr, coffee, beer, pie, donut)
df_laura2 <- df_laura[,c("episode_nr", "Laura")]
quant2 <- merge.data.frame(quant,df_laura2)

quant3 <- melt(quant2 ,  id = 'episode_nr', variable.name = 'series')
quant3[,"series"] <- as.character(quant3[,"series"])
# quant3$ord <- 1
# quant3[quant3$series == 'coffee',4] <- 2
# quant3[quant3$series == 'beer',4] <- 3
# quant3[quant3$series == 'donut',4] <- 4
# quant3[quant3$series == 'laura',4] <- 5
# quant3<-quant3[order(quant3$ord),]

quant4 <- quant3 %>%
  filter(series %in% c("tot", "total_coffee"))

#reorder(episode_nr, episode_nr)
df_ben2 <- rbind(df_ben,quant3[quant3$series == 'coffee',])


### Final plots

p <- ggplot(data = quant3, aes(x = episode_nr, y = value, group = series,color = series, size = series, linetype = series)) + 
  geom_step() +
  scale_size_manual(values=c(1,1,1,2,1)) +
  #scale_size_manual(values=c(1.5,1.5,1.5,2.5,1.5)) +
  scale_color_manual(values=c('goldenrod1','black','plum','firebrick3','steelblue')) +
  scale_linetype_manual(values=c("solid", "solid", "solid", "solid", "solid")) +
  theme_minimal() +
  ggtitle("Mentions about Laura and occurences of particular drinks and food") +
  labs(x = "Episode", y = "Occurences") +
  theme(legend.position = c(0.9, 0.8),
        legend.title = element_blank())
p
#ggsave("laura_all.pdf",device = "pdf",width = 20, height = 15, units = "cm")

p1 <- ggplot(data = quant3[quant3$series %in% c("Laura",'coffee'),], aes(x = episode_nr, y = value, group = series,color = series, size = series, linetype = series)) + 
  geom_step() +
  scale_size_manual(values=c(1.5,2.5)) +
  scale_color_manual(values=c('black','firebrick3')) +
  scale_linetype_manual(values=c("solid", "solid")) +
  theme_minimal() +
  ggtitle("Occurences of Laura and coffee") +
  labs(x = "Episode", y = "Occurences") +
  theme(legend.position = c(0.9, 0.8),
        legend.title = element_blank())
p1
#ggsave("laura_coffee.pdf",device = "pdf",width = 20, height = 15, units = "cm")

p3 <- ggplot(data = df_ben2, aes(x = episode_nr, y = value, group = series,color = series, size = series, linetype = series)) + 
  geom_step() +
  scale_size_manual(values=c(1,2)) +
  scale_color_manual(values=c('mediumorchid3','black')) +
  scale_linetype_manual(values=c("solid", "solid")) +
  theme_minimal() +
  ggtitle("Occurences of coffee and Bob (the Killer)") +
  labs(x = "Episode", y = "Occurences") +
  theme(legend.position = c(0.9, 0.8),
        legend.title = element_blank())
p3
#ggsave("coffee_bob.pdf",device = "pdf",width = 20, height = 15, units = "cm")

p4 <- ggplot(data = quant3[quant3$series %in% c("beer",'coffee',"pie","donut"),], aes(x = episode_nr, y = value, group = series,color = series, size = series, linetype = series)) + 
  geom_step() +
  scale_size_manual(values=c(1.5,1.5,1.5,1.5,1.5)) +
  scale_color_manual(values=c('goldenrod1','black','plum','steelblue')) +
  scale_linetype_manual(values=c("solid", "solid", "solid", "solid")) +
  theme_minimal() +
  ggtitle("Occurences of particular food and drinks") +
  labs(x = "Episode", y = "Occurences") +
  theme(legend.position = c(0.9, 0.8),
        legend.title = element_blank()) 
p4
#ggsave("food.pdf",device = "pdf",width = 20, height = 15, units = "cm")
