cats <- as.factor(rep(c('nieustalony status', 'bierni zawodowo', 'bezrobotni', 'pracj¹cy'), 2))
f <- 16156.2
m <- 14851.9
male <- c(6, c(37.1, 7.3, 55.6)*0.94) / 100
femail <- c(4, c(52, 6, 42)*0.96) / 100
sex <- c(rep('mezczyzni', 4), rep('kobiety', 4))
df <- data.frame(c(male, femail), cats, sex)
colnames(df) <- c('val', 'cats', 'plec')
df$cats <- factor(df$cats, c('nieustalony status', 'bierni zawodowo', 'bezrobotni', 'pracj¹cy'))
library(ggplot2)
ggplot(data=df, aes(x=plec, y=val, fill=plec)) +
  geom_bar(stat="identity") +
  facet_wrap(~cats, nrow = 1) +
  scale_y_continuous(labels = scales::percent) +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        legend.position = 'bottom')

