library('ggplot2')
library(RColorBrewer)
dane <- read.csv2('locations.csv', sep=',')
agg_loc <- aggregate(dane$weight, by=list(dane$location, dane$season), FUN=sum)
res <- data.frame(Group.1=c(), Group.2 =c(), x= c())
for(i in 1:7)
{
  data <- agg_loc[agg_loc$Group.2 == i,]
  data$Group.1 <- factor(data$Group.1, levels =c(levels(data$Group.1), "Inna"))
  to_merge <- data$x < 2500
  data$Group.1[to_merge] <- 'Inna'
  data$x <- data$x/sum(data$x)
  res <- rbind(res, data)
}
agg_loc <- aggregate(res$x, by=list(res$Group.1, res$Group.2), FUN=sum)
agg_loc$x <- agg_loc$x * 100
colnames(agg_loc) <- c("Lokacja", "Sezon", "Procent")
ggplot(agg_loc, aes(fill=Lokacja, x=Sezon, y=Procent)) +
  geom_bar(stat = 'identity') +
  scale_x_continuous(breaks=1:7) +
  scale_y_continuous(breaks = seq(0,100,by=10)) +
  scale_fill_manual(values=brewer.pal(11, "Spectral")) +
  ggtitle("Procent czasu akcji serialu odbywającej się w danej lokacji w poszczególnych sezonach") +
  theme(rect = element_rect(fill = "transparent")) +
  theme_dark() +
  theme(axis.text=element_text(size=20),
        axis.title=element_text(size=20)) +
  theme(plot.title = element_text(size=20, face='bold')) +
  theme(legend.text=element_text(size=20)) +
  theme(legend.position = 'bottom') +
  theme(legend.title = element_text(size=20)) +
  theme(rect = element_rect(fill = "transparent")) +
  theme(plot.title = element_text(size=20, face='bold')) +
  theme(legend.text=element_text(size=20)) +
  theme(
    panel.background = element_rect(fill = "transparent",colour = NA),
    plot.background = element_rect(fill = "transparent",colour = NA),
    legend.background = element_rect(fill = "transparent",colour = NA)
  )

ggsave(filename = 'locations.png', device=png(), scale = 2, bg='transparent')
  