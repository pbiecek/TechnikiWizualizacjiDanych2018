dane <- read.csv2('got_sex_death.csv', sep=',')
agg_death <- aggregate(dane$death, by=list(dane$season), FUN=sum)
agg_sex <- aggregate(dane$sex, by=list(dane$season), FUN=sum)
dane <-data.frame(sezon =1:7, sex =agg_sex$x[1:7], death =agg_death$x[1:7])
library('ggplot2')
library('reshape2')
par(bg=NA) 
dane <- melt(dane, id.vars='sezon', variable.name='Liczba')
ggplot(data = dane) +
  geom_line(aes(x=sezon, y=value, colour=Liczba), size=5) +
  geom_point(aes(x=sezon, y=value, colour=Liczba), size=8) +
  scale_x_continuous(breaks=1:7) +
  scale_y_continuous(breaks = seq(0,60,by=5))+
  ggtitle("Problemy dzielnic Warszawy") +
  scale_color_manual(values = c('#E75480', '#8A0707'), labels = c('Sceny seksu', 'Śmierci postaci')) +
  labs(x='Sezon', y='Liczba', colour = "", title = "Liczba scen seksu i śmierci postaci w poszczególnych sezonach") +
  theme_dark()+
  theme(
    panel.background = element_rect(fill = "transparent",colour = NA),
    plot.background = element_rect(fill = "transparent",colour = NA),
    legend.background = element_rect(fill = "transparent",colour = NA)
  )+
  theme(axis.text=element_text(size=26),
        axis.title=element_text(size=26)) +
  theme(plot.title = element_text(size=26, face='bold')) +
  theme(legend.text=element_text(size=26)) +
  theme(legend.position = 'bottom')
  
ggsave(filename = 'sexandviolence.png', device=png(), scale = 2, bg='transparent')
