dane <- read.csv2('characters.csv', sep=',')
s <- dane[dane$season == 1,]
s <- s[order(s$weight, decreasing = TRUE),]
top <- head(s, 5)
for(i in 2:7)
{
  s <- dane[dane$season == i,]
  s <- s[order(s$weight, decreasing = TRUE),]
  top <- rbind(top, head(s, 5))
}
qq <- unique(c(as.character(top$c_1), as.character(top$c_2)))
print(unique(qq))
par(bg=NA) 
top$label <- paste0(top$c_1, ' i ', top$c_2)
top$comb <- paste(top$c_1, top$c_2)
top$name <- paste('Sezon ', top$season)
top$weight <- top$weight / 60
ggplot(data = top, aes(x = factor(weight), y = weight, fill='gold') ) +
  geom_bar(stat = "identity") +
  geom_text(aes(label=label), position = position_stack(vjust = 0.5)) +
  coord_flip() +
  facet_wrap(~name, scales = "free_y", ncol=1, strip.position="left") +
  ylab("Długość wspólnych scen w minutach") +
  ggtitle("Pary postaci najczęściej występujące razem w scenach w poszczególnych sezonach") +
  guides(fill=FALSE) +
  theme_dark() +
  theme(
    panel.background = element_rect(fill = "transparent",colour = NA),
    plot.background = element_rect(fill = "transparent",colour = NA),
    legend.background = element_rect(fill = "transparent",colour = NA)
  )+
  theme(axis.text=element_text(size=26),
        axis.title=element_text(size=26)) +
  theme(plot.title = element_text(size=26, face='bold')) +
  theme(legend.text=element_text(size=26)) +
  theme(legend.position = 'bottom') +
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title.y = element_blank(),
        strip.text = element_text(size = 10))+
  theme(rect = element_rect(fill = "transparent")) +
  theme(plot.title = element_text(size=20, face='bold')) +
  theme(legend.text=element_text(size=20))
ggsave(filename = 'characters.png', device=png(), scale = 2, bg='transparent')
