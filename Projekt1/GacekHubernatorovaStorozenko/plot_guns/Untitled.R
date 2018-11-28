library(ggplot2)
library(ggimage)
library(stringr)
library(grid)

types_of_crimes <- data.frame("type"=c("Murder, attempted murder", "Kidnapping",
                                       "Bomb attack", "Theft", "Blackmail, threats", "Terrorist attack"),
                              "image"=c('~/twiz/projekt1/handgun.jpg', '~/twiz/projekt1/kidnap2.png', '~/twiz/projekt1/bomb.jpg',
                                        '~/twiz/projekt1/bag.jpg', '~/twiz/projekt1/blackmail.jpeg',
                                        '~/twiz/projekt1/terrorist.jpg'),
                              "numbers"=c(11, 5, 2, 1, 5, 1))
types_of_crimes$type <- str_wrap(types_of_crimes$type, width = 16)
#types_of_crimes$type <- str_pad(types_of_crimes$type, width = 14, 'left')
types_of_crimes$type <- factor(types_of_crimes$type, levels = types_of_crimes$type)
types_of_crimes <- types_of_crimes[order(types_of_crimes$numbers),]

ggplot(types_of_crimes, aes(type, numbers)) + 
  geom_bar(fill='gray30', stat='identity', width=0.4) + 
  expand_limits(y = -1.5)  +
  geom_text(aes(label=numbers), position=position_dodge(width=0.7), hjust=1.5, colour='white',
            family='mono', size=5) +
  theme_transparent() +
  geom_image(aes(x=type,image=image), y=-1, size=.1) +
  ylab("") + 
  theme(text=element_text(size=16,  family="mono"), plot.title = element_text(hjust = 0.5),
        axis.text.x = element_blank(), axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank()) +
  xlab("") + ggtitle("Number of crimes committed") +
  coord_flip()

