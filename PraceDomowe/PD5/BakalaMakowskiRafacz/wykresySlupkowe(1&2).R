library(ggplot2)

wyklady <- data.frame(sex = rep(c('Mężczyźni','Kobiety'),each=3),
                      doing = rep(c('Robi notatki','Słucha','Śpi'),2),
                      how_many = c(34,48,19,53,56,54))

ggplot(data=wyklady, aes(x=doing, y=how_many, fill=sex)) +
  geom_col(position = position_dodge()) + 
  labs(title = 'Co studenci robili na wykładzie') +
  theme(line=element_blank()) + 
  xlab("Co studenci robią") +
  ylab("Liczba studentów") +
  scale_fill_discrete(name="Płeć")

ggsave('wyklad.png')

kolos <- data.frame(sex = rep(c('Mężczyźni','Kobiety'),each=3),
                      doing = rep(c('Pisze kolosa','Ściąga','Płacze'),2),
                      how_many = c(13,54,43,56,54,53))

ggplot(data=kolos, aes(x=doing, y=how_many, fill=sex)) +
  geom_col() + 
  labs(title = 'Co studenci robili na kolosie') +
  theme(line=element_blank()) + 
  xlab("Co studenci robią") +
  ylab("Liczba studentów") +
  scale_fill_discrete(name="Płeć")

ggsave('kolos.png')
