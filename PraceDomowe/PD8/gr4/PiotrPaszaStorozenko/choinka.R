library(ggplot2)
library(latex2exp)
x <- -seq(0,10*pi, length.out = 1000)
y <- sapply(1:10, function(i) {
  i/40 * x + sin(x**2/20)
})

y_base <- seq(min(y[,10]), -min(y[,10]), length.out = 1000)
x_base <- -sin(y_base**2)/5 - 10*pi

x_trunk <- seq(-11*pi, -10*pi, length.out = 500) - 0.01
y_trunk <- sin(x_trunk**2/2) * 1.5

# Niestety for po i = 1:10 nie działał i to była najprostsza metoda
# Brązowy ogonek dodaje choince charakteru!!!
ggplot(mapping = aes(x=x,y=y[,1], color='foo')) +
  geom_line(aes(y=y[,2])) +
  geom_line(aes(y=y[,3])) +
  geom_line(aes(y=y[,4])) +
  geom_line(aes(y=y[,5])) +
  geom_line(aes(y=y[,6])) +
  geom_line(aes(y=y[,7])) +
  geom_line(aes(y=y[,8])) +
  geom_line(aes(y=y[,9])) +
  geom_line(aes(y=y[,10])) +
  geom_line(aes(y=-y[,1])) +
  geom_line(aes(y=-y[,2])) +
  geom_line(aes(y=-y[,3])) +
  geom_line(aes(y=-y[,4])) +
  geom_line(aes(y=-y[,5])) +
  geom_line(aes(y=-y[,6])) +
  geom_line(aes(y=-y[,7])) +
  geom_line(aes(y=-y[,8])) +
  geom_line(aes(y=-y[,9])) +
  geom_line(aes(y=-y[,10])) +
  geom_point(aes(x_base, y_base)) +
  geom_line(aes(x_trunk, y_trunk), color='brown') +
  coord_flip() +
  scale_color_manual(values = "forestgreen") +
  theme_void() +
  theme(text = element_text(family='monospace'), legend.position="none") + 
  ggtitle(TeX("^{sin}kwadratowa choinka"))
