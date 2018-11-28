results <- data.frame(
  Answers <- factor(rep(c('Correct', 'Wrong', 'Passed'),6),levels = c("Correct", "Wrong", "Passed")),
  q <- rep(1:6, each = 3),
  r <- c(17.3, 23.1, 59.6, 11.5, 46.2, 42.3, 26.9, 32.7, 40.4, 78.8,  7.7, 13.5, 65.4, 13.5, 21.2, 42.3, 19.2, 38.5)
)

library(ggplot2)

breaks_x <- 1:6
breaks_y <- seq(0, 80, 10)
labels_y <- paste(breaks_y,"%",sep="")

#cairo_ps("Wykres.eps", height = 7.5, width = 8)

ggplot(data = results, aes(x = q, y = r, fill = Answers)) + geom_col(position = 'dodge') + scale_x_discrete(name = 'Questions') +
  scale_y_continuous(name = 'Percent', breaks = breaks_y, labels = labels_y) + theme_bw() + ggtitle("Results")

#dev.off()