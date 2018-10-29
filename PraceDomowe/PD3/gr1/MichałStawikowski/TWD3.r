library(ggplot2)
library(gridExtra)
library("grid")

company <- c("Visa", "Ripple", "Paypal", "Others")
company2 <- c("BitcoinCash", "Litecoin","Dash", "Ethereum", "Bitcoin")
value2 <- c(60,56,48,20,7)
value <- c(24000,1500,193,60+56+48+20+7)
data <- data.frame(company, value)
data$company <- factor(data$company, levels = rev(data$company))
data2 <- data.frame(company2, value2)
data2$company2 <- factor(data2$company2, levels = rev(data2$company2))

p <- ggplot(data, aes(x=company, y=value )) + geom_bar(stat = "identity", aes(fill=company)) + coord_flip() +
  scale_y_continuous(limits = c(0,28000), breaks = seq(0,28000,4000)) +
  geom_text(aes(label=value, size = 10,y = value + 2500),
            position = position_dodge(0.9),
            vjust = 0, color = "black") + theme_minimal() +
  theme(legend.position="none",axis.title.y.left  = element_text(size =rel(1.4))) + ggtitle("Main companies") +xlab("Comapnies names") + ylab("")

q <- ggplot(data2, aes(x=company2, y=value2 )) + geom_bar(stat = "identity", aes(fill=company2)) + coord_flip() +
  scale_y_continuous() +
  geom_text(aes(label=value2, size = 10,y = value2+4),
            position = position_dodge(0.9),
            vjust = 0, color = "black")  + theme_minimal() +
  theme(legend.position="none") + ggtitle("Others") + xlab("") + ylab("")

grid.arrange(p, q,
          ncol = 2,
          top=textGrob(" Cryptocurrencies Transactions Per Seconds By Company",gp=gpar(fontsize=20)),
          bottom=textGrob("Transactions per second",gp=gpar(fontsize=14)))
