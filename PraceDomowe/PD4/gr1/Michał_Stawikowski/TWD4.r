library(ggplot2)
library(gridExtra)
library(grid)
library(tidyverse)

where <- c("Discount stores", "Halloween stores", "Grocery stores", "Department stores", "Online",
           "Craft and fabric stores", "Cloting stores", "Home decor stores", "Trift and rasale shops", 
           "Drug shops", "Gift shops", "Small buisnesses", "Home improvments stores", "Catalogs")

percentage <- c(47.1, 37.5, 25.4, 24.4, 22.3, 13.7, 11.1, 10.8, 10.8, 9.1, 8.5, 7.2, 6.2, 3.9)
data <- data.frame(where, percentage)
data$where <- factor(data$where, levels = rev(data$where))

p <- ggplot(data, aes(x=where, y=percentage,)) + geom_bar(stat = "identity") + coord_flip() +
  scale_y_continuous(limits = c(0,65), breaks = seq(0,70,15), labels = paste0(seq(0,70,15), "%")) +
  geom_text(aes(label=paste0(data$percentage, "%"),y = percentage + 4), size = 3.5,
            position = position_dodge(0.9),
            vjust = 0.4, color = "black") + theme_minimal() +
  theme(legend.position="none",axis.title.y.left  = element_text(size =rel(1.4))) +  ylab("") + xlab("")

q <- ggplot(data, aes(x=where, y=percentage )) + geom_bar(stat = "identity",aes(fill=as.character(c(1,2,3,4,1,2,3,4,1,2,3,4,1,2)))) + coord_flip() +
  scale_y_continuous(limits = c(0,65), breaks = seq(0,60,15), labels = paste0(seq(0,60,15), "%")) +
  geom_text(aes(label=paste0(data$percentage, "%"),y = percentage + 4), size = 3.5,
            position = position_dodge(0.9),
            vjust = 0.4, color = "black")  + theme_minimal() + scale_color_manual(values = brewer.pal(3, "Accent")) +
  theme(legend.position="none") +xlab("") + ylab("") + scale_fill_brewer(type = "qual" , palette = 6, direction = 1,
                                                                                            aesthetics = "fill")

grid.arrange(p, q,
             ncol = 2,
             top=textGrob(" Where people in US shop to buy for the Halloween?",gp=gpar(fontsize=20)))