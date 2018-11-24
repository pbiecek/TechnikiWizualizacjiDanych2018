library(HoRM)
library(data.table)
library(ggplot2)
library(RColorBrewer)
library(svglite)

data("JamesBond")
bond = data.table(JamesBond)
bond[Bond =='Roger Moore ']$Bond = 'Roger Moore'

breaks <- c(1961,1968,1970,1972,1986,1990,1994,2004,2016)
colors <- c("#999999", "grey", "#999999", "grey", "#999999", "white", "#999999", "grey")
MIN_Y <- 0
MAX_Y <- 50
points <- merge(rep(breaks, each = 2), c(MIN_Y, MAX_Y), by = NULL)
points <- cbind(points, rep(c(1,rep(1:9, each = 2))[1:18], 2))[c(2:17,20:35),]
points[17:32,]<-points[32:17,]
colnames(points) <- c("year", "killed", "group")
points$group <- as.factor(points$group)

lins <- data.frame(xbeg = rep(1961, 6), xend = rep(2016, 6), ybeg = seq(0, 50, 10), yend = seq(0, 50, 10))

ggplot() +
  geom_polygon(data = points, aes(x = year, y= killed, group = group, fill = group)) +
  scale_fill_manual(values = colors, guide = FALSE) +
  geom_segment(data = lins, aes(x = xbeg, y = ybeg, xend = xend, yend = yend), color= "white", alpha = 0.4) +
  geom_bar(data = bond, aes(x = Year, y = Kills_Bond), stat = "identity", fill = "#CC2229") +
  theme_minimal() +
  theme(plot.margin = margin(0.2,0.1,0.1,0.1, "cm"),
        axis.text.x = element_text(angle = 90, color = "black", family = "Anonymous Pro Bold", vjust = 0.5),
        axis.text.y = element_text(family = "Anonymous Pro Bold"),
        axis.title.y = element_text(face = "bold",family = "Anonymous Pro Bold", vjust = -1),
        axis.ticks.x = element_line(size = 1),
        axis.line.x = element_line(size = 2, lineend = "butt"),
        axis.title.x = element_blank()) +
  scale_y_continuous(name = "Liczba zabitych przez Bonda", breaks = seq(0, 50, 10), minor_breaks = NULL, expand=c(0,0)) +
  scale_x_continuous(breaks = bond$Year, minor_breaks = NULL, expand = c(0,0))

ggsave("bond_killed.svg", width = 11, height = 3)
dev.off()