library(ggplot2)
library(dplyr)

data <- read.csv('data/TWD_data_1.csv', sep = ',')
data_w <- read.csv('data/TWD_data_words.csv', sep = ',')

d1 <- data %>% 
  filter(year >= 1965, year <= 2017)

d1$racial_movies <- d1$racial_counter/d1$movies
d1$racial_words <- d1$racial_counter/d1$words
d1$non_vulgar_frac <- d1$non_vulgar_movies/d1$movies

data <- data %>%
  filter(year >= 1965, year <= 2017)

add_transparent_theme <- function(p) {
  p + theme(panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            axis.text = element_text(size = 18, color = "#D1D1D1"),
            axis.title = element_text(size = 18, color = "#D1D1D1"),
            rect = element_rect(fill = "transparent"),
            panel.background = element_rect(fill = "transparent"),
            plot.background = element_rect(fill = "transparent", color = NA),
            legend.background = element_rect(fill = "transparent"),
            legend.box.background = element_rect(fill = "transparent",color = NA),
            legend.key = element_rect(fill = '#D1D1D1'),
            legend.text=element_text(color="#D1D1D1",size=12),
            panel.border = element_rect(color = "#D1D1D1"),
            panel.grid.major.y = element_line(color = "#D1D1D1"),
            panel.grid.minor.y = element_line(color = "#D1D1D1"))
}

# -- Movies per year

p1 <- ggplot(d1,aes(x = year, y = movies)) +
  geom_bar(stat = 'identity', fill = '#FFAC32') + 
  theme_bw() +
  scale_x_continuous(expand = c(0,0), limits = c(1964, 2019), breaks = seq(1965, 2018, by = 5)) +
  scale_y_continuous(expand = c(0,0), limits = c(0, 9000), breaks = seq(0,9000,by = 1000)) +
  theme(plot.title = element_text(hjust = 0.5, size = 20)) +
  xlab("YEARS") +
  ylab("COUNT OF FILMS") 
p1 <- add_transparent_theme(p1)

ggsave(file="films.svg", plot=p1, width=10, height=8) 


# -- Hard_count cs Soft_count

p1 <- ggplot(data, aes(x=year)) +
  geom_line(aes(y=hard_count / movies, colour="HARD"), size = 1)+
  geom_line(aes(y=soft_counter / movies, colour="SOFT"), size = 1)+
  scale_x_continuous(breaks = seq(1965,2018,5))+
  scale_y_continuous(breaks = seq(0,6), limits = c(0,6.5), expand = c(0,0))+
  scale_color_manual(values = c("#8000c1","#ffac32"))+
  guides(colour=guide_legend(title=""))+
  labs(x="YEAR", y="AVG PER MOVIE") +
  theme(legend.position="none")
p1 <- add_transparent_theme(p1)
p1 <- p1 + theme(panel.grid.major.x = element_line(color = "#D1D1D1"))
ggsave(file="hardVsSoft.svg", plot=p1, width=10, height=6) 


# -- Nonvulgar films fraction

p1 <- ggplot(data, aes(x=year)) +
  geom_line(aes(y=non_vulgar_movies / movies), color="#8000c1", size = 1)+
  scale_x_continuous(breaks = seq(1965,2018,5))+
  scale_y_continuous(expand = c(0,0), limits = c(0,0.6), breaks = seq(0,0.6,by = 0.2)) +
  labs(x="YEAR", y="FRACTION")
p1 <- add_transparent_theme(p1)
p1 <- p1 + theme(panel.grid.major.x = element_line(color = "#D1D1D1"))
ggsave(file="nonvulgarFrac.svg", plot=p1, width=10, height=6) 


# -- Racial

p1 <- ggplot(data, aes(x=year)) +
  geom_line(aes(y=racial_counter / movies),color="#ffac32", size = 1)+
  geom_vline(xintercept=2008,linetype=2,color="#8000c1", size = 1) +
  geom_vline(xintercept=2016,linetype=2,color="#8000c1", size = 1) +
  scale_x_continuous(breaks = seq(1965,2018,5))+
  scale_y_continuous(limits = c(0,0.65), expand = c(0,0), breaks = seq(0,0.6,by= 0.2)) +
  guides(colour=guide_legend(title=""))+
  labs(x="YEAR", y="AVG PER MOVIE")
p1 <- add_transparent_theme(p1)
p1 <- p1 + theme(panel.grid.major.x = element_line(color = "#D1D1D1"))
ggsave(file="racial.svg", plot=p1, width=10, height=6) 
