library(ggplot2)
library(ggrepel)
library(dplyr)
library(stringi)
#Wczytanie danych
data <- read.csv("../data/season_ranking.csv", header = TRUE, sep = "\t")
ep_ranking <- read.csv("../data/episode_ranking.csv", header = TRUE, sep = "\t")
lines <- read.csv("../data/the-office-lines - scripts.csv", header = TRUE)

#Obróbka danych
count_by_season <- lines %>% group_by(speaker, season) %>% count
names(count_by_season) <- c('speaker','season', 'lines')
count_by_season <- count_by_season %>% inner_join((lines[,c("season","episode")] %>% unique %>% group_by(season) %>% count), by = "season")
count_by_season$avg <- count_by_season$lines/count_by_season$n
count_by_season %>% group_by(season) %>% filter(avg == max(avg))
ep_ranking$season <- stri_sub(ep_ranking$Episode, from = 2, to = 2)
ep_ranking$season <- stri_paste("Season ", ep_ranking$season)
ep_ranking$Elo.score <- as.numeric(as.character(ep_ranking$Elo.score))
best_episodes <- ep_ranking %>% group_by(season) %>% filter(Elo.score == max(Elo.score))
worst_episodes <- ep_ranking %>% group_by(season) %>% filter(Elo.score == min(Elo.score))

#Przygotowanie wykresu
p <- ggplot(data = data, aes(x = Season, y = Elo.rank)) +
  geom_hline(yintercept=1000) +
  geom_boxplot(data = ep_ranking, aes(x = season, y = Elo.score), inherit.aes = FALSE) +
  # geom_bar(stat = "identity", fill = rgb(231/255, 150/255, 42/255), width = 0.8, alpha = 0.7) +
  geom_point(data = best_episodes, aes(x = season, y = Elo.score), inherit.aes = FALSE) +
  geom_label(data = best_episodes, aes(x = season, y = Elo.score+50, label=Episode.name), inherit.aes = FALSE, size = 3) +
  geom_point(data = worst_episodes, aes(x = season, y = Elo.score), inherit.aes = FALSE) +
  geom_label(data = worst_episodes[worst_episodes$season!="Season 9",], aes(x = season, y = Elo.score-50, label=Episode.name), inherit.aes = FALSE, size = 3) +
  geom_label(data = worst_episodes[worst_episodes$season=="Season 9",], aes(x = season, y = Elo.score-150, label=Episode.name), inherit.aes = FALSE, size = 3) +
  geom_point(data = ep_ranking[ep_ranking$Episode.name=="Goodbye, Michael",], aes(x = season, y = Elo.score), inherit.aes = FALSE, color = "red") +
  geom_label(data = ep_ranking[ep_ranking$Episode.name=="Goodbye, Michael",], aes(x = season, y = Elo.score-50, label=Episode.name), inherit.aes = FALSE, size = 3, hjust = 0.3, color = "red") +
  scale_y_continuous(limits = c(0,1600), expand = c(0.01, FALSE)) + 
  theme(axis.text.x = element_text(family = "cambria", color="azure4", size=10, angle=0), 
        axis.text.y = element_text(family = "cambria", color="azure4", size=10, angle = 60),
        axis.title.x = element_text(family = "cambria", size = 16),
        axis.title.y = element_text(family = "cambria", size = 16),
        plot.title = element_text(family = "cambria", size = 18, hjust = 0.5),
        panel.background = element_rect(fill="transparent", colour = NA),
        plot.background = element_rect(fill="transparent", colour = NA)) +
  labs(x = "", 
       y = "Liczba punktów rankingowych*",
       title = "Popularność sezonów serialu The Office")
#Zapisanie wykresu
#ggsave(plot = p, filename="Overtime_stats.svg", bg = "transparent")
