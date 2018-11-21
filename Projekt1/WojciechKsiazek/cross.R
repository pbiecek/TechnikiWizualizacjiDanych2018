library(tidyverse)

timeline <- read_csv("timeline.csv")

screenplay <- read_csv("screenplay.csv")
summaries <-
  timeline %>% 
  mutate(duration = end - start) %>% 
  group_by(name) %>% 
  summarise(screenTime = sum(duration),
            firstAppearance = min(start)) %>% 
  mutate(screenTime = screenTime / 60000)


word_count <- screenplay %>% 
  group_by(name) %>% 
  count

names <- read_csv("names")
cross_table <- 
inner_join(word_count, names, by = c("name" = "screenplay")) %>% 
  inner_join(summaries, by  = c("timeline" = "name"))

ggplot() + 
  geom_point(data = cross_table, aes(screenTime, n)) + 
  geom_text(data = filter(cross_table, n > 800 | screenTime > 25), aes(screenTime, n,label = timeline), nudge_x = - 8, nudge_y = 150)+
  theme_bw() + labs(x = "Czas ekranowy w minutach", y = "Liczba wypowiedzianych słów") +
  ggtitle("Kto mówi w filmie i jak długo pojawia się na ekranie?", "Czas ekranowy kontra liczba wypowiedzianych słów")
