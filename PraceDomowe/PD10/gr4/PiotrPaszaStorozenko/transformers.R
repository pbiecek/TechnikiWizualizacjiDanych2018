library(archivist)
library(tidyverse)
przezycia <- aread("pbiecek/Przewodnik/arepo/609491e5ec491f240cbeafe377743e21")  

as_tibble(przezycia) %>% 
    filter(Year == 2009) %>%
    mutate(Age = as.integer(as.character(Age))) %>% 
    mutate(Tx = if_else(Gender=='Male', Tx , -Tx)) %>% 
    ggplot(aes(x=Age, y=Tx, fill = Gender)) +
    geom_col() +
    coord_flip()


data <- as_tibble(przezycia) %>% 
    filter(Year == 2009) %>% 
    select(Age, Tx, Gender) %>% 
    mutate(Age = as.integer(as.character(Age))) %>% 
    mutate(Tx = if_else(Gender=='Male', Tx , -Tx)) %>% 
    spread(Gender,Tx) %>% mutate(diff = -(Male + Female)) %>% 
    gather('Sex', 'Tx',-Age)

data %>% ggplot(aes(x=Age, y=Tx, fill = Sex)) +
    geom_col() +
    coord_flip()


as_tibble(przezycia) %>% 
    filter(Year == 2009) %>%
    mutate(Age = as.integer(as.character(Age))) %>% 
    filter(Age < 100) %>% 
    select(Tx, Age, Gender) %>% write_csv('population.csv')
