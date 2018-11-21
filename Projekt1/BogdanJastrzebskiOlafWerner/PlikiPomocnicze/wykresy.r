setwd("/home/bodo/Projekt1")
battles <- read.csv("battles.csv")
Chapters_per_Book <- read.csv("Chapters_per_Book.csv")
deaths <- read.csv("character-deaths.csv")
predictions <- read.csv("character-predictions.csv")

library("dplyr")
library(ggplot2)


# --------------------------------------------------------------------------------
# Gender

# Kobiety i mężczyźni 
deaths %>% filter(!is.na(Death.Chapter))%>%group_by(Gender)%>%summarise(n = n())

deaths %>% group_by(Gender) %>% summarise(n = n()) 

157/(157+760)

# Kobiety
Kobiety <- deaths %>% 
  filter(Gender == 0) %>% 
  mutate(Dead = !is.na(Death.Chapter)) %>% 
  group_by(Dead) %>% 
  summarise(n = n())

Kobiety_przeżywalność <- 1 - as.numeric(Kobiety[Kobiety$Dead==TRUE,2]/(Kobiety[Kobiety$Dead==TRUE,2]+Kobiety[Kobiety$Dead==FALSE,2]))
Kobiety_przeżywalność

pie(c(79,21))

# Męźczyźni
Męźczyźni <- deaths %>% 
  filter(Gender == 1) %>% 
  mutate(Dead = !is.na(Death.Chapter)) %>% 
  group_by(Dead) %>% 
  summarise(n = n())

Męźczyźni_przeżywalność <- 1 - as.numeric(Męźczyźni[Męźczyźni$Dead==TRUE,2]/(Męźczyźni[Męźczyźni$Dead==TRUE,2]+Męźczyźni[Męźczyźni$Dead==FALSE,2]))
Męźczyźni_przeżywalność
pie(c(65,35))

# --------------------------------------------------------------------------------
# Umieralność od rozdziału książki

Umieralność_po_rozdziałach <- deaths %>% 
  filter(!is.na(Death.Chapter)) %>% 
  group_by(Book.of.Death, Death.Chapter) %>% 
  summarise(n = n()) %>% 
  mutate(time = Book.of.Death + Death.Chapter/100)

names(deaths)

ggplot(deaths %>% filter(Book.of.Death == 1), aes(Death.Chapter)) + geom_bar()

# --------------------------------------------------------------------------------
# Szlachta

# Szlachta i nieszlachta 
deaths %>% filter(!is.na(Death.Chapter))%>%group_by(Nobility)%>%summarise(n = n())

# Szlachta
Szlachta <- deaths %>% 
  filter(Nobility == 0) %>% 
  mutate(Dead = !is.na(Death.Chapter)) %>% 
  group_by(Dead) %>% 
  summarise(n = n())

Szlachta_przeżywalność <- 1 - as.numeric(Szlachta[Szlachta$Dead==TRUE,2]/(Szlachta[Szlachta$Dead==TRUE,2]+Szlachta[Szlachta$Dead==FALSE,2]))
Szlachta_przeżywalność

pie(c(61,39))

# NieSzlachta
NieSzlachta <- deaths %>% 
  filter(Nobility == 1) %>% 
  mutate(Dead = !is.na(Death.Chapter)) %>% 
  group_by(Dead) %>% 
  summarise(n = n())

NieSzlachta_przeżywalność <- 1 - as.numeric(NieSzlachta[NieSzlachta$Dead==TRUE,2]/(NieSzlachta[NieSzlachta$Dead==TRUE,2]+NieSzlachta[NieSzlachta$Dead==FALSE,2]))
NieSzlachta_przeżywalność

pie(c(NieSzlachta_przeżywalność, 1 - NieSzlachta_przeżywalność))

# -------------------------------------------------------------------------------




