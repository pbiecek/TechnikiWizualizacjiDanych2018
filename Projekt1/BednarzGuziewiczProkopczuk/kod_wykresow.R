library(ggplot2)
library(dplyr)
library(gender)
library(ggmap)
library(maps)
library(mapdata)
library(wordcloud2)

dem <- read.table('~/oscars.csv', sep = ',', header = TRUE, fill = TRUE,
                  na.strings = c('Na','', NA), stringsAsFactors = FALSE)
dem <- dem[,c(2,3,4,5,6,8,10,12,14,16,18,22,23)]

######### wyznaczanie zmiennych ########

# miejsce urodzenia
dem$birth_city <- sapply(dem$birthplace, function(x) strsplit(x, ', ')[[1]][1])
dem$birth_country_or_state <- sapply(dem$birthplace, function(x) {
  s <- strsplit(x, ', ')[[1]]
  s[length(s)]
})
dem$birth_usa <- ifelse(nchar(dem$birth_country_or_state) == 2, TRUE, FALSE)
sum(dem$birth_usa, na.rm = TRUE) # ponad polowe oskarow dostali amerykanie

# rok urodzenia
dem$birth_year <- sapply(dem$date_of_birth, function(x) as.numeric(strsplit(x, '-')[[1]][3])) # niektore sa dwucyfrowe :(

demyear <- dem[,c('date_of_birth', 'year_of_award','birth_year')]
demyear$award_century <- demyear$year_of_award %/% 100
demyear$award_short_year <- demyear$year_of_award %% 100
demyear$birth_year_full <- ifelse(nchar(as.character(demyear$birth_year)) == 4,
                                  demyear$birth_year,
                                  ifelse(demyear$award_short_year > demyear$birth_year,
                                         paste0(demyear$award_century, ifelse(nchar(demyear$birth_year)<2,paste0('0',demyear$birth_year),demyear$birth_year) ),
                                         paste0(demyear$award_century-1, ifelse(nchar(demyear$birth_year)<2,paste0('0',demyear$birth_year),demyear$birth_year))
                                  )
)

dem$birth_year <- demyear$birth_year_full
dem$oscar_age <- dem$year_of_award - as.numeric(dem$birth_year)

# plec
dem$name <- as.character(sapply(dem$person, function(x) strsplit(x, ' ')[[1]][1]))
g <- gender(names = dem$name)
g <- g[,c(1,4)]
dem <- left_join(dem, g[!duplicated(g$name),], by="name")
tail(dem)
names(dem)

################ wykres demograficzny #############
df <- dem[,c('gender',"oscar_age")]
df <- na.omit(df)
df1 <- df %>% group_by(gender, oscar_age) %>% summarise(n()) %>% arrange(desc(oscar_age))
names(df1)[3] <- "population"

ggplot(data = df1, aes(x = oscar_age, fill = gender)) + 
  geom_bar(data = dplyr::filter(df1, gender == "female"), aes(y=population),stat='identity',fill="#ec7014",width=0.5, position = position_dodge(width=2.9)) +
  geom_bar(data = dplyr::filter(df1, gender == "male"), aes(y = population * (-1)),stat='identity',fill="#41ab5d",width=0.5, position = position_dodge(width=2.9)) +
  scale_x_continuous(breaks = seq(10, 90, 5), labels = abs(seq(10, 90, 5))) +
  scale_y_continuous(breaks = seq(-20, 20, 4), labels = abs(seq(-20, 20, 4)), limits = c(-8,8)) +
  coord_flip() +
  theme_minimal() +
  theme(panel.background = element_rect(fill = 'black', colour = 'black'),
        plot.background = element_rect(fill = 'black', colour = 'black'),
        axis.text = element_text(colour = "#ffed6f",size=9),
        axis.title.x = element_text(colour = "#ffed6f", size=15),
        axis.title.y = element_text(colour = "#ffed6f", size=15),        
        panel.grid.minor.y = element_line(size=0.1, color = '#ffed6f'),
        panel.grid.minor.x = element_line(size=0.1, color = "#ffed6f"),
        panel.grid.major = element_line(size=0.1, color = "#ffed6f"),
        title = element_text(colour = "#ffed6f", size = 19)
  ) + 
  ylab('Liczba osób w danym wieku') +
  xlab('Wiek') +
  ggtitle("Rozkład płci i wieku zdobywców oskara")


################ mapa #############

oscars <- read.csv("oscars.csv")

locat<-geocode(as.character(oscars$birthplace), output="latlon", source = "dsk")
which(is.na(locat)) #50 i 59, CĚÁslav, slovakia
locat[c(50,59),1]=48.669026
locat[c(50,59),2]=19.699024

world <- map_data("world") 
d <- locat %>%
  group_by(lon, lat)  %>% summarise(n())
colnames(d)<-c("lon", "lat", "count")

ggplot() +
  geom_polygon(data = world, aes(x=long, y = lat, group = group), fill="#4D4D4D", alpha=1) +
  geom_point(data=d, aes(x=lon, y=lat, size=count, color=count,alpha = 1),col = "#cc4c02")+
  scale_size_continuous(range=c(1,12), guide = guide_legend(title="Liczba oskarów"))+
  scale_fill_gradient(low = '#fff7bc', high = '#cc4c02')+
  theme_void()+ 
  guides( colour = guide_legend()) +
  ggtitle("Miasto urodzenia laureatów oskarów") +
  theme(panel.background = element_rect(fill = 'black', colour = 'black'),
        plot.background = element_rect(fill = 'black', colour = 'black'),
        axis.text = element_blank(),
        title = element_text(colour = "#ffed6f", size = 19),
        legend.text = element_text(colour="#ffed6f", size = 10)
  )


################# gatunki filmow ##################

movies <- read.csv("movies.csv")
wordcloud2(movies, size = 1.5, color = c("#006837", "#f7fcb9", "#addd8e", "#ffffe5", "#78c679",
                                       "#41ab5d", "#238443", "#d9f0a3", "#004529", 
                                       "#006837", "#f7fcb9", "#addd8e", "#ffffe5", "#78c679",
                                       "#41ab5d", "#238443", "#d9f0a3", "#004529"), backgroundColor = "black")



