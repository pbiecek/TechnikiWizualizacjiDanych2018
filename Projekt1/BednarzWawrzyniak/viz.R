genre_df <- read.csv("genrecountry.csv")
df <- read.csv("films.csv")

library(dplyr)
library(tidyr)
library(countrycode)
library(rworldmap)
library(ggplot2)
library(svglite)

# --- world map ---
df2 <- genre_df %>% 
  mutate(genres = strsplit(as.character(genres), ", ")) %>%
  unnest(genres) %>%
  mutate(countries = strsplit(as.character(countries), ", ")) %>%
  unnest(countries)

df3 <- df2 %>%
  inner_join(df, by = "title")

df4 <- df2 %>%
  inner_join(df, by = "title") %>%
  mutate(countries=countrycode(countries, "country.name", "region")) %>%
  group_by(countries) %>%
  summarise(money = sum(1.0 * lifetime_gross, na.rm = TRUE)) %>%
  arrange(-money)

map.world <- map_data(map="world")
map.world$id <- 1:nrow(map.world)
map.world$countries <- countrycode(map.world$region, "country.name", "region")
map.world$countries[map.world$countries == "Caribbean"] <- "Central America"
map.world$countries[map.world$countries == "Polynesia"] <- "Australia and New Zealand"
map.world$countries[map.world$countries == "Micronesia"] <- "Australia and New Zealand"
map.world$countries[map.world$countries == "Melanesia"] <- "Australia and New Zealand"
map.world$countries[map.world$countries == "Central Asia"] <- "Eastern Asia"

map.world <- map.world %>%
  left_join(df4, by = "countries") %>%
  arrange(id) %>%
  select(long, lat, money, region, id, group)

gg <- ggplot(map.world) +
  theme_minimal() +
  geom_map(map=map.world, aes(map_id=region, x=long, y=lat, fill=log(money))) +
  scale_fill_gradient(low = "green", high = "brown3", guide = "colourbar") +
  coord_equal()
gg

ggsave("world.svg", plot=gg, width=10, height=10)
# --- world map ---

# --- gross by genre ---
df_genre_money <- df3 %>% group_by(genres) %>% summarize(money = mean(1.0 * lifetime_gross, na.rm = TRUE)) %>% arrange(-money)
df_genre_money$genres <- factor(df_genre_money$genres, levels = df_genre_money$genres)

gg <- ggplot(df_genre_money[1:10, ]) +
  geom_bar(aes(genres, money), stat="identity") +
  theme_minimal()
gg
ggsave("genres.svg", plot=gg, width=10, height=10)
# --- gross by genre ---

# --- titles ---
social <- c(
  "Avatar",
  "United Passions",
  "The Dark Knight", 
  "Rudderless",
  "Justin Bieber: Never Say Never",
  "Black November", 
  "High School Musical 3: Senior Year",
  "Twilight",
  "The Legend of Tarzan",
  "Crank: High Voltage",
  "Circle",
  "Agora",
  "After Life",
  "New York",
  "Spotlight",
  "The Last Godfather",
  "Christmas Eve",
  "Windsor Drive",
  "Area 51",
  "Saint John of Las Vegas",
  "Cheap Thrills",
  "Samsara",
  "Khailadi",
  "Max Payne",
  "The Amazing Spider-Man",
  "2012"
)

gg <- ggplot(df[df$title %in% social, ] %>% unique()) +
  geom_text(aes(log(lifetime_gross), imdb_score, label=title)) +
  theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank()) + 
  expand_limits(x = c(5, 20), y = c(0, 10))
gg
ggsave("titles.svg", plot=gg, width=10, height=10)
# --- titles ---