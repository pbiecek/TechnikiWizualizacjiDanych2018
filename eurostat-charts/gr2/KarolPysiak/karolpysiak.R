library(eurostat)
library(ggplot2)

lp <- get_eurostat_geospatial(output_class = "df", resolution = "60", nuts_level = "all")

table(lp[["CNTR_CODE"]])


s1 <- search_eurostat("music", type = "table")

s1

as.list(s1[1, ])

t1 <- get_eurostat(s1[1, "code"])

t1

left_join(lp, t1, by = c("geo" = "geo")) %>%
  na.omit() %>%
  filter(long > -30, lat > 30, LEVL_CODE == 0) %>% 
  ggplot(aes(x = long, y = lat, group = group, fill = values)) + 
  labs(x = "Longtitude", y = "Latitude", fill = "Percent") +
  geom_polygon(color = "black") +
  coord_map() +
  facet_wrap(~time) +
  theme(text = element_text(face = "bold")) +
  ggtitle("Individuals using the internet for playing or downloading games, \nimages, films or music (% of individuals aged 16 to 74)")
