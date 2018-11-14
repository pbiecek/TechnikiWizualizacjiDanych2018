library(dplyr)
library(ggplot2)
library(eurostat)

lp <- get_eurostat_geospatial(output_class = "df", resolution = "60", nuts_level = "all")

s1 <- search_eurostat("death", type = "table")

s1

as.list(s1[4, ])

t1 <- get_eurostat(s1[4, "code"]) %>%  filter(age == "Y15-19", time == "2015-01-01")


names_df <-left_join(lp, t1, by = c("geo" = "geo")) %>% 
  filter(long>-25, lat>35) %>%
  na.omit %>% group_by(CNTR_CODE) %>% 
  summarise(long = mean(long),
            lat = mean(lat))

left_join(lp, t1, by = c("geo" = "geo")) %>% 
  na.omit %>% 
  ggplot(aes(x = long, y = lat, group = group, fill = values)) + 
  scale_fill_distiller(type = "div", palette = 8)+
  geom_polygon(color = "black") +
  geom_text(data = names_df, aes(x = long, y = lat, label = CNTR_CODE), inherit.aes = FALSE) +
  coord_map() + 
  ggtitle("Suicide death rate by age group 15-19, per 100 000 persons in 2015")

