library(dplyr)
library(ggplot2)
library(eurostat)

lp <- get_eurostat_geospatial(output_class = "df", resolution = "60", nuts_level = "all")

table(lp[["CNTR_CODE"]])

ggplot(lp, aes(x = long, y = lat, group = group, fill = CNTR_CODE)) + 
  geom_polygon()

# kraje poza europa
# https://ec.europa.eu/eurostat/documents/345175/501899/NUTS-regions-2015-EU28-CC-EFTA.png

nuts_levels <- lapply(0L:3, function(ith_code) 
  filter(lp, CNTR_CODE == "PL", LEVL_CODE == ith_code) %>% 
    ggplot(aes(x = long, y = lat, group = group, fill = NUTS_NAME)) + 
    geom_polygon(color = "black") +
    ggtitle(paste0("LEVL_CODE = ", ith_code)) +
    coord_map()
)

nuts_levels[[4]]

filter(lp, CNTR_CODE == "PL", LEVL_CODE == 3) %>%
  group_by(NUTS_NAME) %>% 
  mutate(nice_label = c(first(NUTS_NAME), rep("", length(NUTS_NAME) - 1))) %>% 
  ggplot(aes(x = long, y = lat, group = group, fill = NUTS_NAME, label = nice_label)) + 
  geom_polygon(color = "black") +
  geom_text(stat = "unique") +
  coord_map()

