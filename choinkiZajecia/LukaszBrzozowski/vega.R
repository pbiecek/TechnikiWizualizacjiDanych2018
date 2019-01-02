library(vegalite)
library(jsonlite)
library(stringr)
library(dplyr)

dat <- fromJSON("https://vega.github.io/vega-editor/app/data/unemployment-across-industries.json")
dat02 <- dat %>% mutate(date = as.Date(dat$date)) %>% filter(year <= 2006)
dat01 <- dat02 %>% mutate(count = count/((2+month)/2))
dat03 <- dat %>% filter(year == 2007) %>% mutate(count = count/(3+month)^2)
dat <- rbind(dat01, dat03)
vegalite() %>%
  cell_size(300, 400) %>%
  add_data(dat) %>%
  encode_y("date", "temporal") %>%
  encode_x("count", "quantitative", aggregate="sum") %>%
  encode_color("series", "nominal") %>%
  scale_color_nominal(range="category20b") %>%
  timeunit_y("yearmonth") %>%
  scale_y_time(nice="month") %>%
  axis_y(axisWidth=0, format="%Y", labelAngle=0) %>%
  mark_area(interpolate="basis", stack="center")
