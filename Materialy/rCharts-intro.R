library(SmarterPoland)
library(dplyr)

# remove dots from column names
countries2 <- countries
names(countries2) = gsub('\\.', '', names(countries2))
head(countries2)

# ggplot

library(ggplot2)
ggplot(data = countries2, aes(x = birthrate, y = deathrate)) + 
  geom_point() +
  facet_wrap(~ continent)

# lattice

library(lattice)
xyplot(deathrate ~ birthrate | continent, data = countries2)

# http://ramnathv.github.io/rCharts/
source("https://install-github.me/ramnathv/rCharts")
library(rCharts)

# http://www.rpubs.com/dnchari/rcharts

# Polycharts ----------------

rPlot(deathrate ~ birthrate | continent, data = countries2, type = "point")

r <- rPlot(deathrate ~ birthrate | continent, data = countries2, type = "point")
r

r <- rPlot(deathrate ~ birthrate, data = countries2, type = "point", color = "continent")
r

r$facet(var = "continent", type = 'wrap', rows = 2)
r

r$save('r.html')

r_error <- rPlot(death.rate ~ birth.rate | continent, data = countries, type = "point")

str(r_error)
r_error[["params"]][["layers"]][[1]][["x"]] <- "birthrate"
r_error[["params"]][["layers"]][[1]][["y"]] <- "deathrate"
colnames(r_error[["params"]][["layers"]][[1]][["data"]])[2L:3] <- c("birthrate", "deathrate")
r_error


# nvd3 charts -----------------------------
nPlot(death.rate ~ birth.rate, group = "continent", data = countries, type = "scatterChart")

# nie dziala poprawnie
nPlot(death.rate ~ birth.rate | continent, data = countries, type = "scatterChart")

n <- nPlot(death.rate ~ birth.rate, group = "continent", data = countries, type = "scatterChart")

n$xAxis(axisLabel = "Birth rate")
n$yAxis(axisLabel = "Death  rate")
n$set(width = 750, height = 590)
n


continents <- group_by(countries, continent) %>% 
  na.omit %>% 
  summarise(deathrate = mean(death.rate),
            birthrate = mean(birth.rate, na.rm = TRUE),
            population = mean(population),
            ncountries = length(country))

nPlot(ncountries ~ continent, data = continents, type = "multiBarChart")

hair_eye = as.data.frame(HairEyeColor)
hair_eye = as.data.frame(HairEyeColor)
n2 <- nPlot(Freq ~ Hair, group = "Eye", data = filter(hair_eye, Sex == "Female"), 
            type = "multiBarChart")
n2$chart(color = c('brown', 'blue', '#594c26', 'green'))
n2

# plotly -------------------------------

library(plotly)
p <- ggplot(data = countries2, aes(x = birthrate, y = deathrate)) + 
  geom_point() +
  facet_wrap(~ continent)

ggplotly(p)

# rpivotTable -----------------------------

library(rpivotTable)

rpivotTable(countries, rows = "continent",  rendererName = "Bar Chart")
