


library(plotly)
library(RColorBrewer)
library(dplyr)

setwd("C:/Users/Dawid.Kowalczyk/Google Drive/Studia MiNI PW/III semestr/Techniki wizualizacji danych/PD8/")


### cone

points = 10000

theta = runif(points, 0, 2*pi)
r = runif(points, 0, 10)

x = r * sin(theta)
y = r * cos(theta)
z = 2*sqrt(x^2 + y^2)



data = data.frame(x, y, z) %>% arrange(z)
data$z = data$z[nrow(data):1]
names(data) = c("x", "y", "z")

data = data.frame(c(data$x, r * sin(theta)),
                  c(data$y, r * cos(theta)),
                  c(data$z, rep(0, points)))
names(data) = c("x", "y", "z")

 
### root

theta = runif(500, 0, 2*pi)
r = runif(500, 0, 1.5)

x = 1.5 * sin(theta)
y = 1.5 * cos(theta)
z = runif(500, -4, 0)

root = data.frame(x, y, z)
root = data.frame(c(root$x, r * sin(theta)),
                  c(root$y, r * cos(theta)),
                  c(root$z, rep(-4, 500)))
names(root) = c("x", "y", "z")

#### bombki

bombki = data[which(data$z > 1), ][sample(nrow(data[which(data$z > 1), ]), 30),]
bombki = mutate(bombki, "type" = round(seq(1, 8, length.out = 30), 0))
bombki$x = bombki$x + sign(bombki$x)* 0.3
bombki$y = bombki$y + sign(bombki$y)* 0.3

### snow

snow = data.frame(x = runif(300, -15, 15),
                  y = runif(300, -15, 15),
                  z = runif(300, -4, 25))


### ground

ground = data.frame(x = runif(3000, -15, 15),
                    y = runif(3000, -15, 15),
                    z = rep(-4, 3000))


### gifts

gifts1 = data.frame(x = runif(10000, -12, -8),
                    y = runif(10000, 8, 12),
                    z = runif(10000, -4, 1))

gifts2 = data.frame(x = runif(10000, 5, 8),
                    y = runif(10000, 5, 8),
                    z = runif(10000, -4, -2))

gifts3 = data.frame(x = runif(10000, 8, 11),
                    y = runif(10000, -11, -8),
                    z = runif(10000, -4, 0))

gifts = rbind(gifts1, gifts2, gifts3)

### plot

ax <- list(
  title = "",
  zeroline = FALSE,
  showline = FALSE,
  showticklabels = FALSE,
  showgrid = FALSE
)



plot = plot_ly() %>%
  add_markers(x =~data$x, y =~data$y, z =~data$z, color = ~data$z, colors = brewer.pal(9, "Greens")[c(3:9)], showlegend = F)  %>%
  add_markers(x =~root$x, y =~root$y, z=~root$z, marker = list(color = 'rgb(105, 52, 0)'), showlegend = F) %>%
  add_markers(plot, x =~bombki$x, y =~bombki$y, z=~bombki$z, marker = list(size = 10,  color = ~bombki$type,  colors = brewer.pal(8, "Oranges")), showlegend = F) %>% 
  add_markers(x =~snow$x, y =~snow$y, z=~snow$z, marker = list(size = 3, color = 'rgb(255, 255, 255)'), showlegend = F) %>%
  add_markers(x =~ground$x, y =~ground$y, z=~ground$z, marker = list(size = 3, color = 'rgb(255, 255, 255)'), showlegend = F) %>%
  add_markers(x =~gifts$x, y =~gifts$y, z=~gifts$z, marker = list(size = 8, color = 'rgba(235, 20, 0, 1)'), showlegend = F) %>%
  layout(showlegend = FALSE,
         scene = list(camera= list(eye= list(x= 1, y= 1, z= 0)),
                      xaxis = ax,
                      yaxis = ax,
                      zaxis = ax),
         title = "Świąteczna choinka 3D",
         paper_bgcolor = 'rgba(9, 72, 190, 0.25)')
         

#hide_colorbar(plot)


htmlwidgets::saveWidget(as_widget(hide_colorbar(plot)), "choinka.html")





