library(dplyr)
library(ggplot2)

dat <- data.frame(variable = c("Liczba studentów", "Liczba pracowników"),
                  value = c(1000, 153))

p <- ggplot(dat, aes(x = variable, y = value)) +
  geom_col() + 
  theme_bw() +
  scale_x_discrete("") +
  scale_y_continuous("")

library(magick)

mini <- image_read("https://upload.wikimedia.org/wikipedia/commons/thumb/a/ae/Gmach_Wydzia%C5%82u_MiNI.jpg/467px-Gmach_Wydzia%C5%82u_MiNI.jpg")

mini_mini <- mini %>%
  image_scale("140") %>% 
  image_background("grey", flatten = TRUE) %>%
  image_border("white", "30x60") %>%
  image_annotate("MiNI", color = "black", size = 50, 
                 location = "+0+10", gravity = "north")

p + annotation_raster(as.raster(mini_mini), 1, 2, 0, 1000)


