#source("https://install-github.me/thomasp85/gganimate")
devtools::install_github("thomasp85/gganimate")

library(ggplot2)
library(gganimate)

x <- ggplot(mtcars, aes(factor(cyl), mpg)) + 
  geom_boxplot() + 
  # Here comes the gganimate code
  transition_states(
    gear,
    transition_length = 2,
    state_length = 1
  ) +
  enter_fade() + 
  exit_shrink() +
  ease_aes('sine-in-out') 


animate(x, renderer = magick_renderer())
