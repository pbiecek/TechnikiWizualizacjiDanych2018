head(dragons) 
library(ggplot2)
ggplot(dragons, aes(y = life_length, x = number_of_lost_teeth, color=colour)) +
  geom_point() +
  theme_minimal()
write.csv(dragons, file = "dragons.csv")
