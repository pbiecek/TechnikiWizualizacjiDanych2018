library(tidyverse)
df <- as_tibble(dragons)
df <- df %>% 
  mutate(BMI = weight*1000 / (height * height * 0.9144**2))

ggplot(df, aes(y = life_length, x = BMI, color=colour)) +
  geom_point() +
  theme_minimal()

df %>%  write_csv('dragons.csv')
