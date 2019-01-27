library(dplyr)
library(r2d3)

group_by(ChickWeight, Diet) %>% 
  summarise(mw = mean(weight)) %>% 
  mutate(Diet = Diet) %>% 
  r2d3(script = "./Materialy/d3/basics/sp.js")
