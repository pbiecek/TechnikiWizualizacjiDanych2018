load('dragons.rda')
library(data.table)
dragons <- as.data.table(dragons)[,.(oldest=min(year_of_birth),youngest=max(year_of_birth),height=mean(height),weight=mean(weight)
                          ,scars=mean(scars),year_of_discovery=mean(year_of_discovery),number_of_lost_teeth=mean(number_of_lost_teeth)
                          ,life_length=mean(life_length),number=.N),.(colour)]
write.csv(dragons,'dragons.csv')
