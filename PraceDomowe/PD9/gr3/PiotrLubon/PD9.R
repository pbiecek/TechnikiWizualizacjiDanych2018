data <- load('dragons.rda')
dragons$year_of_death <- dragons$year_of_birth + dragons$life_length
red <- numeric(101)
green <- numeric(101)
black <- numeric(101)
blue <- numeric(101)
for(i in 1700:1800)
{
  red[i-1699] <- nrow(dragons[dragons$colour=='red' & dragons$year_of_discovery <= i,])
  green[i-1699] <- nrow(dragons[dragons$colour=='green' & dragons$year_of_discovery <= i,])
  black[i-1699] <- nrow(dragons[dragons$colour=='black' & dragons$year_of_discovery <= i,])
  blue[i-1699] <- nrow(dragons[dragons$colour=='blue' & dragons$year_of_discovery <= i,])
}
df <- rbind(data.frame(colour='red', year=1700:1800, count=red), data.frame(colour='green', year=1700:1800, count=green),
            data.frame(colour='black', year=1700:1800, count=black), data.frame(colour='blue', year=1700:1800, count=blue))
library(jsonlite)
write.csv(df, 'dragons.csv')
df1 <- data.frame(year=1700:1800, red=red, blue=blue, green=green, black=black)
write.csv(df1, 'dragons1.csv', row.names=FALSE)
