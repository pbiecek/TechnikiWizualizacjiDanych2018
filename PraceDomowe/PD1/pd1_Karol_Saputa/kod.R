read.csv("charts_drinking_words_counts_summary.csv", stringsAsFactors = FALSE) -> data_summary
library(ggplot2)
library(dplyr)
data_summary %>% group_by(chart) %>% summarise(mean(percentageOfSongsWithDrinking)) -> sum_by_chart
colnames(sum_by_chart) <- c("Chart", "Percentage of Songs Referencing Alcohol")
#sum_by_chart$`Percentage of Songs Referencing Alcohol` <- round(sum_by_chart$`Percentage of Songs Referencing Alcohol`, digits = 2)

#sum_by_chart %>% arrange(sum_by_chart$`Percentage of Songs Referencing Alcohol`) -> sum_by_chart
ggplot(sum_by_chart,
       aes(x = reorder(Chart, -`Percentage of Songs Referencing Alcohol`) ,
           y=`Percentage of Songs Referencing Alcohol`)) +
        geom_bar(stat = "identity", alpha = 0.2+1.7*sort(sum_by_chart$`Percentage of Songs Referencing Alcohol`, decreasing = TRUE), fill=c("orangered",rep("blue4", times = 6))) + coord_flip() +
  geom_text(aes(y = (`Percentage of Songs Referencing Alcohol`+0.04), label = scales::percent(round(`Percentage of Songs Referencing Alcohol`, digits = 2)), vjust = 0.45) ) +
        theme(
         panel.grid.major = element_blank(), 
         panel.grid.minor = element_blank(),
         panel.background = element_rect(fill = "transparent",colour = NA),
         plot.background = element_rect(fill = "transparent",colour = NA),
         axis.title.y = element_blank(),
         axis.text.x  = element_blank(),
         axis.ticks.x = element_blank()
       ) 