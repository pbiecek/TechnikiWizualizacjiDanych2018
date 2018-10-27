library(ggplot2)
wedding_costs <- c(41521, 29613, 27091, 31191, 28465, 27245, 30586, 28320, 28240, 33406, 31335, 36902)
region <- c("London", "East of England", "Wales", "West Mids", "East Mids", "Yorkshire", "North West", "North East",
            "South West", "South East", "Scotland", "Northern Ireland")

wedding_data <- data.frame(region = region, 
                           wedding_costs = wedding_costs)
wedding_data$limits <- ifelse(wedding_costs > 40000, "Above 40 000", 
                              ifelse(wedding_costs > 30000, "Above 30 000", "Above 20 000"))

ggplot(data = wedding_data, 
  aes(x = region, y = wedding_costs), fill = wedding_costs) +
  geom_bar(aes(fill=factor(limits)), position = 'dodge', stat="identity", width=.5) +
  scale_fill_manual(values=c("#E066FF", "#9932CC", "#68229B")) +
  theme(axis.line = element_blank(), 
        plot.title = element_text(hjust=0.5)) + 
  labs(title="UK Wedding Costs By Region", 
       subtitle="The average total cost of wedding suppliers in 2018 is 32.273 pounds", 
       x="Region of UK", y="Wedding Costs in Pounds") + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6), legend.title = element_blank()) +
  geom_text(aes(label=wedding_costs), position=position_dodge(width=0.9), vjust=-0.25) 
