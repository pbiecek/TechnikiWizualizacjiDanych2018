library(data.table)
library(ggplot2)

#df <- as.data.table(read.csv2(file='Wojtek.csv', header=TRUE, sep=';', encoding = 'UTF-8',dec=","))

df$day = factor(df$day, levels=c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

day_steps <- df[,.(steps = mean(steps, na.rm=TRUE)), by=day]


day_steps_plt <- ggplot(data=day_steps, aes(x=day, y=steps, fill = day)) + geom_col() + ggtitle("") + 
  theme(legend.position = "none", text = element_text(size=20)) +
  scale_x_discrete(name = "Dzieñ", labels = c("Poniedzia³ek", "Wtorek", "Œroda", "Czwartek", "Pi¹tek", "Sobota", "Niedziela")) + scale_y_continuous(name = "Liczba kroków")

#sleep_steps_day <- ggplot(data=df, aes(x=sleep, y=steps, color=day)) + geom_point(size=10)

series_steps <- ggplot() + 
  geom_col(data=df, aes(x=X.U.FEFF.date,y=steps,group=1, fill = X.U.FEFF.date)) +
  theme(axis.text.x = element_text(size=10, vjust = -0.05), legend.position = "none", text = element_text(size=20)) + 
  scale_x_discrete(name="Data") +
  scale_y_discrete(name="Liczba kroków")

#series_sleep <- ggplot() + 
  #geom_line(data=df, aes(x=X.U.FEFF.date,y=sleep,group=1)) + geom_hline(yintercept = mean(df$sleep, na.rm = TRUE))

day_steps_violin <- ggplot(data=df, aes(x=day, y=steps)) + geom_violin() + theme_minimal() +
  theme(text = element_text(size=20)) +
  scale_x_discrete(name="Dni", labels = c("Poniedzia³ek", "Wtorek", "Œroda", "Czwartek", "Pi¹tek", "Sobota", "Niedziela")) +
  scale_y_continuous(name="Kroki")

day_sleep_violin <- ggplot(data=df, aes(x=day, y=sleep)) + geom_violin() + theme_minimal() +
  theme(text = element_text(size=20)) +
  scale_x_discrete(name="Dni", labels = c("Poniedzia³ek", "Wtorek", "Œroda", "Czwartek", "Pi¹tek", "Sobota", "Niedziela")) +
  scale_y_continuous(name="Sen [h]")

series_step_length <- ggplot(data = df[,.(X.U.FEFF.date, ratio=distance/steps * 1000)], aes(x=X.U.FEFF.date,y=ratio, group=1, fill = X.U.FEFF.date) ) + geom_col() + theme_minimal() +
  scale_y_continuous(name="D³ugoœæ kroku [m]") +
  scale_x_discrete(name = "Data") +
  theme(axis.text.x = element_text(size=10, vjust = -0.05), legend.position = "none", text = element_text(size=20))

day_step_length <- ggplot(data = df[,.(ratio=mean(distance/steps * 1000, na.rm=TRUE)), by=day], aes(x=day, y=ratio, fill = day)) + geom_col() + 
  theme(legend.position = "none", text = element_text(size=20)) +
  scale_x_discrete(name = "Dzieñ",labels = c("Poniedzia³ek", "Wtorek", "Œroda", "Czwartek", "Pi¹tek", "Sobota", "Niedziela")) +
  scale_y_continuous(name="D³ugoœæ kroku [m]")


sleep_steps_density <- ggplot(data = df, aes(x=sleep, y=steps)) + geom_density_2d() + facet_wrap(~day) +
  theme_minimal() +
  theme(text = element_text(size=20)) +
  scale_x_continuous(name = "Sen [h]") +
  scale_y_continuous(name = "Kroki")
