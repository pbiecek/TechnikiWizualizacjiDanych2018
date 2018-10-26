library(ggplot2)
library(ggthemes)
library(reshape2)

df <- data.frame(read.csv("dane.csv", header = TRUE, sep = ";", dec = ","))
df.m <- melt(df, id.vars = "stan")

names(df)<-c("stan", "r2002", "r2011")
names(df.m)<-c("stan", "rok", "wartosc")

df.m <- df.m[order(df.m$wartosc, df.m$stan),]
df.m$stan <- reorder(df.m$stan, df.m$wartosc)
wykresik <- ggplot(data = df.m, aes(x = stan, y = wartosc, fill = rok)) + 
                     geom_bar(stat = "identity", position = position_dodge()) + 
                     coord_flip() + 
                     ggtitle(" Ludność w wieku 15 lat i więcej według stanu cywilnego prawnego
w latach 2002 i 2011 (w %) ") +
                     ylab("Wartość") + xlab("Stan cywilny") +
                     scale_fill_brewer(palette = "Paired", name = "Rok",labels = c("2002", "2011")) +
                     theme_minimal() + scale_y_continuous(name = "Wartość", limits = c(0,70), breaks = seq(0,70,10))
                    
                   
wykresik

