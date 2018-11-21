library(data.table)
library(ggplot2)
library(Cairo)

dt <- data.table(read.csv2("edges.csv"))
names(dt) <- c("hero1","hero2")
ans1 <- dt[, .(.N), by = .(hero1)]
ans2 <- dt[, .(.N), by = .(hero2)]

merged <- merge(ans1,ans2,all=TRUE,by.x = "hero1",by.y = "hero2")
merged$n <- (merged$N.x + merged$N.y)
merged <- merged[,c(1,4)]
merged <- merged[order(-n)]
ans <- merged[1:20]
ans$hero1 <- c("CAPTAIN AMERICA","SPIDER-MAN","IRON MAN","THOR","THING","WOLVERINE",
               "HUMAN TORCH","SCARLET WITCH","MISTER FANTASTIC","VISION","INVISIBLE WOMAN",
               "BEAST","CYCLOPS","STORM","HAWK","WASP","COLOSSUS","PROFESSOR X",
               "HULK","ANT-MAN")

powers <- data.table(read.csv2("powers.csv"))
colnames(powers)[1] <- "heros"
powers$heros <- toupper(powers$heros)

tmp <- merge(ans,powers,by.x = "hero1",by.y = "heros", all.x = TRUE)
write.csv2(tmp,file="dowykresu.csv")

temp <- tmp[Accelerated.Healing==TRUE]
nrow(temp)

# po filtrowaniu i posortowaniu mocy

dowykresu <- read.csv("dowykresupoprawione.csv")
moce <- read.csv("nazwy.csv")

moce2 <- dcast(melt(moce, id.vars = "X"), variable ~ X)
moce2[3:21,]<-moce2[2,]
moce2 <- moce2[2:50]
names(moce2) <- moce2[1,]
moce2 <- moce2[2:21,]

write.csv(moce2,file="moceiklasy.csv")

 # po znalezieniu kto ma ile jakich mocy

final <- data.table(read.csv("final.csv"))

final <- final[,c(2,3,53:56)]
final <- final[order(-n)]
final$add <- 1:20

names(final) <- c("hero","popularity","real","scifi","fantasy","all","number")
final$real <- final$real/final$all*100
final$scifi <- final$scifi/final$all*100
final$fantasy <- final$fantasy/final$all*100

final$hero <- c("Capitan America","Spider-Man","Iron Man","Thor","Thing","Wolverine",
               "Human Torch","Scarlet Witch","Mister Fantastic","Vision","Invisible Woman",
               "Beast","Cyclops","Storm","Hawk","Wasp","Colossus","Professor X",
               "Hulk","Ant-Man")

tmp <- final[,-6]
tmp <- melt(tmp[,-2], id=c("hero","number"))
tmp[order(tmp$number)]
tmp <- tmp[value!=0]

w1 <- ggplot(tmp[order(tmp$number)], aes(x=hero, y=value, fill=variable)) +
  geom_bar(stat="identity") +
  scale_x_discrete(limits = final$hero[20:1]) +
  geom_text(position = "stack",aes(label = paste0(round(value),"%")),hjust=1.2)+
  coord_flip()+
  scale_fill_manual(values = c("#636363","#ca0020","#0571b0"),breaks = c ('real','scifi','fantasy'),labels = c('Real','Science Fiction     ','Fantasy     '),guide = guide_legend(reverse=TRUE)) +
  labs(y = "",x ="", title="What distribution of power do the most popular heros have?",fill='Types of Powers')+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        title = element_text(colour = "black", size = 18),
        axis.text.y = element_text(margin = margin(r=0),colour = "black", size = 14),
        axis.ticks.y = element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        legend.title=element_blank(),legend.position="top",
        legend.text = element_text( size = 16, face = "bold"))



cairo_ps("wykres_mocy.eps" )
w1
dev.off()

cairo_pdf("wykres_mocy.pdf")
w1
dev.off()


