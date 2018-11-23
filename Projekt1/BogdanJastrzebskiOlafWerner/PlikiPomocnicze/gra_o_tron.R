library(dplyr)
library(ggplot2)
library(readr)
battles <- read_csv("battles.csv")
Chapters_per_Book <- read_csv("Chapters_per_Book.csv")
character_deaths <- read_csv("character-deaths.csv")
character_predictions <- read_csv("character-predictions.csv")
c
character_deaths %>%
  filter(!is.na(`Book of Death`))->cleaned

Chapters_per_Book<-data.frame(book_name=c("A Game of Thrones","A Clash of Kings","A Storm of Swords","A Feast for Crows","A Dance with Dragons"),
                              number_of_chapter=c(73,70,82,46,73))

#write_csv(Chapters_per_Book,path = "~/Desktop/Gra_o_Tron/Chapters_per_Book.csv")

#liczymy ile przezyly martwe postacie

dead<-as.data.frame(character_deaths)

FirstBook<-apply(dead,1,FUN = function(x){min(which(x[c(9:13)]==1))})

LastBook<-apply(dead,1,FUN = function(x){max(which(x[c(9:13)]==1))})

dead<-cbind(dead,FirstBook,LastBook)

#uwaga dużo ludzi z nieznanym rozdziałem smierci
sum(is.na(dead$`Death Chapter`))

dead[,"Death Chapter"]<-as.numeric(apply(dead, 1, function(x){if(is.na(x["Death Chapter"])){as.data.frame(Chapters_per_Book)[x["LastBook"],2]}else{x["Death Chapter"]}}))

dead[,"Book Intro Chapter"]<-as.numeric(apply(dead, 1, function(x){if(is.na(x["Book Intro Chapter"])){0}else{x["Book Intro Chapter"]}}))

dead[,"Book of Death"]<-as.numeric(apply(dead, 1, function(x){if(is.na(x["Book of Death"])){x["LastBook"]}else{x["Book of Death"]}}))


survived<-apply(dead, 1, FUN = function(x){sum(as.numeric(x["Death Chapter"]),as.data.frame(Chapters_per_Book)[x["FirstBook"]:x["LastBook"],2],na.rm = TRUE,-1*as.numeric(x["Book Intro Chapter"]),-1*as.data.frame(Chapters_per_Book)[x["LastBook"],2])})

dead<-cbind(dead,survived)

dead<-subset(dead,survived>=0)

#liczymy ile przezyly zywe postacie

character_predictions %>%
  filter(isAlive==1)->alive

alive<-as.data.frame(alive)

FirstBook<-apply(alive,1,FUN = function(x){min(which(x[c(17:21)]==1))})

LastBook<-apply(alive,1,FUN = function(x){max(which(x[c(17:21)]==1))})

alive<-cbind(alive,FirstBook,LastBook)

alive<-subset(alive,is.infinite(FirstBook)==FALSE,is.infinite(LastBook)==FALSE)

survived<-apply(alive, 1, FUN = function(x){sum(as.data.frame(Chapters_per_Book)[x["FirstBook"]:x["LastBook"],2],na.rm = TRUE)})

alive<-cbind(alive,survived)

alive<-subset(alive,survived>=0)

alive%>%
  mutate(Nobility=isNoble,)->alive

#ilosc Y.O.L.O.S
sum(dead$FirstBook==dead$LastBook)

sum(alive$LastBook==alive$FirstBook)

#ilosc One shotow
sum(dead$survived==0)

ggplot(dead , aes(x=`Book of Death`))+geom_bar()+theme_minimal()+labs(y="Dead")

ggplot(dead , aes(x=`Death Chapter`,group=`Book of Death`))+geom_histogram(binwidth = 10)+theme_minimal()+
  facet_grid(.~`Book of Death`)+ coord_fixed(ratio = 1.2) + theme(aspect.ratio=1.7,
                                                                  panel.grid.major  = element_line(color = "black",linetype = "dashed")) +
  xlab("Books & Chapters") +
  ylab("Number of Deaths")

ggplot(dead , aes(x=`Death Chapter`,group=`Book of Death`))+geom_density()+theme_minimal()+
  facet_grid(.~`Book of Death`)+theme(aspect.ratio=1.7,panel.grid.major  = element_line(color = "black",linetype = "dashed"))

#ggplot(dead,aes(x=survived,fill=Nobility,color=Nobility,group=Nobility))+geom_histogram(binwidth = 10)



ggplot(dead,aes(x=survived,fill=factor(Nobility),color=factor(Nobility),group=factor(Nobility)))+geom_histogram(binwidth = 10)+theme_minimal()+labs(title = "Dead")+scale_x_continuous(expand = c(0,0),limits = c(0,NA))+theme(panel.grid.major  = element_line(color = "black",linetype = "dashed"))

ggplot(alive,aes(x=survived,fill=factor(Nobility),color=factor(Nobility),group=factor(Nobility)))+geom_histogram(binwidth = 10)+theme_minimal()+labs(title = "Alive")+scale_x_continuous(expand = c(0,0),limits = c(0,NA))+theme(panel.grid.major  = element_line(color = "black",linetype = "dashed"))

#ggplot(dead,aes(x=survived,fill=factor(Nobility),color=factor(Nobility),group=factor(Nobility)))+geom_freqpoly(binwidth = 10)

ggplot(dead,aes(x=survived,fill=factor(Nobility),color=factor(Nobility),group=factor(Nobility)))+geom_area(stat = "count")



ggplot(dead,aes(x=survived))+geom_area(stat = "count")

#ggplot(dead,aes(x=survived,fill=factor(Nobility),color=factor(Nobility),group=factor(Nobility)))+geom_line(stat = "count")


ggplot(dead %>% filter(Nobility == 0),aes(x=survived))+geom_density()+theme_minimal()+labs(title = "Dead")+scale_x_continuous(expand = c(0,0),limits = c(0,NA))+theme(panel.grid.major  = element_line(color = "black",linetype = "dashed"))

ggplot(dead,aes(x=survived,fill=factor(Nobility),color=factor(Nobility),group=factor(Nobility)))+
  geom_density(alpha=0.5)+theme_minimal()+
  theme(panel.grid.major  = element_line(color = "black",linetype = "dashed"),aspect.ratio=1/4)


