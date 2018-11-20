library(ggplot2)
library(ggthemes)
library(extrafont)
library(ggalluvial)
library(Cairo)
chars <- as.data.frame(read.csv("characters.csv", sep = "\t"))
locations <- as.data.frame(read.csv("locations.csv", sep = "\t"))
locs <- locations[,c("id", "name")]
names(locs) <- c("location", "name")
chars <- chars[,c("name", "species", "gender", "location")]
df1 <- merge(chars, locs, by = "location")
names(df1) <- c("location", "name","species", "gender", "loc_name")
df1 <- df1[,c("gender","species", "loc_name")]
df1$species <- as.character(df1$species)
df1$species[df1$species %in% names(table(df1$species)[table(df1$species) <20])] <- "Other"
df1$species <- factor(df1$species)
df1$loc_name <- as.character(df1$loc_name)
df1$loc_name[df1$loc_name %in% names(table(df1$loc_name)[table(df1$loc_name) <12])] <- "Other"
df1$loc_name <- factor(df1$loc_name)
df1 <- aggregate(seq(nrow(df1))~.,data=df1, FUN=length)
colnames(df1) <- c("sex","species","loc","count")
df1 <- df1[df1$count > 5,]

plot1 <- ggplot(df1, aes(y = count,axis1 = sex, axis2 = species, axis3 = loc)) +
  geom_alluvium(aes(fill = species),width = 1/12) +
  geom_stratum(width = 1/12, fill = "black", color = "grey", alpha = 0.9) +
  geom_text(stat = "stratum", label.strata = TRUE , size = 7, color = "white", family = "Calligraphr") +
  scale_x_discrete(limits = c("sex", "species","location"), expand = c(.05, 1)) +
  scale_fill_brewer(type = "qual", palette = 3) + 
  labs(title = "How diverse are characters in Rick and Morty?") +
  theme(plot.background = element_rect(fill = "black"),
        panel.background = element_rect(fill = "black"),
        plot.margin = unit(c(1, 3, 1, 3), "cm"),
        title = element_text(hjust = 0.5, color = "#39ff14", size = 26, family = "Rick_and_Morty"),
        axis.text.x = element_text(colour = "#39ff14", size = 16),
        axis.text.y = element_text(colour = "#39ff14", size = 18),
        legend.text = element_text(colour = "white", size = 14, family = "Calligraphr"),
        legend.background = element_rect(fill = "black")) + guides(fill = FALSE)
plot1
CairoPDF("proj_3.pdf", width = 16, height = 12)
plot1
dev.off()

