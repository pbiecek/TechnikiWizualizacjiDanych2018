library(igraph)
library(ggplot2)
library(readr)
library(dplyr)
library(data.table)
library(patchwork)
library(png)
library(jpeg)

#projekt w katalogu głównym

#wczytanie
nodes <- read.csv('dataset/nodes.csv')
edges <- read.csv('dataset/edges.csv')
hero_network  <- data.table(read.csv('dataset/hero-network.csv'))
hero1 <- hero_network[, .(.N), by = .(hero1)]
hero2 <- hero_network[, .(.N), by = .(hero2)]
merged <- merge(hero1,hero2,all=TRUE,by.x = "hero1",by.y = "hero2")
#NA-s should be replaced
merged$n <- (merged$N.x + merged$N.y)
merged <- merged[,c(1,4)]
top20 <- merged[order(-n)][1:20]
#powyżej zmienia się 10 na inną liczbę, żeby było tylu superbohaterów
heros <- hero_network[, .(.N), by = .(hero1,hero2)][order(-N)]



if_both_of_heros_in_top_20 <- function (x) if(any(rep(x[1],20)==top20$hero1) & any(rep(x[2],20)==top20$hero1)) 1 else 0
#which_rows_of_hero_network <- apply(hero_network,1,if_both_of_heros_in_top_20)
#saveRDS(which_rows_of_hero_network,'heroesGraph/which_rows_of_hero_network.RDS')
which_rows_of_hero_network = readRDS('heroesGraph/which_rows_of_hero_network.RDS')
hn_edges <- hero_network[which_rows_of_hero_network==1, .(.N), by = .(hero1,hero2)][order(-N)]
#brzydkie :(
hn_edges2 <- hn_edges[1,]
for (i in 2:nrow(hn_edges)) {
  j <- nrow(hn_edges2)
  heros_the_same <- (hn_edges2[1:j]$hero1 == rep(as.character(hn_edges[i]$hero2),j)) + 
    (hn_edges2[1:j]$hero2 == rep(as.character(hn_edges[i]$hero1),j))
  if (any((heros_the_same) == 2)) {
    index <- (1:j)[heros_the_same==2]
    hn_edges2[index]$N <- hn_edges2[index]$N + hn_edges[i]$N
  }
  else {hn_edges2 <- rbind(hn_edges2,hn_edges[i])}
}
hn_edges <- hn_edges2[-42,]
hn_edges <- hn_edges[nrow(hn_edges):1,]
#636363
edge_colors <- rep('blue',nrow(hn_edges))
#edge_colors[as.numeric(hn_edges$N)>350L] <- 'blue'

hn_edges <- hn_edges[as.numeric(N)>110L]
hn_edges[] <- lapply(hn_edges, as.character)
network1 <- graph_from_data_frame(hn_edges, directed = F, top20$hero1)

V(network1)$raster[[1]] <- readPNG('icons/captain_america.png')
V(network1)$raster[[2]] <- readPNG('icons/spider_man.png')
V(network1)$raster[[3]] <- readPNG('icons/iron_man.png')
V(network1)$raster[[4]] <- readPNG('icons/thor.png')
V(network1)$raster[[5]] <- readPNG('icons/thing.png')
V(network1)$raster[[6]] <- readPNG('icons/wolverine.png')
V(network1)$raster[[7]] <- readPNG('icons/torch.png')
V(network1)$raster[[8]] <- readPNG('icons/scarlet.png')
V(network1)$raster[[9]] <- readPNG('icons/fantastic.png')
V(network1)$raster[[10]] <- readPNG('icons/vision.png')
V(network1)$raster[[11]] <- readPNG('icons/invisible.png')
V(network1)$raster[[12]] <- readPNG('icons/beast.png')
V(network1)$raster[[13]] <- readPNG('icons/cyclops.png')
V(network1)$raster[[14]] <- readPNG('icons/storm.png')
V(network1)$raster[[15]] <- readPNG('icons/hawkeye.png')
V(network1)$raster[[16]] <- readPNG('icons/wasp.png')
V(network1)$raster[[17]] <- readPNG('icons/colossus.png')
V(network1)$raster[[18]] <- readPNG('icons/professor.png')
V(network1)$raster[[19]] <- readPNG('icons/hulk.png')
V(network1)$raster[[20]] <- readPNG('icons/ant_man.png')

labels <- c("CAPTAIN AMERICA","SPIDER-MAN","IRON MAN","THOR",
            "THING","WOLVERINE",
            "HUMAN TORCH","SCARLET WITCH","MISTER FANTASTIC",
            "VISION","INVISIBLE WOMAN",
            "BEAST","CYCLOPS","STORM","HAWKEYE","WASP",
            "COLOSSUS","PROFESSOR X",
            "HULK","ANT-MAN")

where_they_are = matrix(as.numeric(as.matrix(read.csv('heroesGraph/layout.csv'))[,3:4]),ncol=2)

color.gradient <- function(x, colors=c("grey","black","black"), colsteps=100) {
  return( colorRampPalette(colors) (colsteps) [ findInterval(x, seq(min(x),max(x), length.out=colsteps)) ] )
}



cairo_ps("heroesGraph/graf_polaczen.eps", height=50, width=100)
plot.igraph(network1,vertex.shape="raster",vertex.label = rep('',20),
            # vertex.size = 10, vertex.label.cex = 1,
            asp = 0.5, vertex.size = 2, vertex.size2 = 4,
            vertex.label.cex = 1.5, vertex.label.dist = 3,
            edge.width = as.numeric(hn_edges$N)/10,
            edge.color = color.gradient(as.numeric(hn_edges$N)),
            layout = where_they_are)
            #main = "a Network Relationship between Heroes")
dev.off()
#zdjecia superbohaterow są takie małe, ponieważ przy większych krawędzie są rysowane 
#jakby z losowej części obrazka jednego do losowej części obrazka drugiego


#pionowo zdjęcia
hn_edges <- hn_edges[as.numeric(N)>1100L]
hn_edges[] <- lapply(hn_edges, as.character)
network1 <- graph_from_data_frame(hn_edges, directed = F, top20$hero1)

V(network1)$raster[[1]] <- readPNG('icons/captain_america.png')
V(network1)$raster[[2]] <- readPNG('icons/spider_man.png')
V(network1)$raster[[3]] <- readPNG('icons/iron_man.png')
V(network1)$raster[[4]] <- readPNG('icons/thor.png')
V(network1)$raster[[5]] <- readPNG('icons/thing.png')
V(network1)$raster[[6]] <- readPNG('icons/wolverine.png')
V(network1)$raster[[7]] <- readPNG('icons/torch.png')
V(network1)$raster[[8]] <- readPNG('icons/scarlet.png')
V(network1)$raster[[9]] <- readPNG('icons/fantastic.png')
V(network1)$raster[[10]] <- readPNG('icons/vision.png')
V(network1)$raster[[11]] <- readPNG('icons/invisible.png')
V(network1)$raster[[12]] <- readPNG('icons/beast.png')
V(network1)$raster[[13]] <- readPNG('icons/cyclops.png')
V(network1)$raster[[14]] <- readPNG('icons/storm.png')
V(network1)$raster[[15]] <- readPNG('icons/hawkeye.png')
V(network1)$raster[[16]] <- readPNG('icons/wasp.png')
V(network1)$raster[[17]] <- readPNG('icons/colossus.png')
V(network1)$raster[[18]] <- readPNG('icons/professor.png')
V(network1)$raster[[19]] <- readPNG('icons/hulk.png')
V(network1)$raster[[20]] <- readPNG('icons/ant_man.png')

cairo_ps("heroesGraph/os_wykresu.eps", height=50, width=50)
plot.igraph(network1,vertex.shape="raster",vertex.label = rep('',20),
            asp = 1, vertex.size = 10, vertex.size2 =10,
            layout = matrix(c(rep(1,20),seq(20,1)),ncol=2))
dev.off()

