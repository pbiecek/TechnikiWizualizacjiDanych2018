library(data.table)
library(ggplot2)
library(stringi)
library(igraph)
library(svglite)

#Wczytanie danych
unique_per_scene <- as.data.table(read.csv("../data/the-office-lines - scripts.csv"))

#Obróbka danych
unique_per_scene <- unique_per_scene[deleted == FALSE, 
                                     ][ , - c("id", "line_text", "deleted")]

unique_per_scene <- unique_per_scene[, .(unique(speaker)), by = .(season, episode, scene)]

matrix <- matrix(rep(0, length(unique(unique_per_scene$V1))^2), 
                 nrow = length(unique(unique_per_scene$V1)))

rownames(matrix) <- unique(unique_per_scene$V1)
colnames(matrix) <- unique(unique_per_scene$V1)


on_scene <- c("Michael")
for(i in 2:length(unique_per_scene$V1)) {

  if(unique_per_scene[i]$scene != unique_per_scene[i-1]$scene || i == length(unique_per_scene$V1)){
    
    # Ostatnia scena - doliczamy ostatnia osobe i podliczamy
    if(i == length(unique_per_scene$V1)) {
      on_scene <- c(on_scene, as.character(unique_per_scene[i]$V1))
    }
    
    # Naliczamy dane w macierzy macierz
    for(p1 in on_scene) {
      for(p2 in on_scene) {
        if(p1 != p2) {
          matrix[p1, p2] <- matrix[p1, p2] + 1
        }
      }
    }
    
    # Przechodzimy scene dalej
    # Resetujemy pomocnicze dane
    on_scene <- c(as.character(unique_per_scene[i]$V1))
  } else {
    # Dalej ta sama scena
    
    on_scene <- c(on_scene, as.character(unique_per_scene[i]$V1))
  }
}
write.table(matrix, "scenes_together.csv", sep = ";")

result <- matrix[
  c("Pam", "Jim", "Andy", "Michael", "Phyllis", "Dwight", "Angela", "Oscar", "Kevin", "Stanley"),
  c("Pam", "Jim", "Andy", "Michael", "Phyllis", "Dwight", "Angela", "Oscar", "Kevin", "Stanley")
]
groups <- list(c("Angela", "Dwight"), c("Angela", "Kevin", "Oscar"), c("Pam", "Jim"))

#Przygotowanie grafu i zapis
#svg("graf.svg",width=14,height=14)

graph <- graph_from_adjacency_matrix(result, weighted = TRUE, mode = "undirected")
E(graph)$width <- E(graph)$weight / 30
l <- layout.circle(graph = graph)
par(bg = rgb(255/255,245/255,220/255, alpha = 0),
    family = "Cambria")
groups_color <- c(rgb(255/255, 204/255, 221/255, alpha = 0.5), 
                  rgb(25/255, 100/255, 25/255, alpha = 0.5),
                  rgb(255/255, 204/255, 221/255, alpha = 0.5))

plot(graph, layout = l, mark.groups = groups, mark.col = groups_color, 
     mark.border = "black",
     vertex.size = 34, vertex.color = "grey96", vertex.label.cex = 2, vertex.label.color = "lightblue4",
     edge.color = rgb(25/255, 25/255, 112/255, alpha = (log(E(graph)$width) / log(max(E(graph)$width)))))

#dev.off()

