library(stringi)
library(dplyr)
library(extrafont)
library(gridExtra)
library(svglite)
library(patchwork)
library(cowplot)
devtools::install_github("thomasp85/patchwork")

#font_import()
fonts()
loadfonts()

library(ggplot2)

format_movie_title <- function(file_path) {
  file_name <- tools::file_path_sans_ext(file_path)
  stri_join_list(lapply(stri_split(file_name, fixed='_'), function(part) { tools::toTitleCase(part) }), " ")
}

plot_emoji <- function(file_path) {
  emojis <- read.csv('emoji_unicode.csv', header = FALSE, stringsAsFactors = FALSE)
  
  subtitle_emojis <- read.csv(file_path, stringsAsFactors = FALSE, sep = ";")
  
  subtitle_emojis <- subtitle_emojis[subtitle_emojis$Top5. > .4,]
  
  f <- factor(emojis$V1[subtitle_emojis$Emoji_1+1])
  tab <- tabulate(f)
  
  dat <- data.frame(x = stri_unescape_unicode(levels(f)), y = tab)
  dat <- dat %>% arrange(y) %>% filter(y > 2) %>% top_n(10)
  
  
  ggplot(dat, aes(x=reorder(x, -y), y=y)) + 
    geom_bar(stat = "identity") +
    ggtitle(paste("Top emoji in", format_movie_title(file_path))) +
    ylab("Count") + xlab("Emoji") +
    theme(axis.text.x = element_text(family = "Segoe UI Emoji", size = 30),
          legend.text = element_text(family = "Segoe UI Emoji"),
          title = element_text(size = 20))
}

emoji_plot <- grid.arrange(
  plot_emoji('reservoir_dogs_script.csv'),
  plot_emoji('jackie_brown_script.csv'),
  plot_emoji('pulp_fiction_script.csv'),
  plot_emoji('django_script.csv')
)

save_plot(file="emoji_plot.png", plot=emoji_plot, base_aspect_ratio = 7, nrow = 3)

plot_timeline <- function(file_path, title, plot_legend = FALSE) {
  
  movie_timeline <- read.csv(file_path, stringsAsFactors = FALSE, sep = ";")
  
  ggplot(movie_timeline, aes())
  
  df <- expand.grid(x = 0:5, y = 0:5)
  df$z <- runif(nrow(df))
  
  ggplot(df, aes(x, y, fill = NA)) + geom_raster()
  
  i <- 0L
  df <- data_frame()
  columns <- colnames(movie_timeline)
  columns <- columns[columns != "time"]
  for (col in columns) {
    df <- rbind(df, data_frame(x = which(!is.na(movie_timeline[,col])), y = i, z = i))
    i <- i + 1L
  }
  
  palette <- c("#caccce", "#860707", "#36312e", "#fcfcfc", "#e85407", "#f3b400", "#3f9dc7", "#000000", "#1e61ab")
  
  interleave <- function(x,y){
    lx <- length(x)
    ly <- length(y)
    n <- max(lx,ly)
    as.vector(rbind(rep(x, length.out=n), rep(y, length.out=n)))
  }
  
  p <- ggplot(df, aes(x, y, fill = factor(z))) + 
    geom_raster() + 
    coord_equal() + 
    scale_y_discrete(limits=0:(length(columns)-1), labels=columns) + 
    scale_x_discrete(limits=0:100, breaks=seq(1, 100, by = 1), labels=interleave(seq(1,100,by=2), "")) + 
    scale_fill_manual(values=palette, labels=columns, name="Category", position="bottom") + 
    ggtitle(title) +
    xlab("Time") + ylab("Category") +
    theme(legend.position="bottom", plot.title = element_text(size=22))
  if (!plot_legend) {
    p <- p +  
      theme(legend.position="none")
  }
  p
}


timeline_plot <- plot_timeline('reservoir_dogs.csv', 'Reservoir Dogs') /
plot_timeline('pulp_fiction.csv', 'Pulp Fiction') /
plot_timeline('jackie_brown.csv', 'Jackie Brown', TRUE)

save_plot(file="timeline_plot.png", plot=timeline_plot, base_aspect_ratio = 7, nrow = 3)
