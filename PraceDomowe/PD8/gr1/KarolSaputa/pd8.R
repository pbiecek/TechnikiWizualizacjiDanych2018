jpeg::readJPEG("Tree.jpg") -> one_picture
png(filename="name.png", width = 1920, height = 1080)
plot(1:100,ty="n", ann = FALSE, xaxt='n', yaxt='n', bty="n")

height <- 80
width <- 60
midpoint <- 50
cos_x <- 2*height/width

on_tree <- function(x, y){

  
  dx <- abs(midpoint - x)
  height_dx <- (width/2 - dx) * cos_x
  if( y < height_dx){
    return(TRUE)
  }
  else
    return(FALSE)
}
n <- 500
size <- 5
points_ <- matrix(nrow = n, ncol = 2)
points_[ , 1] <- runif(n, 0, 100)
points_[ , 2] <- runif(n, 0, 100)

apply(points_, 1, function(x){
  if(on_tree(x[1], x[2])){
    rasterImage(one_picture, x[1]-size, x[2] - size, x[1], x[2])
  }
})
text(width/2+10, y = 90, labels = "Choinka zespolona")

dev.off()