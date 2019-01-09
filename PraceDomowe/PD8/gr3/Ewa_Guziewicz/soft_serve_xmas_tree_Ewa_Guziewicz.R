library(plotly)

t = runif(1000, 0, 2*pi)
x = t*cos(6*t)
y = t*sin(6*t)
z = -t/(2*pi)+11

type = rep("darkgreen", 1000)
size = rep(as.numeric(10), 1000)
df = matrix(c(x, y, z, type, size), ncol = 5)
df = as.data.frame(df)
colnames(df) = c("x", "y", "z", "type", "size")

star = as.data.frame(matrix(c(0, 0, 11, "gold", as.numeric(20)), ncol = 5))
colnames(star) = colnames(df)
df = rbind(df, star)

stump = as.data.frame(matrix(c(0, 0, 10, "brown", as.numeric(30)), ncol = 5))
colnames(stump) = colnames(df)
df = rbind(df, stump)

df$size = as.numeric(df$size)*10


ax <- list(
  title = "",
  showline = FALSE,
  showticklabels = FALSE,
  showgrid = FALSE
)

p = plot_ly(df, x = ~x, y = ~y, z = ~z, type = "scatter3d", mode = "markers", 
        marker = list(symbol = c(rep("circle", 1001), "square"), color = ~type, color = c("darkgreen", "gold", "brown"), size = ~size,
                      line = list(color = c("darkgreen"), width = 0.00001))) %>%
    layout(scene = list(xaxis = ax, yaxis = ax, zaxis = ax))


df = df[1:1000, ]
t = runif(1000, 0, 2*pi)
x = t*cos(3*t)
y = t*sin(3*t)
z2 = -t/(2*pi)+11

p2 = add_trace(p, x = x, y = y, z = z2) %>%
  layout(scene = list(xaxis = ax, yaxis = ax, zaxis = ax))

t = runif(1000, 0, 2*pi)
x = t*cos(4*t)
y = t*sin(4*t)
z3 = -t/(2*pi)+11

p3 = add_trace(p2, x = x, y = y, z = z3) %>%
  layout(scene = list(xaxis = ax, yaxis = ax, zaxis = ax))


t = runif(1000, 0, 2*pi)
x = t*cos(5*t)
y = t*sin(5*t)
z4 = -t/(2*pi)+11

add_trace(p2, x = x, y = y, z = z4)%>%
  layout(scene = list(xaxis = ax, yaxis = ax, zaxis = ax), showlegend = FALSE)

