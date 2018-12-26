library(rbokeh)
christmas_tree <- data.frame(x=rep(seq(-2.5,2.5,0.1),times=c(seq(1,50),seq(50,1))),y=-abs(rep(seq(2.5,-2.5,-0.1))))




figure(tools = "save") %>% ly_hexbin(x=christmas_tree[,"x"],y=christmas_tree[,"y"],palette = "Greens3")

christmas_tree


christmas_tree <- data.frame(x=rep(seq(-5,5,0.1),times=c(seq(1,50),seq(50,1))),y=rep())