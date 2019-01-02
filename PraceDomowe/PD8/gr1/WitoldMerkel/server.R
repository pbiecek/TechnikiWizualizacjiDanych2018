library(datasets)
library(ggplot2)
library(RColorBrewer)
library(grid)

shinyServer(function(input, output) {
  output$choinka = renderPlot({
    
    k = 101
    a <- cbind(rep(100,k),rep(100,k),seq(0,100,1), rep(0,k))
    a1 <- cbind(rep(100,34), rep(100,34), seq(67,100,1), rep(0,34))
    a2 <- cbind(rep(100,67), rep(100,67), seq(34,100,1), rep(0,67))
    a3 <- cbind(rep(100,101), rep(100,101), seq(0,100,1), rep(0,101))
    x <- rbind(a1,a2,a3)
    wektor <- c(rep("a",34),rep("b",67),rep("c",101))
    data <- cbind(x,wektor,rep("p",202))
    
    b <- cbind(rep(0,k),rep(100,k),seq(0,100,1), rep(0,k))
    b1 <- cbind(rep(0,34), rep(100,34), seq(0,33,1), rep(0,34))
    b2 <- cbind(rep(0,67), rep(100,67), seq(0,66,1), rep(0,67))
    b3 <- cbind(rep(0,101), rep(100,101), seq(0,100,1), rep(0,101))
    y <- rbind(b1,b2,b3)
    wektor2 <- c(rep("a",34),rep("b",67),rep("c",101))
    data2 <- cbind(y,wektor2,rep("q",202))
    
    l = 21
    c <- cbind(rep(seq(90,100,0.5)),rep(100,l),seq(90,100,0.5),rep(0,l))
    wektor3 <- c(rep("d",l))
    data3 <- cbind(c,wektor3,rep("p",l))
    
    d <- cbind(rep(seq(0,10,0.5)),rep(100,l),seq(0,10,0.5),rep(0,l))
    wektor4 <- c(rep("d",l))
    data4 <- cbind(d,wektor4,rep("q",l))
    
    prezent1 <- cbind(rep(seq(30,40,0.5)),rep(45,l),seq(30,40,0.5),rep(0,l))
    wektor5 <- c(rep("d",l))
    data5 <- cbind(prezent1,wektor5,rep("p",l))
    
    prezent2 <- cbind(rep(seq(25,35,0.5)),rep(60,l),seq(25,35,0.5),rep(0,l))
    wektor6 <- c(rep("d",l))
    data6 <- cbind(prezent2,wektor6,rep("q",l))
    
    prezent3 <- cbind(rep(seq(70,80,0.5)),rep(70,l),seq(70,80,0.5),rep(0,l))
    wektor7 <- c(rep("d",l))
    data7 <- cbind(prezent3,wektor7,rep("p",l))
    
    prezent4 <- cbind(rep(seq(55,85,0.5)),rep(30,3*l-2),seq(55,85,0.5),rep(0,3*l-2))
    wektor8 <- c(rep("d",3*l-2))
    data8 <- cbind(prezent4,wektor8,rep("q",3*l-2))
    
    prezent5 <- cbind(rep(seq(10,20,0.5)),rep(90,l),seq(10,20,0.5),rep(0,l))
    wektor9 <- c(rep("d",l))
    data9 <- cbind(prezent5,wektor9,rep("p",l))
    
    prezent6 <- cbind(rep(seq(48,58,0.5)),rep(15,l),seq(48,58,0.5),rep(0,l))
    wektor10 <- c(rep("d",l))
    data10 <- cbind(prezent6,wektor10,rep("p",l))
    
    kolory <- brewer.pal(9,"Greens")
    data <- rbind(data,data2,data3,data4)
    
    if(input$Prezenty == "Nie"){
      data <- rbind(data, data2, data3, data4)
    }
    
    if(input$Prezenty == "Tak"){
      data <- rbind(data, data2, data3, data4, data5, data6, data7, data8, data9, data10)
    }
    
    data <- data.frame(data)
    colnames(data) <- c("xstart","ystart","xend","yend","wiersz","kolumna")
    
    data$xstart <- as.numeric(as.character(data[,1]))
    data$ystart <- as.numeric(as.character(data[,2]))
    data$xend <- as.numeric(as.character(data[,3]))
    data$yend <- as.numeric(as.character(data[,4]))
    
    if(input$Stan == "Widziała lepsze dni"){
      p <- ggplot(data) + geom_segment(aes(x=xstart,y=ystart,xend=xend,yend=yend, color=wiersz)) +
        facet_grid(vars(wiersz),vars(kolumna), margin=FALSE) + theme_minimal() +
        guides(color=FALSE) + scale_color_manual(values = c("#996633", "#994d00", "#804000", "brown")) +
        scale_x_continuous(expand = c(0, 0)) +
        scale_y_continuous(expand = c(0, 0)) +
        theme(axis.title.x=element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank(),
              axis.title.y=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks.y=element_blank(),
              panel.spacing = unit(height + aux, "lines"))
      
      
    }
    
    
    if(input$Stan == "Nówka sztuka"){
      p <- ggplot(data) + geom_segment(aes(x=xstart,y=ystart,xend=xend,yend=yend, color=wiersz)) +
        facet_grid(vars(wiersz),vars(kolumna), margin=FALSE) + theme_minimal() +
        guides(color=FALSE) + scale_color_manual(values = c(kolory[6], kolory[7], kolory[8], "brown")) +
        scale_x_continuous(expand = c(0, 0)) +
        scale_y_continuous(expand = c(0, 0)) +
        theme(axis.title.x=element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank(),
              axis.title.y=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks.y=element_blank(),
              panel.spacing = unit(height + aux, "lines"))
      
      
    }
    p
  })
  
  
  output$Download1 <-  downloadHandler(
    
    filename = function(){
      paste("choinka_do_wyrzucenia.png")
    },
    
    content = function(file){
      ggsave(file, plot = p, device = "png", width = 14, height = 10)
    }
  )
})