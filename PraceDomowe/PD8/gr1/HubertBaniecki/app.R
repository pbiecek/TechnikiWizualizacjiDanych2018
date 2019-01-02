library(datasets)
library(ggplot2)
library(RColorBrewer)
library(shiny)

## dane
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

l = 31
c <- cbind(rep(seq(94,100,0.2)),rep(100,l),seq(85,100,0.5),rep(0,l))
wektor3 <- c(rep("d",l))
data3 <- cbind(c,wektor3,rep("p",l))


d <- cbind(rep(seq(0,6,0.2)),rep(100,l),seq(0,15,0.5),rep(0,l))
wektor4 <- c(rep("d",l))
data4 <- cbind(d,wektor4,rep("q",l))


kolory <- brewer.pal(9,"Greens")


data <- rbind(data,data2,data3,data4)

lancuchyGora <- rbind(c(84,50,100,50,"a","p"),c(76,25,100,25,"a","p"),
                      c(16,50,0,50,"a","q"),c(24,25,0,25,"a","q"))
lancuchySrodek <- rbind(c(69,50,100,50,"b","p"),c(50,25,100,25,"b","p"),
                        c(31,50,0,50,"b","q"),c(50,25,0,25,"b","q"))
lancuchyDol <- rbind(c(50,50,100,50,"c","p"),c(25,25,100,25,"c","p"),
                     c(50,50,0,50,"c","q"),c(75,25,0,25,"c","q"))

gwiazda <- rbind(c(100,88,0,0,"a","p"),c(0,88,0,0,"a","q"))

swiatelka <- rbind(c(80,20,1,0,"a","p"),c(85,35,2,0,"a","p"),c(90,40,1,0,"a","p"),
                   c(20,20,2,0,"a","q"),c(15,35,1,0,"a","q"),c(10,40,2,0,"a","q"),
                   c(60,30,1,0,"b","p"),c(70,40,2,0,"b","p"),c(90,50,1,0,"b","p"),
                   c(40,30,2,0,"b","q"),c(30,40,1,0,"b","q"),c(10,50,2,0,"b","q"),
                   c(30,20,1,0,"c","p"),c(50,30,2,0,"c","p"),c(70,40,1,0,"c","p"),c(90,50,2,0,"c","p"),
                   c(70,20,2,0,"c","q"),c(50,30,1,0,"c","q"),c(30,40,2,0,"c","q"),c(10,50,1,0,"c","q"))

data <- rbind(data,lancuchyGora,lancuchySrodek,lancuchyDol,gwiazda, swiatelka)
data <- data.frame(data)
colnames(data) <- c("xstart","ystart","xend","yend","wiersz","kolumna")
data$xstart <- as.numeric(as.character(data[,1]))
data$ystart <- as.numeric(as.character(data[,2]))
data$xend <- as.numeric(as.character(data[,3]))
data$yend <- as.numeric(as.character(data[,4]))

##


ui <- fixedPage(
   
   titlePanel("Ubierz Choinkę FacetGrid"),
   sidebarLayout(
      sidebarPanel(
        selectInput("gwiazdka", "Gwiazdka", 
                    choices = c("TAK","NIE"), selected = "NIE"),
        checkboxGroupInput("lancuch", 
                           "Łańcuchy:", 
                           choices = c("Góra","Środek","Dół"),
                           selected = FALSE),
         sliderInput("light",
                     "Oświetlenie:",
                     min = 0,
                     max = 4,
                     value = 0),
        fluidRow(
          actionButton("posprzataj", "Posprzątaj po świętach"),
          actionButton("ups", "UPS"))
      ),
      mainPanel(
         plotOutput("choinkaPlot",width="60%",height = "450px")
      )
   )
)


server <- function(input, output) {
  v <- reactiveValues(flag=TRUE)
  choinka <- reactive({
     
     p <- ggplot(data) + geom_segment(data=data[1:466,],aes(x=xstart,y=ystart,xend=xend, yend=yend, color=wiersz)) +
       facet_grid(vars(wiersz),vars(kolumna), margin=FALSE) + theme_minimal() +
       guides(color=FALSE) + scale_color_manual(values = c(kolory[6], kolory[7], kolory[8], "brown")) +
       scale_x_continuous(expand = c(0, 0)) +
        scale_y_continuous(expand = c(0, 0))
     
     if("Góra" %in% input$lancuch){
       p <- p + geom_curve(data=data[467:470,],aes(x=xstart,y=ystart,xend=xend, yend=yend), 
                           curvature = -0.1,size=2.5, color="grey")
     }
     if("Środek" %in% input$lancuch){
       p <- p + geom_curve(data=data[471:474,],aes(x=xstart,y=ystart,xend=xend, yend=yend), 
                           curvature = -0.1,size=2.5, color="grey")
     }
     if("Dół" %in% input$lancuch){
       p <- p + geom_curve(data=data[475:478,],aes(x=xstart,y=ystart,xend=xend, yend=yend), 
                           curvature = -0.1,size=2.5, color="grey")
     }
     if(input$gwiazdka=="TAK"){
       p <- p + geom_point(data=data[479:480,],aes(x=xstart,y=ystart), 
                           size = 30, color = "darkgoldenrod1", pch="*")
     }
     if(input$light>0){
       p <- p + geom_point(data=data[481:500,], aes(x=xstart,y=ystart, fill = as.factor(xend)),
                           size = input$light+2, pch=24, colour = "transparent") +
         scale_fill_manual(values=brewer.pal(3,"Set1")[1:2]) + guides(fill=FALSE)
     }
     
     
     library(grid)
     height <- 1e-6 # Vertical spacing
     aux <- 1e-5 # Auxiliary number to identify 'height' among other heights
     width <- 1e-6 # Desirable horizontal spacing
     
     p <- p + theme(panel.spacing = unit(height + aux, "lines"))
     

     if(v$flag==FALSE){
       ggplot()
     } else {
       p
     }
     
  })
   
   
   posprzataj <- observeEvent(input$posprzataj, {
     v$flag <- FALSE
   })
   
   ups <- observeEvent(input$ups, {
     v$flag <- TRUE
   })
   
   output$choinkaPlot <- renderPlot({
     p <- choinka() + labs(x="",y="")
      p
   })
}


shinyApp(ui = ui, server = server)

