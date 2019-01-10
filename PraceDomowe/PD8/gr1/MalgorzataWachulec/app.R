library(shiny)
library(ggiraph)
library(ggplot2)
library(gridExtra)

x1 <- seq(0,1002,2)
x2 <- seq(1,1003,2)
y1 <- rep(0,502)
y2 <- c(seq(0,150,1),rep(150,200),seq(150,0,-1))
b1 <- c(100,500,300,800,600,350)
c1 <- c(35,100,120,40,30,70)
col1 <- c("red","salmon","tomato","ivory","gold","darkred")

x1b <- seq(150,850,2)
x2b <- seq(151,851,2)
y1b <- rep(0,351)
y2b <- c(seq(0,150,150/105),rep(150,139),seq(150,0,-150/105))
b2 <- c(200,500,750,600,350)
c2 <- c(15,100,40,90,70)
col2 <- c("ivory","coral","darkred","salmon","brown")

x1c <- seq(250,750,2)
x2c <- seq(251,751,2)
y1c <- rep(0,251)
y2c <- c(seq(0,150,150/124), seq(150,0,-150/125))
b3 <- c(300,500,600,400)
c3 <- c(15,40,90,70)
col3 <- c("gold","red","ivory","coral")

ui <- fluidPage(

   titlePanel("Choinka z dawnych fotografii, która dzięki ggirafe sama się przedstawia - pamiętaj na nią najechać!"),

   sidebarLayout(
      sidebarPanel(
         radioButtons("typ",
                      label = NULL,
                      choiceNames = c("Chcę choinkę z dawnych fotografii","Chcę kolorową choinkę"),
                      choiceValues = c("dawna","nowa"),
                      selected = "dawna")
      ),

      mainPanel(
        ggiraphOutput("choinka")
      )
   )
)


server <- function(input, output, session) {
   
  tekst <- reactive({
    if(input$typ == "dawna") {
      "Jestem czarną bombką"
    } else{
      "Jestem kolorową bombką"
    }
  })
  
  tekst2 <- reactive({
    if(input$typ == "dawna") {
      "Jestem choinką z dawnych fotografii"
    } else{
      "Jestem kolorową choinką - mało oryginalną"
    }
  })
  
  kolor <- reactive({
    if(input$typ == "dawna"){
      TRUE
    }else{
      FALSE
    }
  })
  
   output$choinka <- renderggiraph({
     
     if(kolor()){
     margin = theme(plot.margin = unit(c(-0.07,-0.07,-0.07,-0.07),"cm"))
     
     p1 <- ggplot() + geom_rect_interactive(aes(xmin=x1,xmax=x2,ymin=y1,ymax=y2,tooltip = tekst2())) + 
       geom_point_interactive(aes(x = b1, y=c1,tooltip = tekst()),size = 6) +
       geom_rect_interactive(aes(xmin=481,xmax=521,ymin=-100,ymax=0,tooltip = "Jestem pieńkiem"))+
       scale_x_continuous(limits = c(0,1002)) + scale_y_continuous(expand = c(0,0)) + theme_minimal() +
       theme(axis.text = element_blank(),axis.title = element_blank(), panel.grid = element_blank()) + margin 
     
     p2 <- ggplot() + geom_rect_interactive(aes(xmin=x1b,xmax=x2b,ymin=y1b,ymax=y2b,tooltip = tekst2()))+ 
       geom_point_interactive(aes(x = b2, y=c2,tooltip = tekst() ),size = 6) + scale_x_continuous(limits = c(0,1002)) + 
       scale_y_continuous(expand = c(0,0))  + theme_minimal()+ theme(
       axis.text = element_blank(),axis.title = element_blank(), panel.grid = element_blank()
     ) + margin 
     
     p3 <- ggplot() + geom_rect_interactive(aes(xmin=x1c,xmax=x2c,ymin=y1c,ymax=y2c,tooltip = tekst2()))+ 
       geom_point_interactive(aes(x = b3, y=c3,tooltip = tekst() ),size = 6)+
       geom_polygon_interactive(aes(tooltip = "Jestem szarą gwiazdą",x=c(501,531,521,541,516,501,486,461,481,471),y=c(130,110,145,160,160,190,160,160,145,110)),fill = "grey80")+
       scale_x_continuous(limits = c(0,1002)) + scale_y_continuous(expand = c(0,0),limits = c(0,200))  + theme_minimal()+ theme(
         axis.text = element_blank(),axis.title = element_blank(), panel.grid = element_blank()
       ) + margin
     
     ggiraph(code =grid.arrange(p3,p2,p1, ncol = 1,heights=c(1,1,2)) )
     
     } else {
       margin = theme(plot.margin = unit(c(-0.07,-0.07,-0.07,-0.07),"cm"))
       p1 <- ggplot() + geom_rect_interactive(aes(xmin=x1,xmax=x2,ymin=y1,ymax=y2,tooltip = tekst2()),
                                              col=rep(c("forestgreen","darkgreen"),251)) + 
         geom_point_interactive(aes(x = b1, y=c1,tooltip = tekst()), col = col1, size = 6) +
         geom_rect_interactive(aes(xmin=481,xmax=521,ymin=-100,ymax=0,tooltip = "Jestem pieńkiem"),fill="brown")+
         scale_x_continuous(limits = c(0,1002)) + scale_y_continuous(expand = c(0,0),limits = c(-100,150)) + theme_minimal() +
         theme(axis.text = element_blank(),axis.title = element_blank(), panel.grid = element_blank()) + margin 
       
       p2 <- ggplot() + geom_rect_interactive(aes(xmin=x1b,xmax=x2b,ymin=y1b,ymax=y2b,tooltip = tekst2()),
                                              col=c(rep(c("forestgreen","darkgreen"),175),"forestgreen"))+ 
         geom_point_interactive(aes(x = b2, y=c2,tooltip = tekst() ), col = col2, size = 6) + scale_x_continuous(limits = c(0,1002)) + 
         scale_y_continuous(expand = c(0,0))  + theme_minimal()+ theme(
           axis.text = element_blank(),axis.title = element_blank(), panel.grid = element_blank()
         ) + margin 
       
       p3 <- ggplot() + geom_rect_interactive(aes(xmin=x1c,xmax=x2c,ymin=y1c,ymax=y2c,tooltip = tekst2()),
                                              col=c(rep(c("forestgreen","darkgreen"),125),"forestgreen"))+ 
         geom_point_interactive(aes(x = b3, y=c3,tooltip = tekst() ), col = col3, size = 6)+
         geom_polygon_interactive(aes(tooltip = "Jestem złotą gwiazdą",x=c(501,531,521,541,516,501,486,461,481,471),y=c(130,110,145,160,160,190,160,160,145,110)),fill = "gold")+
         scale_x_continuous(limits = c(0,1002)) + scale_y_continuous(expand = c(0,0),limits = c(0,200))  + theme_minimal()+ theme(
           axis.text = element_blank(),axis.title = element_blank(), panel.grid = element_blank()
         ) + margin
       
       ggiraph(code =grid.arrange(p3,p2,p1, ncol = 1,heights=c(1,1,2)) )
     }
   })
}


shinyApp(ui = ui, server = server)

