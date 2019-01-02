library(shiny)
library(rbokeh)
library(colourpicker)

ui <- fluidPage(
   
   # Application title
   titlePanel("Wesołych Świąt i Szczęśliwego Nowego Roku!!!"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput("bombki",
                     "Podaj liczbę bombek:",
                     min = 1,
                     max = 100,
                     value = 50),
         sliderInput("gen",
                     "Podaj ziarno generatora bombek:",
                     min = 1,
                     max = 1000,
                     value = 755),
         colourInput("col", "Wybierz kolor łańcucha: ", value="red", showColour = "background"),
         colourInput("col_bom", "Wybierz kolor bombek: ", value="blue", showColour = "background")
      ),
      
      mainPanel(
         rbokehOutput("choinka")
      )
   )
)

server <- function(input, output) {
   choinka_data <- data.frame(y=seq(100,1),x0=seq(-1,-100), x1=seq(1,100))
   a <- reactive(as.integer(runif(input[["bombki"]],0,100)))
   kolor <- reactive({
     input[["col"]]
   })
   kolor_bombek <- reactive({
     input[["col_bom"]]
   })
   
   gen <- reactive(input[["gen"]])
   output[["choinka"]] <- renderRbokeh({
     kolor1 <- rep("#3ead15",100)
     kolor1[seq(7,100,by=10)] <- kolor()
     a <- a()
     set.seed(gen())
     suppressWarnings(figure(width = 350, height = 400, xgrid=FALSE, ygrid=FALSE,xaxes = FALSE, yaxes = FALSE) %>% 
       ly_segments(x0,y,x1,y, data=choinka_data, color=kolor1,type=1, line_width=4) %>% 
       ly_segments(rep(-20,10),seq(0,-10),rep(20,10),seq(0,-10),color = "brown", line_width=4) %>%
       ly_points(c(-50,-35),c(-5,-7), glyph = "square_cross", size=c(20,10), color = c("blue","red")) %>%
       ly_points(lapply(a,function(a)as.integer(runif(1,-100+a,100-a))),a, size=12.5-a/10, fill_alpha=1, hover=a, color = kolor_bombek()) %>%
       ly_points(0,100, glyph="asterisk", size=40, color="gold", line_width=5))
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

