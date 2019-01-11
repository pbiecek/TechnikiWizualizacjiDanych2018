library(shiny)
library(dplyr)
library(ggplot2)
library(reshape2)
library(ggrepel)
options(stringsAsFactors = FALSE)

t = 300
tab <- c()
tab <- rbind(numeric(100), tab)
for (i in 1:t){
  tab = rbind(tab, rnorm(100, sd=c(95:1)))
}

tab <- as.data.frame(tab)

dane <- melt(data = tab, variable.name = "V1")



ui <- fluidPage(
  
  sidebarLayout(
    sidebarPanel(
      checkboxInput(inputId = "gwiazdka",
                    label = "Gwiazda",
                    value = TRUE),
      sliderInput(inputId = "bombki",
                  label = "Liczba bombek",
                  min = 0,
                  max = 300,
                  value = 100),
      sliderInput(inputId = "snieg",
                  label = "Liczba płatków śniegu",
                  min = 0,
                  max = 10000,
                  value = 2000)
     
      
    ),
    
    mainPanel(
      h2("Prawdopodobna choinka"),
      plotOutput("choinka_plot", height = 600, width = 700)
    )
  )
  
)

server <- function(input, output, session) {
  
  output[["choinka_plot"]] <- renderPlot({
    p <- ggplot(data = dane, aes()) +
      geom_point(col = 3, aes(y = V1, x = value)) 

    p <- p + geom_point(data = dane[sample(1000:(nrow(dane)-801), input[["snieg"]]),], aes(y = V1, x = value), col = "white") +
      geom_point(data = dane[(nrow(dane)-1600):nrow(dane), ], aes(y = V1, x = value), col = "blue") +
      geom_point(data = dane[1:2000, ], aes(y = V1, x = value), col = "blue") +
      geom_rect(ymin = 1, ymax = 7.7, xmin = -30, xmax = 30, fill = "brown") +
      geom_point(data = dane[sample(2000:(nrow(dane)-4000), input[["bombki"]]),], aes(y = V1, x = value), 
                 col = rgb(runif(input[["bombki"]]), runif(input[["bombki"]]), runif(input[["bombki"]]), 1), size = 10) +
      theme(panel.background = element_rect(fill = rgb(0, 0, 1, 1)), panel.grid = element_blank(), 
            axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), plot.background = element_rect(size = 100000), 
            title = element_blank())
    
    if(input[["gwiazdka"]]){
      p <- p + geom_point(data = dane[nrow(dane)-1601,], aes(y = V1, x = value), shape = 8, col = "gold", size = 10, stroke = 4)
    }
    p
    
    
    })
  
}

shinyApp(ui, server)