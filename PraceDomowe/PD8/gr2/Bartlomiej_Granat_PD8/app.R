library(reshape2)
library(ggplot2)
dat <- c()
dat <- rbind(dat, numeric(100))
for (i in 1:50){
  dat <- rbind(dat, seq(-i,0, length.out = 100))
}
dat <- as.data.frame(dat)
dat <- melt(data = dat, variable.name = "V1")
dat1 <- dat
dat1$value <- -dat$value
dat <- rbind(dat1, dat)
ui <- fluidPage(
  
  sidebarLayout(
    sidebarPanel(
      sliderInput(inputId = "bombki",
                  label = "Liczba bombek",
                  min = 0,
                  max = 1000,
                  value = 300)
      
      
    ),
    
    mainPanel(
      titlePanel("Święta święta i po świętach"),
      plotOutput("choinka", height = 600, width = 700)
    )
  )
  
)

server <- function(input, output, session) {
  output[["choinka"]] <- renderPlot({

    ggplot(data = dat, aes(y=V1, x= value)) +
      geom_point(col =3) +
      geom_point(data = dat[sample(1:9500, input[["bombki"]]),], col = rgb(runif(input[["bombki"]]), runif(input[["bombki"]]), runif(input[["bombki"]]), 1), size = 3)+
      geom_point(data = dat[10050,], aes(y=V1, x=value), col = "gold", shape = 8, size = 7, fill = "yellow", stroke = 4)+
      theme(panel.background = element_rect(fill = "white", size = 100000),panel.grid = element_blank(), axis.title = element_blank(),
            axis.text = element_blank(), axis.ticks = element_blank(), 
            title = element_blank())
  })
  
  
}
shinyApp(ui, server)