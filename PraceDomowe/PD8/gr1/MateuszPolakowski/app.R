library(shiny)
library(ggplot2)
library(data.table)
library(rsconnect)

choinka <- c("Mała", "Średnia", "Wielka!")
prezenty <- c("Wcale!", "Może trochę", "W miarę..", "Bardzo!")
temp <- pi*(seq(0, 60, 0.1) - 30)/180
choinka_w <- data.table(cbind(0, 0))

ui <- fluidPage(
  titlePanel("Tegoroczna choinka"),
  sidebarLayout(
    sidebarPanel(
      radioButtons("czy_duza_choinka",
                   "Jak wielka ma być choinka?",
                   choinka,
                   selected = "Średnia"),
      sliderInput("ile_ozdob",
                  "Ile bombek zawiesić na choince?",
                  min = 0, max = 25, value = 10),
      checkboxInput("z_gwiazda",
                    "Czy włożyć gwiazdę na samą górę?",
                    TRUE),
      downloadButton(outputId = "downloadPlot.png",
                     label = "Pobierz wykres choinki!")
    ),
    mainPanel(plotOutput("wykres_choinki", height = 660, width = 660))
  )
)

server <- function(input, output) {
  
  wykres_choinki <- reactive({
    tt <- match(input$czy_duza_choinka, choinka) * 2
    
    for(i in 1:tt) {
      t <- runif(i*500, 0, 60) - 30
      r <- runif(i*500, i-1, i)
      
      choinka_w <- rbind(
        choinka_w,
        cbind(sin(temp) * i, -cos(temp) * i),
        cbind(sin(pi * t / 180) * r, 
              -cos(pi * t / 180) * r)
      )
    }
    colnames(choinka_w) <- c("X", "Y")
    
    pien <- data.table(rbind(
      c(-tt/10, -tt),
      c(tt/10, -tt),
      c(tt/10, -1.2*tt),
      c(-tt/10, -1.2*tt)
    ))
    colnames(pien) <- c("X", "Y")
    
    pl <- ggplot() + 
      geom_polygon(data = pien, aes(x = X, y = Y), fill = "#a0522d") +
      geom_point(data = choinka_w, aes(x = X, y = Y), size = 2, color = "#4d9e3a") +
      scale_x_continuous(breaks = NULL) +
      labs(y = "", x = "") +
      theme_bw()
    
    if(input$z_gwiazda == TRUE) {
      gwiazda <- data.table(cbind(0, 0))
      colnames(gwiazda) <- c("X", "Y")
      
      pl <- pl +
        geom_point(data = gwiazda, aes(x = X, y = Y), size = 15, color = "yellow")
    }
    
    bombki <- as.data.table(choinka_w[sample(1:nrow(choinka_w), input$ile_ozdob), ])
    
    pl <- pl +
      geom_point(data = bombki, aes(x = X, y = Y), color = "blue", size = 7)
  })
  
  output$wykres_choinki <- renderPlot({
    
    pl <- wykres_choinki()
    pl
  })
  
  output$downloadPlot.png <- downloadHandler(
    filename = function() { "choinka.png" },
    content = function(file) {
      ggsave(file, plot = wykres_choinki(), device = "png", width = 14, height = 14)
    }
  )
}

shinyApp(ui, server)