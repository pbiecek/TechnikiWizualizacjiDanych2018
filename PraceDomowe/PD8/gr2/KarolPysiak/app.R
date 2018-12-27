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
  
  
  # countries_r <- reactive({
  #   filter(skoki, Kraj %in% input[["chosen_country"]])%>% 
  #     mutate(chosen_name = Kraj == input[["chosen_country"]])
  #   
  # })
  # 
  # jumpers_r <- reactive({
  #   filter(countries_r(), Skoczek %in% input[["chosen_jumper"]])
  # })
  # 
  # all_r <- reactive({
  #   if (input[["byJumper"]]) {
  #     jumpers_r()
  #   } else {
  #     countries_r()
  #   }
  # })
  # 
  # model_r <- reactive({
  #   lm(Skok ~ Punkty, data = all_r())
  # })
  # 
  # observe({
  #   updateSelectizeInput(session, inputId = "chosen_jumper", 
  #                        selected = input[["chosen_jumper"]],
  #                        choices = countries_r()[["Skoczek"]])
  # })
  
  
  # output[["jumpers_plot"]] <- renderPlot({
  #   p <- ggplot(all_r(), aes(x=Punkty, y=Skok, label=Skoczek)) + 
  #     theme_bw() +
  #     geom_point(stat='identity', color = rgb(input[["red"]]/255, input[["green"]]/255, input[["blue"]]/255),  size=3)  +
  #     geom_text_repel(color="black", size=3, force=1) +
  #     ylim(min(all_r()[["Skok"]]), max(all_r()[["Skok"]])) +
  #     xlim(min(all_r()[["Punkty"]]), max(all_r()[["Punkty"]])) +
  #     ylab('Długość skoku') +
  #     coord_flip()
  #   
  #   if (input[["byJumper"]]) {
  #     p <- p + geom_text(data = all_r(), aes(x = Punkty, y = Skok, label = paste0("Punkty: ", all_r()[["Punkty"]], " pkt, Długość skoku: ", all_r()[["Skok"]], " m")), 
  #                        size = 10, color = "black", vjust = 5)
  #   } else if(input[["abline"]]){
  #     p <- p + geom_abline(slope = coef(model_r())[2], intercept = coef(model_r())[1], color = "black")
  #   }
  #   p
  # })
  
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