library(shiny)
library(SmarterPoland)
library(dplyr)

ui <- fluidPage(
  
  titlePanel("Simple Shiny App"),
  
  sidebarLayout(
    sidebarPanel(
      selectizeInput(inputId = "chosen_country", 
                     label = "Select a country name:",
                     choices = countries[["country"]]),
      checkboxGroupInput(inputId = "chosen_continent",
                         label = "Select continent names:",
                         choices = unique(countries[["continent"]]),
                         selected = unique(countries[["continent"]])),
      checkboxInput(inputId = "abline", "Add regression line")
    ),
    
    mainPanel(
      h2("Scatterplot"),
      plotOutput("countries_plot", height = 600),
      h2("Population"),
      textOutput("countries_text"),
      h2("Model"),
      verbatimTextOutput("lm")
    )
  )
)

server <- function(input, output, session) {
  
  continent_colors <- c(Asia = "red", Europe = "green", Africa = "orange", Americas = "black", 
                        Oceania = "blue")
  
  countries_r <- reactive({
    filter(countries, continent %in% input[["chosen_continent"]]) %>% 
      mutate(chosen_name = country == input[["chosen_country"]])
  })
  
  model_r <- reactive({
    lm(death.rate ~ birth.rate, data = countries_r())
  })
  
  observe({
    updateSelectizeInput(session, inputId = "chosen_country", 
                         choices = countries_r()[["country"]])
  })
  
  output[["countries_plot"]] <- renderPlot({
    p <- ggplot(countries_r(), aes(x = birth.rate, y = death.rate, color = continent, 
                                   size = chosen_name, shape = chosen_name)) +
      geom_point() +
      scale_color_manual(values = continent_colors[input[["chosen_continent"]]]) +
      theme_bw()
    
    if(input[["abline"]])
      p <- p + geom_abline(slope = coef(model_r())[2], intercept = coef(model_r())[1])
    
    p
  })
  
  output[["countries_text"]] <- renderText({
    paste0("The current population of ", filter(countries_r(), chosen_name)[["country"]], 
           " is ", filter(countries_r(), chosen_name)[["population"]], ".")
  })
  
  output[["lm"]] <- renderPrint({
    print(summary(model_r()))
  })
}

shinyApp(ui = ui, server = server)

