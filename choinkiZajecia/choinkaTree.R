library(rbokeh)
library(shiny)
library(dplyr)

dane <- data.frame(y = c(rlnorm(10000, 0, 0.5), rlnorm(5000, 0, 0.35)+2.5, rlnorm(2500, 0, 0.22)+4.5))

ui <- fluidPage(
  titlePanel("Choinka Tree"),
  sidebarLayout(
    sidebarPanel = sidebarPanel(
      sliderInput("shape", "Kształt choinki", 1, 8, 3),
      radioButtons("colour", "Kolor choinki", c("Zielona" = "Greens9", "Inaczej zielona" = "Greens7",
                                                "Mocno dyskretnie zielona" = "Greens4", "Zielona, ale z nadzieniem" = "PiYG11",
                                                "Do czarno-białego druku" = "Greys9", "Marsjańska" = "OrRd9"))
    ),
    mainPanel = mainPanel(
      rbokehOutput(outputId = "christmasPlot")
    )
  )
)

server <- function(input, output) {
  dane_r <- reactive({
    mutate(dane, x = rnorm(17500, sd = input[["shape"]])/(y+input[["shape"]]))
  })
  
  output[["christmasPlot"]] <- renderRbokeh({
    figure() %>%
      ly_hexbin(x = dane_r()[, "x"], y = dane_r()[, "y"], palette = input[["colour"]])
  })
}

shinyApp(ui = ui, server = server)
