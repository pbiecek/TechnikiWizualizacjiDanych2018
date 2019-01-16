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
                                                "Do czarno-białego druku" = "Greys9", "Marsjańska" = "OrRd9")),
      sliderInput("baubles", "Ilość bombek", 13, 75, 42),
      radioButtons("baublesCol", "Kolor bombek", c("Czerwień nocy" = "red", "Zemsta karmazynu" = "#DC143C",
                                                   "Zmrożona Coca-Cola" = "#F40000", "Kardynalskie marzenia" = "#FF2400",
                                                   "Apetyczny mak" = "#E21E13"))
    ),
    mainPanel = mainPanel(
      rbokehOutput(outputId = "christmasPlot")
    )
  )
)

server <- function(input, output) {
  dane_r <- reactive({
    transmute(dane, y = c(rlnorm(10000, 0, 0.5), rlnorm(5000, 0, 0.35)+2.5, rlnorm(2500, 0, 0.22)+4.5))
    mutate(dane, x = rnorm(17500, sd = input[["shape"]])/(y+input[["shape"]]))
  })
  
  output[["christmasPlot"]] <- renderRbokeh({
    maks <- which.max(dane_r()[, "y"])
    baubles <- sample_n(dane_r(), input[["baubles"]], weight = exp((dane_r()[, "x"])^2))
    figure(xlab = NULL, ylab = NULL) %>%
      ly_hexbin(x = dane_r()[, "x"], y = dane_r()[, "y"], palette = input[["colour"]]) %>%
      ly_points(x = dane_r()[maks, "x"], y = dane_r()[maks, "y"],
                color = ifelse(input[["colour"]] == "Greys9" ,"gray" , "gold"), glyph = 18) %>%
      ly_points(x = baubles[, "x"], y = baubles[, "y"],
                color = ifelse(input[["colour"]] == "Greys9" ,"gray" , input[["baublesCol"]]), glyph = 20)
  })
}

shinyApp(ui = ui, server = server)