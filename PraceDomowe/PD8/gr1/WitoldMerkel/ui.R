library(shiny)


shinyUI(fluidPage(
  titlePanel("Interaktywna choinka - PD 8"),
  sidebarLayout(
    sidebarPanel(
      radioButtons(inputId = "Prezenty",
                   label = "Czy byliśmy grzeczni?",
                   choices = c("Nie", "Tak")),
      radioButtons(inputId = "Stan",
                   label = "W jakim stanie jest choinka?",
                   choices = c("Nówka sztuka", "Widziała lepsze dni")
        
      )
    ),
    mainPanel(
      plotOutput("choinka"),
      downloadButton(outputId = "Download1", label = "Pobierz choinkę, aby można było ją wyrzucić")
    )
  )
))