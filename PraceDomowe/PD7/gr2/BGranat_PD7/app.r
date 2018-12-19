library(shiny)
library(shinydashboard)
library(reshape2)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(openxlsx)


df_25 <- readRDS("dane_PM25.rds")
df_10 <- readRDS("dane_PM10.rds")
df_25_miasta <- readRDS("dane_PM25_miasta.rds")
df_10_miasta <- readRDS("dane_PM10_miasta.rds")

ui <- dashboardPage(
  skin = "black",
  dashboardHeader(title = "Smog"
  ),
  dashboardSidebar(
    sidebarUserPanel(Sys.info()[["effective_user"]],
                     subtitle = a(href = "#", icon("circle", class = "text-success"), "Online")
    ),
    sidebarMenu(
      id = "tabs",
      menuItem("Wykresy", tabName = "warszawa", icon = icon("dashboard")),
      menuItem("Informacje", icon = icon("info-circle"), tabName = "about")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem("warszawa",
              plotOutput("my_plot1"),
              selectInput("stacje1",
                          label = 'Wybierz stacje 1',
                          choices = as.list(c('Ursynow', 'Targowek'))
              ),
              selectInput("stacje2",
                          label = 'Wybierz stacje 2',
                          choices = as.list(c('Srodmiescie1', 'Srodmiescie2','Bielany'))
              ),
              plotOutput(("my_plot2")),
              selectInput("stacje3",
                          label = 'Wybierz miasto 1',
                          choices = as.list(c("Wroclaw", "Lodz", "Krakow", "Rzeszow","Gdansk","Katowice","Warszawa","Poznan"))
              )
      ),
      tabItem("about",
              "Dashboard umożliwia eksplorację poziomów pyłu w powietrzu
              na przestrzeni roku 2017 dla Warszawy oraz na przestrzeni stycznia 2017 dla największych miast Polski"
      )
    )
  )
)

server <- function(input, output) {
  output[["my_plot1"]] <- renderPlot({
    plot1<-ggplot(df_25, aes_string('Date', input$stacje1)) + geom_line(color = "steelblue") + 
      theme_minimal() + ggtitle("Wykres pyłu PM 2.5 w powietrzu")
    plot2<-ggplot(df_10, aes_string('Date', input$stacje2)) + geom_line(color = "steelblue") + 
      theme_minimal() + ggtitle("Wykres pyłu PM 10 w powietrzu")
    grid.arrange(plot1,plot2,ncol=2)
  })
  output[["my_plot2"]] <- renderPlot({
    plot1<-ggplot(df_25_miasta, aes_string('Date', input$stacje3)) + geom_line(color = "steelblue") + 
      theme_minimal() + ggtitle("Wykres pyłu PM 2.5 w powietrzu")
    plot2<-ggplot(df_10_miasta, aes_string('Date', input$stacje3)) + geom_line(color = "steelblue") + 
      theme_minimal() + ggtitle("Wykres pyłu PM 10 w powietrzu")
    grid.arrange(plot1,plot2,ncol=2)
  })
  
}

shinyApp(ui, server)