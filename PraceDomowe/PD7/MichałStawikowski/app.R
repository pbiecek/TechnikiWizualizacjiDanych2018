library(shiny)
library(shinydashboard)
library(reshape2)
library(dplyr)
library(ggplot2)
library(openxlsx)
library(data.table)

t <- as.data.table(read.xlsx("Statystyki_2000-2018_wer20180828.xlsx",1))[
  , .(mean(Śr..zimowa)), by = .(Nazwa.strefy, Rok)
  ]
colnames(t) <- c("Strefa", "Rok", "Wartosc")
choice <- "Lowest"

t2 <- as.data.table(read.xlsx("Statystyki_2000-2018_wer20180828.xlsx",7))[
  , .(mean(Średnia)), by = .(Nazwa.strefy, Rok)
  ]
colnames(t2) <- c("Strefa", "Rok", "Wartosc")


t3 <- as.data.table(read.xlsx("Statystyki_2000-2018_wer20180828.xlsx",8))[
  , .(mean(Średnia)), by = .(Nazwa.strefy, Rok)
  ]
colnames(t3) <- c("Strefa", "Rok", "Wartosc")


ui <- dashboardPage(
  skin = "black",
  dashboardHeader(title = "Air pollution in Poland"
                  
                 ),
  dashboardSidebar(
    sidebarUserPanel(Sys.info()[["effective_user"]],
                     subtitle = a(href = "#", icon("circle", class = "text-success"), "Online")
    ),
    sidebarMenu(
      # Setting id makes input$tabs give the tabName of currently-selected tab
      id = "tabs",
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("About", icon = icon("info-circle"), tabName = "about", badgeLabel = "new",
               badgeColor = "green"),
      sliderInput("rok",
                  label = "Year:",
                  min = 2000, max = 2017,
                  value = 2017),
      sliderInput("topn",
                  label = "Number of top places:",
                  min = 1, max = 10,
                  value = 5),
      radioButtons("wybor",
                   label = "Pollution:",
                   choices = c("Lowest", "Highest"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem("dashboard",
              plotOutput("SO2"),
              plotOutput("PM10"),
              plotOutput("PM2,5")
      ),
      tabItem("about",
              "The appplication enables comparison of the most
and least polluted places in Poland in terms of three selected substances:
- SO2
- PM10
- PM2,5"
      )
    )
  )
)

server <- function(input, output) {
  output[["SO2"]] <- renderPlot(
    if(choice == input$wybor) {
      t <- na.omit(t[Rok == as.numeric(input$rok), ][order(as.numeric(as.character(Wartosc)))][1:input$topn, ])
      ggplot(data = t, aes(x = reorder(Strefa, as.numeric(as.character(Wartosc))),
                           y = as.numeric(as.character(Wartosc)))) + 
        geom_col(colour="black",fill = "dodgerblue4") +
        scale_y_continuous(limits = c(0, 52)) +
        theme_gray() + geom_text(
          aes(label = round(as.numeric(as.character(Wartosc)),2), y =as.numeric(as.character(Wartosc)) + 1),
          position = position_dodge(0.9),
          vjust = 0
        ) + 
        labs(x = "Places", y = "SO2 concentration [µg/m^3]") + ggtitle("SO2") +
        theme(plot.title = element_text(face = "bold", size = (15)), 
              axis.title = element_text(size = (12),face = "bold"),
              axis.text = element_text(size = (10)))
    } else {
      t <- na.omit(t[Rok == as.numeric(input$rok), ][order(-as.numeric(as.character(Wartosc)))][1:input$topn, ])
      ggplot(data = t, aes(x = reorder(Strefa, -as.numeric(as.character(Wartosc))), 
                           y = as.numeric(as.character(Wartosc)))) + 
        geom_col(colour="black",fill = "dodgerblue4") +
        scale_y_continuous(limits = c(0, 52)) +
        theme_gray() + geom_text(
          aes(label = round(as.numeric(as.character(Wartosc)),2), y =as.numeric(as.character(Wartosc)) + 1),
          position = position_dodge(0.9),
          vjust = 0
        ) + 
        labs(x = "Places", y = "SO2 concentration [µg/m^3]") + ggtitle("SO2") +
        theme(plot.title = element_text(face = "bold", size = (15)), 
              axis.title = element_text(size = (12),face = "bold"),
              axis.text = element_text(size = (10)))
    } 
  )
  
  output[["PM10"]] <- renderPlot(
    if(choice == input$wybor) {
      t2 <- na.omit(t2[Rok == as.numeric(input$rok), ][order(as.numeric(as.character(Wartosc)))][1:input$topn, ])
      ggplot(data = t2, aes(x = reorder(Strefa, as.numeric(as.character(Wartosc))),
                           y = as.numeric(as.character(Wartosc)))) + 
        geom_col(colour="black",fill = "firebrick4") +
        scale_y_continuous(limits = c(0, 70)) +
        theme_gray() + geom_text(
          aes(label = round(as.numeric(as.character(Wartosc)),2), y =as.numeric(as.character(Wartosc)) + 1),
          position = position_dodge(0.9),
          vjust = 0
        ) + 
        labs(x = "Places", y = "PM10 concentration [µg/m^3]") + ggtitle("PM10") +
        theme(plot.title = element_text(face = "bold", size = (15)), 
              axis.title = element_text(size = (12),face = "bold"),
              axis.text = element_text(size = (10)))
    } else {
      t2 <- na.omit(t2[Rok == as.numeric(input$rok), ][order(-as.numeric(as.character(Wartosc)))][1:input$topn, ])
      ggplot(data = t2, aes(x = reorder(Strefa, -as.numeric(as.character(Wartosc))), 
                           y = as.numeric(as.character(Wartosc)))) + 
        geom_col(colour="black",fill = "firebrick4") +
        scale_y_continuous(limits = c(0, 70)) +
        theme_gray() + geom_text(
          aes(label = round(as.numeric(as.character(Wartosc)),2), y =as.numeric(as.character(Wartosc)) + 1),
          position = position_dodge(0.9),
          vjust = 0
        ) + 
        labs(x = "Places", y = "PM10 concentration [µg/m^3] ") + ggtitle("PM10") +
        theme(plot.title = element_text(face = "bold", size = (15)), 
              axis.title = element_text(size = (12),face = "bold"),
              axis.text = element_text(size = (10)))
    } 
  )
  
  output[["PM2,5"]] <- renderPlot(
    if(choice == input$wybor) {
      t3 <- na.omit(t3[Rok == as.numeric(input$rok), ][order(as.numeric(as.character(Wartosc)))][1:input$topn, ])
      ggplot(data = t3, aes(x = reorder(Strefa, as.numeric(as.character(Wartosc))),
                           y = as.numeric(as.character(Wartosc)))) + 
        geom_col(colour="black",fill = "olivedrab4") +
        scale_y_continuous(limits = c(0, 52)) +
        theme_gray() + geom_text(
          aes(label = round(as.numeric(as.character(Wartosc)),2), y =as.numeric(as.character(Wartosc)) + 1),
          position = position_dodge(0.9),
          vjust = 0
        ) + 
        labs(x = "Places", y = "PM2,5 concentration [µg/m^3]") + ggtitle("PM2,5") +
        theme(plot.title = element_text(face = "bold", size = (15)), 
              axis.title = element_text(size = (12),face = "bold"),
              axis.text = element_text(size = (10)))
    } else {
      t3 <- na.omit(t3[Rok == as.numeric(input$rok), ][order(-as.numeric(as.character(Wartosc)))][1:input$topn, ])
      ggplot(data = t3, aes(x = reorder(Strefa, -as.numeric(as.character(Wartosc))), 
                           y = as.numeric(as.character(Wartosc)))) + 
        geom_col(colour="black",fill = "olivedrab4") +
        scale_y_continuous(limits = c(0, 52)) +
        theme_gray() + geom_text(
          aes(label = round(as.numeric(as.character(Wartosc)),2), y =as.numeric(as.character(Wartosc)) + 1),
          position = position_dodge(0.9),
          vjust = 0
        ) + 
        labs(x = "Places", y = "PM2,5 concentration [µg/m^3]") + ggtitle("PM2,5") +
        theme(plot.title = element_text(face = "bold", size = (15)), 
              axis.title = element_text(size = (12),face = "bold"),
              axis.text = element_text(size = (10)))
    } 
  )
}

shinyApp(ui, server)