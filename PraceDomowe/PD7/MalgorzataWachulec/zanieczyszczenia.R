library(shiny)
library(shinydashboard)
library(reshape2)
library(data.table)
library(ggplot2)
library(lubridate)

dane25 <- data.table(read.csv2("dane25.csv"))
dane25 <- dane25[, .(Dzien,Wroclaw,Lodz,Krakow,Warszawa,Poznan)]
dane25$Dzien <- parse_date_time(dane25$Dzien,"ymd")
tmp25 <- melt(dane25,id.vars = 'Dzien')

dane10 <- data.table(read.csv2("dane10.csv"))
dane10 <- dane10[, .(Dzien,Wroclaw,Lodz,Krakow,Warszawa,Poznan)]
dane10$Dzien <- parse_date_time(dane10$Dzien,"ymd")
tmp10 <- melt(dane10,id.vars = 'Dzien')
temp10 <- dane10
temp10[is.na(temp10)] <- 0
temp10$Wroclaw <- ifelse(temp10$Wroclaw <= 50, 1, ifelse(temp10$Wroclaw <= 200, 2, 3))
temp10$Lodz <- ifelse(temp10$Lodz <= 50, 1, ifelse(temp10$Lodz <= 200, 2, 3))
temp10$Krakow <- ifelse(temp10$Krakow <= 50, 1, ifelse(temp10$Krakow <= 200, 2, 3))
temp10$Warszawa <- ifelse(temp10$Warszawa <= 50, 1, ifelse(temp10$Warszawa <= 200, 2, 3))
temp10$Poznan <- ifelse(temp10$Poznan <= 50, 1, ifelse(temp10$Poznan <= 200, 2, 3))
temp10 <- temp10[, .(Wroclaw,Lodz,Krakow,Warszawa,Poznan)]

ui <- dashboardPage(
  skin = "black",
  dashboardHeader(title = "Zanieczyszczenie"),
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
      menuItem(checkboxGroupInput("miasto",
                            "Wybierz miasta:",
                            choiceNames = c("Warszawa","Kraków","Wrocław","Łódź","Poznań"),
                            choiceValues = c("Warszawa","Krakow","Wroclaw","Lodz","Poznan"),
                            selected = "Warszawa")),
      menuItem(selectInput("chemical",
                           "Wybierz zanieczyszczenie:",
                           choices = c("PM 2.5","PM 10"),
                           selected = "PM 2.5")),
      menuItem(checkboxInput("trend",
                             "Czy dodać linię trendu?")),
      menuItem(radioButtons("miasto1",
                                  "Wybierz pierwsze\nmiasto do porównania:",
                                  choiceNames = c("Warszawa","Kraków","Wrocław","Łódź","Poznań"),
                                  choiceValues = c("Warszawa","Krakow","Wroclaw","Lodz","Poznan"),
                                  selected = "Warszawa")),
      menuItem(radioButtons("miasto2",
                                  "Wybierz drugie\nmiasto do porównania:",
                                  choiceNames = c("Warszawa","Kraków","Wrocław","Łódź","Poznań"),
                                  choiceValues = c("Warszawa","Krakow","Wroclaw","Lodz","Poznan"),
                                  selected = "Krakow"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem("dashboard",
              plotOutput("boxplot"),
              plotOutput("linegraph"),
              plotOutput("comparison")
      ),
      tabItem("about",
              "Ta aplikacja została stworzona w ramach 7 pracy domowej przedmiotu Techniki Wizualizacji Danych na Politechnice Warszawskiej."
      )
    )
  )
)

server <- function(input, output, session) {
  
  chemical <- reactive({
    if(input$chemical == "PM 2.5") {
      tmp25
    } else {
      tmp10
    }
  })
  
  trend <- reactive({
    if(input$trend) {
      TRUE
    } else{
      FALSE
    }
  })
  
  miasta <- reactive({
    chemical()[chemical()$variable %in% input$miasto,]
  })
  
  porownanie <- reactive({
    
    miasto1 <- table(temp10[[input$miasto1]])
    miasto2 <- table(temp10[[input$miasto2]])
    tmp <- rbind(as.data.frame(miasto1),as.data.frame(miasto2))
    tmp$Miasto <- c(rep(input$miasto1,nrow(miasto1)), rep(input$miasto2,nrow(miasto2)))
    p <- ggplot(data = tmp, aes(x = Var1, y = Freq,fill = Miasto)) + 
      #geom_col(position = "dodge", width = 0.7) +
      geom_bar(stat="identity", position = "dodge",width = 0.7) +
      theme_light() +
      scale_x_discrete("Poziom", labels = c("1" = "dopuszczalny","2" = "informowania","3" = "alarmowy")) +
      labs(y="Liczba dni w roku") +
      ggtitle("Porównanie poziomu zanieczyszczenia PM10 w dwóch miastach", subtitle = "Ile dni w roku przekraczanie są normy?")
    p
  })
  
  output[["boxplot"]] <- renderPlot(
    
    ggplot(miasta(), aes(x = variable , y = value)) +
      geom_boxplot(fill='#A4A4A4')+
      theme_light() +
      xlab("Wybrane miasta") +
      ylab("Stężenie zanieczyszczenia w mikrogramach na metr sześcienny")+
      ggtitle("Stężenie zanieczyszczenia w wybranych miastach w 2017 roku")
  )
  
  output[["linegraph"]] <- renderPlot(
    if(trend()){
      ggplot(miasta(), aes(x = Dzien , y = value, color = variable)) +
        geom_line()+
        theme_light() +
        xlab("Data") +
        ylab("Stężenie zanieczyszczenia w mikrogramach na metr sześcienny")+
        ggtitle("Zmiany stężenia zanieczyszczenia w wybranych miastach w czasie 2017 roku") +
        labs(color = "Wybrane\nmiasta") +
        geom_smooth()
    } else{
    ggplot(miasta(), aes(x = Dzien , y = value, color = variable)) +
      geom_line()+
      theme_light() +
      xlab("Data") +
      ylab("Stężenie zanieczyszczenia w mikrogramach na metr sześcienny")+
      ggtitle("Zmiany stężenia zanieczyszczenia w wybranych miastach w czasie 2017 roku") +
      labs(color = "Wybrane\nmiasta")
    }
  )
  
  output[["comparison"]] <- renderPlot({
    porownanie()
    
 } )
}

shinyApp(ui, server)