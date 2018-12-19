library(shiny)
library(reshape2)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(openxlsx)


dat25 <- read.csv("dat25.csv")
dat10 <- read.csv("dat10.csv")
dat10$Data <- as.Date(as.numeric(dat10$Data), origin = "2017-01-01")
dat25$Data <- as.Date(as.numeric(dat25$Data), origin = "2017-01-01")



ui <- dashboardPage(
  skin = "black",
  dashboardHeader(title = "Zanieszczenie powietrza w Polskich miastach",
                  
                  dropdownMenu(type = "notifications", badgeStatus = "warning",
                               notificationItem(icon = icon("exclamation-triangle"), status = "info",
                                                "Aplikacja wyświetla dane z 2017 roku"
                               )
                  )),
  dashboardSidebar(
    sidebarUserPanel(Sys.info()[["effective_user"]],
                     subtitle = a(href = "#", icon("circle", class = "text-success"), "Online")
    ),
    sidebarMenu(
      # Setting id makes input$tabs give the tabName of currently-selected tab
      id = "tabs",
      menuItem("PM10", tabName = "pm10", icon = icon("info-circle")),
      menuItem("PM2,5", tabName = "pm25", icon = icon("info-circle"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem("pm10",
              box(
                checkboxGroupInput(inputId = "wybraneMiasta", 
                                   label = "Wybierz miasta",
                                   choices = c("wroclaw","Lodz","Krakow","Warszawa","Gdansk"),
                                   selected = "Warszawa"),
                dateRangeInput(inputId = "zakresDat", label = "Wybierz interesujący okres czasu", 
                               start = "2017-01-01", end = "2017-12-31", 
                               min = "2017-01-01", max = "2017-12-31", format = "yyyy-mm-dd")
              ),
              
              fluidRow(box(plotOutput("n_matches_plot"), width = 12, title = "Wykres ilości PM10 w powietrzu",
                           background = "navy", solidHeader = TRUE))
              
              
              
      ),
      tabItem("pm25",
              box(
                checkboxGroupInput(inputId = "wybraneMiasta2", 
                                   label = "Wybierz miasta",
                                   choices = c("Wroclaw","Lodz","Krakow","Warszawa","Gdansk"),
                                   selected = "Warszawa"),
                dateRangeInput(inputId = "zakresDat2", label = "Wybierz interesujący okres czasu", 
                               start = "2017-01-01", end = "2017-12-31", 
                               min = "2017-01-01", max = "2017-12-31", format = "yyyy-mm-dd")
              ),
              
              fluidRow(box(plotOutput("10plot"), width = 12, title = "Wykres ilości PM25 w powietrzu",
                           background = "orange", solidHeader = TRUE))
              
              
              
      )
    )
  )
)

server <- function(input, output) {
  wykres10 <- reactive({
    dat10 %>% filter(Data > input$zakresDat[1]) %>% filter(Data < input$zakresDat[2])
    dat10 <- dat10[dat10$Data > input$zakresDat[1],]
    dat10 <- dat10[dat10$Data < input$zakresDat[2],]
    
    p <- ggplot(dat10)
    if("Warszawa" %in% input$wybraneMiasta){
      p <- p + geom_smooth(aes(x=Data,y=Warszawa, colour="Warszawa"))
        
    }
    if("Torun" %in% input$wybraneMiasta){
      p <- p + geom_smooth(aes(x=Data,y=Torun, colour="Torun"))
      
    }
    if("Krakow" %in% input$wybraneMiasta){
      p <- p + geom_smooth(aes(x=Data,y=Krakow, colour="Krakow"))
      
    }
    if("Gdansk" %in% input$wybraneMiasta){
      p <- p + geom_smooth(aes(x=Data,y=Gdansk, colour="Gdansk"))
      
    }
    if("Wroclaw" %in% input$wybraneMiasta){
      p <- p + geom_smooth(aes(x=Data,y=Wroclaw, colour="Wroclaw"))
      
    }
    if("Lodz" %in% input$wybraneMiasta){
      p <- p + geom_smooth(aes(x=Data,y=Lodz, colour="Lodz"))
      
    }
    p + theme_bw() 

    p
  })
  
  wykres25 <- reactive({
    dat25 %>% filter(Data > input$zakresDat2[1]) %>% filter(Data < input$zakresDat2[2])
    dat25 <- dat25[dat25$Data > input$zakresDat2[1],]
    dat25 <- dat25[dat25$Data < input$zakresDat2[2],]
    
    p <- ggplot(dat25)
    if("Warszawa" %in% input$wybraneMiasta2){
      p <- p + geom_smooth(aes(x=Data,y=Warszawa, colour="Warszawa"))
      
    }
    if("Torun" %in% input$wybraneMiasta2){
      p <- p + geom_smooth(aes(x=Data,y=Torun, colour="Torun"))
      
    }
    if("Krakow" %in% input$wybraneMiasta2){
      p <- p + geom_smooth(aes(x=Data,y=Krakow, colour="Krakow"))
      
    }
    if("Gdansk" %in% input$wybraneMiasta2){
      p <- p + geom_smooth(aes(x=Data,y=Gdansk, colour="Gdansk"))
      
    }
    if("Wroclaw" %in% input$wybraneMiasta2){
      p <- p + geom_smooth(aes(x=Data,y=Wroclaw, colour="Wroclaw"))
      
    }
    if("Lodz" %in% input$wybraneMiasta2){
      p <- p + geom_smooth(aes(x=Data,y=Lodz, colour="Lodz"))
      
    }
    p + theme_bw() 
    
    p
  })
      
  output[["n_matches_plot"]] <- renderPlot(
    wykres10() +       theme(
      legend.title = element_blank(),
      axis.title.y = element_blank() 
      
    )
  )

  output[["10plot"]] <- renderPlot(
    wykres25() +       theme(
      legend.title = element_blank(),
      axis.title.y = element_blank() 
      
    )
  )
}

shinyApp(ui, server)