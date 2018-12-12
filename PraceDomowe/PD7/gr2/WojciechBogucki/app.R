#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(shinydashboard)
library(data.table)
load("PM10.RData")
load("PM25.RData")
# Define UI for application that draws a histogram

ui <- dashboardPage(
  skin = "green",
  dashboardHeader(title = "Zanieczyszczenie \npowietrza"),
  dashboardSidebar(
    sidebarUserPanel(paste0("Witaj ",Sys.info()[["effective_user"]],"!"),
                     subtitle = icon("air-freshener", class = "text-success")
    ),
    sidebarMenu(
      id = "tabs",
      menuItem("Dashboard", tabName = "main", icon = icon("tachometer-alt")),
      menuItem("About", icon = icon("info-circle"), tabName = "about", badgeLabel = "new",
               badgeColor = "green"),
      selectInput("PM_typ", "Wybierz rodzaj zanieczyszczenia:",
                  choices = c("PM 2.5", "PM 10")),
      uiOutput("stacje"),
      sliderInput("minimum", "Filtruj wyniki od:",
                  min=0, max=150, value = 0)
    )
  ),
  dashboardBody(
    tabItems(
      tabItem("main",
              plotOutput("main_plot",width = "100%"),
              fluidRow(column(width=6,plotOutput("warszawa_plot")),
                      column(6,plotOutput("krakow_plot")))
              
      ),
      tabItem("about",
              "Made with passion by Wojciech Bogucki
              \nTechniki Wizualizacji Danych Z2018/2019"
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  output[["stacje"]] <- renderUI({
    if(input[["PM_typ"]]=="PM 10"){
    checkboxGroupInput("stacje","Wybierz stacje:", 
                       choices = unique(PM10$nazwa),
                       selected = unique(PM10$nazwa))
    }else{
      checkboxGroupInput("stacje","Wybierz stacje:", 
                         choices = unique(PM25$nazwa),
                         selected = unique(PM25$nazwa))
      }
  })
  
  PM10_r <- reactive(PM10[nazwa %in% input[["stacje"]] & value>input[["minimum"]],])
  PM25_r <- reactive(PM25[nazwa %in% input[["stacje"]] & value>input[["minimum"]],])
  output[["main_plot"]] <- renderPlot({
    if(input[["PM_typ"]]=="PM 2.5") data=PM25_r()
    else data=PM10_r()
    ggplot(na.omit(data),aes(x=`Kod stacji`, y=value, color=nazwa)) + 
      geom_point()+
      ylab("Stężenie pyłów w ug/m3") +
      xlab("Data pomiaru") + 
      labs(title="Zanieczyszczenie powietrza")+
      scale_y_continuous(limits = c(0,200))
  })
  PM10_monthly <- PM10[,.(PM10 = mean(value,na.rm = TRUE)), by=.(month(`Kod stacji`),miasto)]
  PM25_monthly <- PM25[,.(PM25 = mean(value,na.rm = TRUE)), by=.(month(`Kod stacji`),miasto)]
  PM <- melt(cbind(PM10_monthly, PM25=PM25_monthly$PM25),1:2, variable.name = "Zanieczyszczenie")
  output[["warszawa_plot"]] <- renderPlot({
    
    ggplot(PM[miasto=="Warszawa"], aes(x=month, y=value,fill=Zanieczyszczenie)) + 
      geom_col(position = "dodge")+ 
      ylab("Stężenie pyłów w ug/m3") +
      xlab("Miesiąc pomiaru") +
      scale_x_discrete(limits=seq(1,12)) +
      labs(title="Warszawa - średnie zanieczyszczenie w 2017")+
      scale_fill_discrete(labels = c("PM 10", "PM 2.5"))+
      scale_y_continuous(limits = c(0,150))
  })
  
  output[["krakow_plot"]] <- renderPlot({
    
    ggplot(PM[miasto=="Kraków"], aes(x=month, y=value, fill=Zanieczyszczenie)) + 
      geom_col(position = "dodge") +
      ylab("Stężenie pyłów w ug/m3") +
      xlab("Miesiąc pomiaru") +
      scale_x_discrete(limits=seq(1,12)) +
      labs(title="Kraków - średnie zanieczyszczenie w 2017")+
      scale_fill_discrete(labels = c("PM 10", "PM 2.5")) +
      scale_y_continuous(limits = c(0,150))
  })
   
}

# Run the application 
shinyApp(ui = ui, server = server)

