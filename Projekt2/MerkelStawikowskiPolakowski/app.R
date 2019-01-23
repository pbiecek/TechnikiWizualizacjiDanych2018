library(shiny)
library(shinydashboard)
library(reshape2)
library(dplyr)
library(ggplot2)
library(ggExtra)
library(jsonlite)
library(data.table)

### Time online - dashboard2
### Data preprocessing
t <- data.table(read.csv("dane_z_telefonu.csv", sep = ";"))
t$Logging = paste(t$Who, t$Kiedy)

### Distance covered - dashboard3
### Data processing
dane <- read.csv2("podroze.csv", sep = ";")
colnames(dane) <- c("Osoba", "Data", "Dystans", "Wartosc")
dane$Osoba <- as.factor(dane$Osoba)
dane$Dystans <- as.numeric(dane$Dystans)

ui <- dashboardPage(
  skin = "black",
  dashboardHeader(title = "Project about us"),
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("Time spent online", tabName = "dashboard2", icon = icon("dashboard")),
      menuItem("Keyboard Typing Stats", tabName = "dashboard1", icon = icon("dashboard")),
      menuItem("Distance covered", tabName = "dashboard3", icon = icon("dashboard")),
      menuItem("About our app", icon = icon("info-circle"), tabName = "about")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem("dashboard1",
              div("Touch typing is the ability to use muscle memory to find keys fast, without using the sense of sight,
                  and with all the available fingers, just like piano players do. In order to improve our typing skills Mateusz and Michal started
                  course on keybr.com, these charts show progress in typing speed and controlling of making mistakes. ",
                  style = "font-family:Arial;font-size:20px"),
              hr(),
              div(
                sidebarLayout(
                  mainPanel(imageOutput("speed"), width = 12),
                  mainPanel(imageOutput("errors"), width = 12)
                ), align = "center" 
              )
      ),
      tabItem("dashboard2",
              div("Due to overwhelming Internet presence firstly we decided to control our time being online during a day.
                  Assuming that after 'logging on' we are online until we 'log off' we wanted to see whether something interesting hides behind
                  this data.",
                  style = "font-family:Arial;font-size:20px"),
              div("Indeed, it's not questionable to predict where weekends are or where Christmas time is. Of course, no one has a problem to
                  figure out where New Year's Eve is situated on graph!",
                  style = "font-family:Arial;font-size:20px"),
              hr(),
              mainPanel(plotOutput("time_online"),
                        width = 12)
      ),
      tabItem("dashboard3",
              div("Traveling is an everyday activity, that all of us do. Some people travel more than others or maybe on some specific
                  day we travel more than we usuallly do. We were collecting data with help from google maps. From this data we tried
                  to vizualize how much certain people (Mateusz or Witold) travel. On the graph we can see such
                  days as: returning home for christmas or returnig to Warsaw from home or even New Year's Eve.",
                  style = "font-family:Arial;font-size:20px"),
              hr(),
              div(
                sidebarLayout(
                  mainPanel(imageOutput("witold"), width = 12),
                  mainPanel(imageOutput("mateusz"), width = 12)
                ), align = "center" 
              )
      ),
      tabItem("about",
              div("Our app has been created for completing 2nd project of Data Visualisation Course at MiNI PW faculty.",
                  style = "font-family:Arial;font-size:20px"),
              div("We do show some measures about our everyday life before, during and after Christmas break 2018.",
                  style = "font-family:Arial;font-size:20px"),
              div("Enjoy!",
                  style = "font-family:Arial;font-size:25px"),
              hr(),
              div("\n\n\nAuthors: Mateusz Polakowski, Michał Stawikowski, Witold Merkel",
                  style = "font-family:Arial;font-size:10px")
      )
    )))


server <- function(input, output) {
  
  ### Time online - dashboard2
  time_online <- reactive({
    t[Kto %in% input$ktoLogowanie]
  })
  
  output$time_online = renderPlot({
    ggplot(data = t, aes(x = Kolejnosc, y = Czas, color = Logging)) +
      geom_hline(yintercept = 1440, size = 1.1, color = "gray") +
      geom_label(aes(3, 1350), label = "Midnight", color = "gray") +
      geom_vline(xintercept = 16, size = 1.1, color = "#d42426") +
      geom_vline(xintercept = 28, size = 1.1, color = "#d42426") +
      geom_point(size = 5) + geom_line(aes(linetype = Who), size = 1.5) +
      geom_label(aes(17.6, 1000), label = "Start of Christmas break", color = "#d42426") +
      geom_label(aes(26.6, 1000), label = "End of Christmas break", color = "#d42426") +
      scale_y_continuous(breaks = seq(0, 60*30, 60), labels = paste(c(paste("0", 0:9, sep = ""), 10:23, paste("0", 0:6, sep = "")), ":00", sep = "")) +
      scale_x_continuous(breaks = 1:33, labels = unique(t$Data), expand = c(0.001, 0.001)) +
      scale_color_brewer(type = "qual", palette = "Set1") +
      theme_bw() + 
      theme(axis.text.x = element_text(angle = 75, hjust = 1),
            axis.title = element_text(size = 17),
            plot.title = element_text(size = 25, face = "bold")) +
      labs(title = "Our time online", y = "Daytime", x = "Date")
  })
  
  
 
  ### Speed typing - dashboard1
  output[["speed"]] <- renderImage({
    # A temp file to save the output.
    # This file will be removed later by renderImage
    outfile <- tempfile(fileext='.gif')
    
    # Return a list containing the filename
    list(src = "filenamehere1.gif",
         contentType = 'image/gif'
         # width = 400
         # height = 300,
         # alt = "This is alternate text"
    )}, deleteFile = FALSE)
  
  output[["errors"]] <- renderImage({
    # A temp file to save the output.
    # This file will be removed later by renderImage
    outfile <- tempfile(fileext='.gif')
    
    # Return a list containing the filename
    list(src = "filenamehere3.gif",
         contentType = 'image/gif'
         # width = 400
         # height = 400
         # alt = "This is alternate text"
    )}, deleteFile = FALSE)
  
  
  
  ### Distance covered - dashboard3
  output[["witold"]] <- renderImage({
    # A temp file to save the output.
    # This file will be removed later by renderImage
    outfile <- tempfile(fileext='.gif')
    
    # Return a list containing the filename
    list(src = "witold.gif",
         contentType = 'image/gif'
         # width = 400
         # height = 400
         # alt = "This is alternate text"
    )}, deleteFile = FALSE)
  
  
  output[["mateusz"]] <- renderImage({
    # A temp file to save the output.
    # This file will be removed later by renderImage
    outfile <- tempfile(fileext='.gif')
    
    # Return a list containing the filename
    list(src = "mateusz.gif",
         contentType = 'image/gif'
         # width = 400
         # height = 400
         # alt = "This is alternate text"
    )}, deleteFile = FALSE)
}

shinyApp(ui, server)
