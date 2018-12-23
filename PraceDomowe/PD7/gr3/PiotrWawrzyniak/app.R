library(shiny)
library(ggplot2)
library(shiny)
#install.packages("shinydashboard")

library(shinydashboard)
library(leaflet)
#install.packages("geojsonio")
library(geojsonio)
library(plyr)
library(dplyr)
library(reshape)

poland_map <- geojsonio::geojson_read("json/poland_woj.geojson",what = "sp")
pal <- colorNumeric("viridis", NULL)

data <- read.csv("Statystyki_2000-2018.csv", header = TRUE, sep=";")
data_pm2 <- read.csv("Statystyki_2000-2018_PM2.csv", header = TRUE, sep=";")
data_pm10 <- read.csv("Statystyki_2000-2018_PM10.csv", header = TRUE, sep=";")

map <- data %>%
  group_by(Województwo) %>%
  summarise(srednia = mean(Średnia, na.rm = TRUE))

source <- data %>%
  group_by(Rok) %>%
  summarise(srednia = mean(Średnia, na.rm = TRUE))

source_PM2 <- data_pm2 %>%
  group_by(Rok) %>%
  summarise(srednia = mean(Średnia, na.rm = TRUE))

source_PM10 <- data_pm10 %>%
  group_by(Rok) %>%
  summarise(srednia = mean(Średnia, na.rm = TRUE))

source <- source[c(-1,-2),]
source_PM10 <- source_PM10[c(-1,-2),]
source$sredniaPM2 <- source_PM2$srednia
source$sredniaPM10 <- source_PM10$srednia

max_source <- data_pm10 %>%
  group_by(Nazwa.strefy) %>%
  summarise(max = max(Średnia, na.rm = TRUE)) %>%
  arrange(-max)

max_source$Nazwa.strefy <- factor(max_source$Nazwa.strefy, levels = max_source$Nazwa.strefy[order(max_source$max)])

ui <- dashboardPage(
  dashboardHeader(title="Zanieczyszczenie powietrza"),
  dashboardSidebar(sidebarMenu(
    menuItem("O projekcie", tabName = "wstep"),
    menuItem("Na przestrzeni lat", tabName = "lata", icon = icon("dashboard")),
    menuItem("Mapa zanieczyszczeń polski", tabName = "map", icon = icon("th")),
    menuItem("Maksymalne zanieczyszczenia", tabName = "maksymalne", icon = icon("th"))
    )),
  dashboardBody(tabItems
  (
    #Wstęp
    tabItem(tabName = "wstep",
            h2("O projekcie:"),
            p("Projekt ten ma zapewnić możliwość podstawowej analizy danych o zanieczyszczeniach w Polsce."),
            p("Możesz dowiedzieć się:"),
            tags$ul(
              tags$li("Jak kształtuje się poziom zanieczyszczeń na przestrzeni lat."), 
              tags$li("Które województwa są najbardziej a które najmniej zanieczyszczone."), 
              tags$li("Jakich obszarów trzeba szczególnie unikać jeśli zależy nam na czystym powietrzu.")
            )
            
    ),
    #Mapa
    tabItem(tabName = "map",
            fluidRow(
                leaflet(poland_map) %>%
                  addTiles() %>%
                  addPolygons(stroke = FALSE, smoothFactor = 0.3, fillOpacity = 1,
                              fillColor = ~pal(log10(map$srednia)),
                              label = ~paste0(map$Województwo, ": ", formatC(map$srednia, big.mark = ","))) %>%
                  addLegend(pal = pal, values = ~map$srednia, opacity = 1.0, title="Poziom SO2")
            )
    ),
    # Wykres
    tabItem(tabName = "lata",
      pageWithSidebar(
        headerPanel('Zanieczyszczenie powietrza na przestrzeni lat.'),
        sidebarPanel(
          selectInput('color', 'Motyw wygladu wykresu', c("Podstawowy", "Minimalistyczny", "Klasyczny")),
          checkboxGroupInput("levels", "Wybierz wskaźnik:", c("SO2","PM2.5","PM10"), selected = c("SO2","PM2.5","PM10"))
        ),
        mainPanel(
          plotOutput('plot1'),
          br(),
          br(),
          DT::dataTableOutput("table")
        )
      )),
    tabItem(tabName = "maksymalne",
            fluidPage(
              verticalLayout(
                titlePanel("Których stref unikać - maksymalne zanieczyszczenie"),
                plotOutput("plot2"),
                wellPanel(
                  sliderInput("imp", "Liczba stref:", min = 1, max = 46, value = 10))
                )
              )
            )
  ))
)

server <- function(input, output, session) {
  
  # Combine the selected variables into a new data frame
  selectedData <- reactive({
    columns <- revalue(input$levels, c("SO2"="srednia", "PM2.5"="sredniaPM2", "PM10"="sredniaPM10"))
    columns[[length(columns)+1]] = "Rok" 
    source[,columns]
  })
  
  output$table <- DT::renderDataTable(DT::datatable({
    selectedData()
  }))
  
  output$view <- renderTable({
    head(selectedData(), nrow(selectedData()))
  })
  
  output$plot2 <-renderPlot({
    max_data = max_source[0:input$imp,]
    plot = ggplot(max_data, 
           aes(x = Nazwa.strefy, y = max)) +
      #scale_y_continuous(limits = c(0, 150)) +
      geom_col(width=0.8) +
      labs(title="Maksymalne średnie zanieczyszczenie powietrza (MP10)", 
           subtitle="Badanie przeprowadzone wielokrotnie od 2002 roku gdzie czas pomiaru wynosił minimalnie godzinę.", 
           x="Kandydat", y="Wynik") + 
      coord_flip()
    
    plot
  })
  
  output$plot1 <- renderPlot({
    
    if(ncol(selectedData())>1){
      
      Molten <- melt(as.data.frame(selectedData()), id ="Rok")
      plot <- ggplot(data=Molten, aes(x=Rok, y=value, colour=variable))+
        geom_line(size=1)+
        scale_color_discrete(name = "Wskaźnik", labels = c("SO2", "PM2.3", "PM10")) +
        ylab("Zanieczyszczenie\n") +
        ggtitle("Zanieczyszczenie powietrza w Polsce na przestrzeni lat\n")+
        scale_x_continuous(breaks = c(2002:2017))
      
      if(input$color=="Podstawowy"){
        plot <- plot +
          theme(axis.title.x=element_blank(),
                axis.text.x=element_blank(),
                axis.ticks.x=element_blank(),
                axis.text.y=element_text(size=12),
                axis.title.y=element_text(size=12,face="bold"),
                plot.title = element_text(size=18,face="bold",hjust=0.5),
                strip.text.x = element_text(size = 12, colour = "black", angle = 0),
                legend.title = element_text(colour="black", size=12, face="bold"),
                panel.background = element_rect(fill = "grey97",
                                                colour = "grey90",
                                                size = 0.5, linetype = "solid"),
                panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                                colour = "white"), 
                panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                                colour = "white"))
      }
      
      if(input$color=="Minimalistyczny"){
        plot <- plot +
          theme_minimal() +
          theme(axis.title.x=element_blank(),
                axis.text.x=element_blank(),
                axis.ticks.x=element_blank(),
                axis.text.y=element_text(size=12),
                axis.title.y=element_text(size=12,face="bold"),
                plot.title = element_text(size=18,face="bold",hjust=0.5),
                strip.text.x = element_text(size = 12, angle = 0),
                legend.title = element_text(size=12, face="bold"))
      }
      
      if(input$color=="Klasyczny"){
        plot <- plot +
          theme_classic() +
          theme(axis.title.x=element_blank(),
                axis.text.x=element_blank(),
                axis.ticks.x=element_blank(),
                axis.text.y=element_text(size=12),
                axis.title.y=element_text(size=12,face="bold"),
                plot.title = element_text(size=18,face="bold",hjust=0.5),
                strip.text.x = element_text(size = 12, angle = 0),
                legend.title = element_text(size=12, face="bold"))
      }
      
      plot	
    }
  })
}

shinyApp(ui = ui, server = server)
#deployApp(appName="Projekt7")

