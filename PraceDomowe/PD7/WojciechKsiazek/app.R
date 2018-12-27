library(shiny)
library(shinydashboard)
library(leaflet)
library(readr)
library(dplyr)
library(ggplot2)

metadane <- read_csv("metadane.csv")
pomiary <- read_csv("pomiary.csv")

miasta <- metadane %>% pull(miejscowosc) %>% unique %>% sort()

dates_range <- range(pomiary$data)

ui <- dashboardPage(
  skin = "black",
  dashboardHeader(title = "Air Quality"),
  dashboardSidebar(
    selectInput("city_1", 
                label = "First city", 
                choices = miasta, 
                selected = "Warszawa"),
    
    selectInput("city_2", 
                label = "Second city", 
                choices = miasta,
                selected = "Kraków"),
    
    dateRangeInput("dates", 
                   label = "Choose daterange:",
                   start = dates_range[1], 
                   end = dates_range[2],
                   min = dates_range[1], 
                   max = dates_range[2])
    
  ),
  dashboardBody(
    
    infoBoxOutput("infobox_city_1"),
             infoBoxOutput("infobox_city_2"),
    
    fluidRow(box(leafletOutput("map_city_1")),
             box(leafletOutput("map_city_2"))),
    
    fluidRow(box(plotOutput("plot_city_1")),
             box(plotOutput("plot_city_2")))
    


  )
)

server <- function(input, output) {
  metadane_city_1 <- reactive({metadane %>% filter(miejscowosc == input[["city_1"]])})
  metadane_city_2 <- reactive({metadane %>% filter(miejscowosc == input[["city_2"]])})
  
  pomiary_city_1 <- reactive({
    
    pomiary %>% filter(miejscowosc == input[["city_1"]],
                                                 between(data, as.POSIXct(input[["dates"]][1]), as.POSIXct(input[["dates"]][2])))
    })
  pomiary_city_2 <- reactive({pomiary %>% filter(miejscowosc == input[["city_2"]], 
                                                 between(data, as.POSIXct(input[["dates"]][1]), as.POSIXct(input[["dates"]][2])))
    })
  
  output[["infobox_city_1"]] <- renderInfoBox({
    
    days_above <- 
      pomiary_city_1() %>% 
      group_by(kodStacji) %>% 
      summarise(days_above = sum(pomiar > 40, na.rm = T)) %>% 
      filter(days_above == max(days_above, na.rm = T)) %>% 
      pull(days_above)
      
    infoBox(title = input[["city_1"]], 
            subtitle = paste("Days above PM10 norm"),
            value = days_above,
            icon = icon("cloud"), width = 8)
  })
  
  output[["infobox_city_2"]] <- renderInfoBox({
    
    days_above <- 
      pomiary_city_2() %>%       
      group_by(kodStacji) %>% 
      summarise(days_above = sum(pomiar > 40, na.rm = T)) %>% 
      filter(days_above == max(days_above, na.rm = T)) %>% 
      pull(days_above)
    
    infoBox(title = input[["city_2"]], 
            subtitle = paste("Days above PM10 norm"), 
            value = days_above,
            icon = icon("cloud",lib = "font-awesome"), width = 12)
  })
  
  output[["map_city_1"]] <- renderLeaflet({
    # browser()
    df <- 
    pomiary_city_1() %>% 
    group_by(kodStacji, nazwa) %>% 
      summarise(pomiar = mean(pomiar, na.rm = T),
                lat = median(lat),
                lng = median(lng)) %>% 
      mutate(grupa = case_when(pomiar <= 40 ~ "green",
                               pomiar <= 80 ~ "orange",
                               pomiar <= 120 ~ "red",
                               T ~ "purple"),
             label = paste("<p><b>", nazwa, "</b></p>", 
                           "<p>Mean daily PM10:", round(pomiar, 1), "</p>"))
    
    icons <- awesomeIcons(icon = 'ios-close',
                          iconColor = 'black',
                          library = 'ion',
                          markerColor = df$grupa)

     
    leaflet(data = df) %>%
      addTiles() %>% 
      addAwesomeMarkers(icon = icons, label = ~nazwa, popup =  ~label)
  })
  
  output[["map_city_2"]] <- renderLeaflet({
    df <- 
      pomiary_city_2() %>% 
      group_by(kodStacji, nazwa) %>% 
      summarise(pomiar = mean(pomiar, na.rm = T),
                lat = median(lat),
                lng = median(lng)) %>% 
      mutate(grupa = case_when(pomiar <= 40 ~ "green",
                               pomiar <= 80 ~ "orange",
                               pomiar <= 120 ~ "red",
                               T ~ "purple"),
             label = paste("<p><b>", nazwa, "</b></p>", 
                           "<p>Mean daily PM10:", round(pomiar, 1), "</p>"))
    
    icons <- awesomeIcons(icon = 'ios-close',
                          iconColor = 'black',
                          library = 'ion',
                          markerColor = df$grupa)
    
    
    leaflet(data = df) %>%
      addTiles() %>% 
      addAwesomeMarkers(icon = icons, label = ~nazwa, popup = ~label)
  })
  
  output[["plot_city_1"]] <- renderPlot({
    pomiary_city_1() %>% 
      ggplot(aes(data, pomiar)) + geom_line() + facet_grid(nazwa~.) + labs(y = "PM10")
  })
  
  output[["plot_city_2"]] <- renderPlot({
    pomiary_city_2() %>% 
      ggplot(aes(data, pomiar)) + geom_line() + facet_grid(nazwa~.) + labs(y = "PM10")
  })
  
}

shinyApp(ui, server)