library(dplyr)
library(reshape)
library(ggplot2)
library(lubridate)
library(shiny)
library(shinydashboard)

library(rsconnect)



kody <- read.csv2('Kopia Kody_stacji_pomiarowych.csv', sep=',', encoding = 'UTF-8')

pm25 <- read.csv2('2017_PM25_1g.csv', sep=',')
pm10 <- read.csv2('2017_PM10_1g.csv', sep=',')
o3  <- read.csv2('2017_O3_1g.csv', sep=',')

o3_melted <- o3 %>% melt('Kod.stacji')
pm25_melted <-  pm25 %>% melt('Kod.stacji')
pm10_melted <- pm10 %>% melt('Kod.stacji')

colnames(o3_melted) <- c('Date', 'station_code', 'value')
colnames(pm25_melted) <- c('Date', 'station_code', 'value')
colnames(pm10_melted) <- c('Date', 'station_code', 'value')

o3_melted <-  o3_melted %>% left_join(kody %>% select('MIEJSCOWOSC', 'KOD.NOWY'), by = c('station_code' = 'KOD.NOWY')) %>%
  filter(!is.na(MIEJSCOWOSC )) %>% filter(!is.na(value))

pm25_melted <-  pm25_melted %>% left_join(kody %>% select('MIEJSCOWOSC', 'KOD.NOWY'), by = c('station_code' = 'KOD.NOWY')) %>%
  filter(!is.na(MIEJSCOWOSC )) %>% filter(!is.na(value))

pm10_melted <-  pm10_melted %>% left_join(kody %>% select('MIEJSCOWOSC', 'KOD.NOWY'), by = c('station_code' = 'KOD.NOWY')) %>%
  filter(!is.na(MIEJSCOWOSC )) %>% filter(!is.na(value))



stations <- o3_melted$station_code %>% unique()

cities <- o3_melted$MIEJSCOWOSC %>% unique() %>% sort()


sample <- pm10_melted %>% filter(station_code == 'MzWarAlNiepo')

sample$Date <-  sample$Date %>% as.Date()

sample <- sample %>% group_by(Date = floor_date(Date, 'day')) %>% summarise(value = max(value))

dates <- sample$Date %>%unique()

dates <- dates[dates < '2018-01-01']


ui <- dashboardPage(
  skin = "black",
  dashboardHeader(title = "Zanieczyszczenie powietrza w Kujawsko-Pomorskim"),
  dashboardSidebar(
    sidebarUserPanel(Sys.info()[["effective_user"]],
                     subtitle = a(href = "#", icon("circle", class = "text-success"), "Online")
    ),
    sidebarMenu(
      # Setting id makes input$tabs give the tabName of currently-selected tab
      id = "tabs",
      menuItem("Wykresy", tabName = "plots", icon = icon("heart")),
      menuItem("O aplikacji", icon = icon("info-circle"), tabName = "about"),
      dateRangeInput(inputId = 'Date_input', label = " Wybierz zakres dat.", min = min(dates), max = max(dates), 
                     separator = " do ", start = '2017-01-01', end = '2017-12-31'),
      selectInput(inputId = "select_city", label = "Select city", 
                  choices = list('Gdańsk'
                                 ,'Warszawa'
                                 ,'Kraków'
                                 ,'Łódź'
                                 ,'Toruń'
                                 ,'Szczecin'
                                 ,'Poznań'), 
                  selected = 'Gdańsk'),
      checkboxGroupInput("checkGroup", label = "Wybierz wskaźnik", 
                         choices = list("PM10" = 'pm10', 
                                        "PM2.5" = 'pm25', 
                                        "O3" = 'o3'),
                         selected = 'pm10')
    )
  ),
  dashboardBody(
    tabItems(
      tabItem("plots",
              plotOutput("p1"),
              plotOutput('p3'),
              plotOutput("p2")
              
      ),
      tabItem("about",
              "Na tym dashboardzie możemy zobaczyć zanieczyszczenie powietrza w Polsce w 2017 roku ze względu na wskaźniki pm10, pm2.5 oraz O3
              Jako poziom przekroczenia przyjąłęm średnią dobową 50, 25 oraz 70 odpowiednio dla pm10, pm2.5 oraz o3. 
              Przeglądając dane mozemy zauważyć, że najbardziej zanieczyszczonym miastem jest Kraków z największą liczbą przekroczeń norm zanieczyszczenia powietrza.
              "
      )
    )
  )
)

server <- function(input, output) {
  
  
  
  output[["p1"]] <- renderPlot(({
    
    
    pm10_agg <- pm10_melted %>% filter(MIEJSCOWOSC == input$select_city)
    pm10_agg$Date <-  pm10_agg$Date %>% as.Date()
    pm10_agg <- pm10_agg %>% group_by(Date = floor_date(Date, 'day')) %>% summarise(value = mean(value))
    
    pm25_agg <- pm25_melted %>% filter(MIEJSCOWOSC == input$select_city)
    pm25_agg$Date <-  pm25_agg$Date %>% as.Date()
    pm25_agg <- pm25_agg %>% group_by(Date = floor_date(Date, 'day')) %>% summarise(value = mean(value))
    
    o3_agg <- o3_melted %>% filter(MIEJSCOWOSC == input$select_city)
    o3_agg$Date <-  o3_agg$Date %>% as.Date()
    o3_agg <- o3_agg %>% group_by(Date = floor_date(Date, 'day')) %>% summarise(value = mean(value))
    
    pm10_agg <- pm10_agg %>% filter(Date >= input$Date_input[1] & Date <= input$Date_input[2])
    pm25_agg <- pm25_agg %>% filter(Date >= input$Date_input[1] & Date <= input$Date_input[2])
    o3_agg <- o3_agg %>% filter(Date >= input$Date_input[1] & Date <= input$Date_input[2])
    
    
    newData <- melt(list(pm10 = pm10_agg, pm25 = pm25_agg, o3 = o3_agg), id.vars = "Date") %>% 
      filter(L1 %in% input$checkGroup)
    
    newData <- rename(newData, c('L1'='Wskaźnik'))
    
    
    ggplot(newData ,aes(x = Date, y = value,  colour = Wskaźnik))+
      geom_line(size = 0.5) + ylab('Średniodobowa wartość czynnika [µg/m3]') + xlab('Data') +
      scale_fill_discrete(name = "New Legend Title") +
      ggtitle("Średniodobowe zanieczyszczenie powietrza") 

    
  }))
  
  output[["p2"]] <- renderPlot(({
    
    pm10_agg <- pm10_melted %>% filter(MIEJSCOWOSC == input$select_city)
    pm10_agg$Date <-  pm10_agg$Date %>% as.Date()
    pm10_agg <- pm10_agg %>% group_by(Date = floor_date(Date, 'day')) %>% summarise(value = mean(value))
    pm10_agg <- pm10_agg %>% group_by(moth = floor_date(Date, 'month')) %>% summarise(over = sum(ifelse(value > 50, 1,0)))
    
    
    pm25_agg <- pm25_melted %>% filter(MIEJSCOWOSC == input$select_city)
    pm25_agg$Date <-  pm25_agg$Date %>% as.Date()
    pm25_agg <- pm25_agg %>% group_by(Date = floor_date(Date, 'day')) %>% summarise(value = mean(value))
    pm25_agg <- pm25_agg %>% group_by(moth = floor_date(Date, 'month')) %>% summarise(over = sum(ifelse(value > 25, 1,0)))
    
    o3_agg <- o3_melted %>% filter(MIEJSCOWOSC == input$select_city)
    o3_agg$Date <-  o3_agg$Date %>% as.Date()
    o3_agg <- o3_agg %>% group_by(Date = floor_date(Date, 'day')) %>% summarise(value = mean(value))
    o3_agg <- o3_agg %>% group_by(moth = floor_date(Date, 'month')) %>% summarise(over = sum(ifelse(value > 70, 1,0)))

    
    pm10_agg <- pm10_agg %>% filter(moth <= '2017-12-31')
    pm25_agg <- pm25_agg %>% filter(moth <= '2017-12-31')
    o3_agg <- o3_agg %>% filter(moth <= '2017-12-31')

    
    pm10_agg$moth <- c("Styczeń","Luty","Marzec","Kwiecień","Maj",
                       "Czerwiec","Lipiec", "Sierpień", 'Wrzesień',
                       "Październik", 'Listopad', 'Grudzień')
    
    pm25_agg$moth <- c("Styczeń","Luty","Marzec","Kwiecień","Maj",
                       "Czerwiec","Lipiec", "Sierpień", 'Wrzesień',
                       "Październik", 'Listopad', 'Grudzień')
    
    o3_agg$moth <- c("Styczeń","Luty","Marzec","Kwiecień","Maj",
                     "Czerwiec","Lipiec", "Sierpień", 'Wrzesień',
                     "Październik", 'Listopad', 'Grudzień')
    
    
    newData <- melt(list(pm10 = pm10_agg, pm25 = pm25_agg, o3 = o3_agg), id.vars = "moth") %>% 
      filter(L1 %in% input$checkGroup)
    newData$moth <- factor(newData$moth, levels = newData$moth %>% unique())
    newData <- rename(newData, c('L1'='Wskaźnik'))
    ggplot(newData ,aes(x = (moth), y =  value,  fill = Wskaźnik))+
      geom_bar(stat='identity', position = "dodge") + 
      xlab("Miesiąc") + 
      ylab("Liczba przekroczeń normy w danym miesiącu.")+
      ggtitle("Liczba przekroczeń normy w danym miesiącu.")
  }))
  
  
  output[["p3"]] <- renderPlot(({
    
    pm10_agg <- pm10_melted %>% filter(MIEJSCOWOSC == input$select_city)
    pm10_agg$Date <-  pm10_agg$Date %>% as.Date()
    pm10_agg <- pm10_agg %>% group_by(Date = floor_date(Date, 'day')) %>% summarise(value = mean(value))
    
    
    pm25_agg <- pm25_melted %>% filter(MIEJSCOWOSC == input$select_city)
    pm25_agg$Date <-  pm25_agg$Date %>% as.Date()
    pm25_agg <- pm25_agg %>% group_by(Date = floor_date(Date, 'day')) %>% summarise(value = mean(value))
    
    o3_agg <- o3_melted %>% filter(MIEJSCOWOSC == input$select_city)
    o3_agg$Date <-  o3_agg$Date %>% as.Date()
    o3_agg <- o3_agg %>% group_by(Date = floor_date(Date, 'day')) %>% summarise(value = mean(value))
    
    pm10_agg <- pm10_agg %>% filter(Date >= input$Date_input[1] & Date <= input$Date_input[2])
    pm25_agg <- pm25_agg %>% filter(Date >= input$Date_input[1] & Date <= input$Date_input[2])
    o3_agg <- o3_agg %>% filter(Date >= input$Date_input[1] & Date <= input$Date_input[2])
    
    
    newData <- melt(list(pm10 = pm10_agg, pm25 = pm25_agg, o3 = o3_agg), id.vars = "Date") %>% 
      filter(L1 %in% input$checkGroup)
    newData <- rename(newData, c('L1'='Wskaźnik'))
    ggplot(newData ,aes(x = Date, y = value,  colour = Wskaźnik))+
      geom_point() + geom_smooth() + ylab('Średniodobowa wartość czynnika [µg/m3]') + xlab('Data') +
      ggtitle("Wykres rozproszenia zanieczyszczenia powietrza.")
    
  }))
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)