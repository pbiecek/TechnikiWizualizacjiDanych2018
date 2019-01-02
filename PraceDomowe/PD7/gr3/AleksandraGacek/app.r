library(shiny)
library(shinydashboard)
library(leaflet)
library(htmltools)
library(ggplot2)
library(dplyr)

monthStart <- function(x) {
  x <- as.POSIXlt(x)
  x$mday <- 1
  as.Date(x)
}

pm10_mean <- read.csv("pm10_mean.csv")

pm10_mean$date <- c(seq(as.Date("2012/1/1"), as.Date("2017/12/1"), "months"))

pm10_max <- read.csv("pm10_max.csv")

pm10_max$date <- c(seq(as.Date("2012/1/1"), as.Date("2017/12/1"), "months"))


pm25_mean <- read.csv("pm25_mean.csv")

pm25_mean$date <- c(seq(as.Date("2012/1/1"), as.Date("2017/12/1"), "months"))

pm25_max <- read.csv("pm25_max.csv")

pm25_max$date <- c(seq(as.Date("2012/1/1"), as.Date("2017/12/1"), "months"))


cities_coord <- data.frame("cities"=c("Szczecin", "Gdansk", 'Olsztyn', 'Bialystok', 'Warszawa', 'Poznan',
                                      'Zielona Gora', 'Wroclaw', 'Opole', 'Lodz', 'Katowice', 'Kielce', 'Krakow', 'Lublin', 'Rzeszow',
                                      'Bydgoszcz', 'Torun'),
                           'lat' = c(53.438056, 54.3475, 53.773056, 53.135278, 52.232222, 52.408333, 51.939722,
                                     51.11, 50.664722, 51.776667, 50.264167, 50.874167, 50.061389, 51.248056, 50.033611, 53.125,
                                     53.022222),
                           'long' = c(14.542222, 18.645278, 20.476111, 23.145556, 21, 16.934167, 15.505, 17.022222,
                                      17.926944, 19.454722, 19.023611, 20.633333, 19.938333, 22.570278, 22, 18.011111, 18.611111))

get_color <- function(chosen_year_start, chosen_year_end, mean_or_max, pm10_or_25) {
  df_to_use <- if(mean_or_max=='max') {
    if(pm10_or_25 == 'PM10') pm10_max else pm25_max
  } else if(pm10_or_25 == 'PM10') pm10_mean else pm25_mean
  fun_to_use <- if(mean_or_max=='max') max else mean
  sapply(cities_coord$cities, function(city) {
    city <- as.character(city)
    mean_result <- fun_to_use(df_to_use[(df_to_use$Year>=chosen_year_start&df_to_use$Year<=chosen_year_end),][[city]], na.rm=TRUE)
    message <- paste("City: ", city, "<br/>", pm10_or_25, ": ", formatC(mean_result, format="f", digits=1), "[\u03BCg/m3]")
    if(is.na(mean_result)) c("gray", message)
    else {
      if(mean_result < 50) {
        c("green", message)
      } else {
          if(mean_result <= 200) {
            c("orange", message)
          }  else c("red", message)
        }
        
      }
    }
  )
}

get_color_monthly <- function(chosen_year_start, chosen_year_end, mean_or_max, pm10_or_25, month) {
  df_to_use <- if(mean_or_max=='max') {
    if(pm10_or_25 == 'PM10') pm10_max else pm25_max
  } else if(pm10_or_25 == 'PM10') pm10_mean else pm25_mean
  fun_to_use <- if(mean_or_max=='max') max else mean
  sapply(cities_coord$cities, function(city) {
    city <- as.character(city)
    mean_result <- fun_to_use(df_to_use[(df_to_use$Year>=chosen_year_start&df_to_use$Year<=chosen_year_end&df_to_use$Month==month),][[city]], na.rm=TRUE)
    message <- paste("City: ", city, "<br/>", pm10_or_25, ": ", formatC(mean_result, format="f", digits=1), "[\u03BCg/m3]")
    if(is.na(mean_result)) c("gray", message)
    else {
      if(mean_result < 50) {
        c("green", message)
      } else {
        if(mean_result <= 200) {
          c("orange", message)
        }  else c("red", message)
      }
      
    }
  }
  )
}

get_city_data <- function(chosen_date_start, chosen_date_end, mean_or_max, pm10_or_25) {
  df_to_use <- if(mean_or_max=='max') {
    if(pm10_or_25 == 'PM10') pm10_max else pm25_max
  } else if(pm10_or_25 == 'PM10') pm10_mean else pm25_mean
  fun_to_use <- if(mean_or_max=='max') max else mean
  sapply(cities_coord$cities, function(city) {
    city <- as.character(city)
    mean_result <- fun_to_use(df_to_use[(df_to_use$date>=chosen_date_start&df_to_use$date<=chosen_date_end),][[city]], na.rm=TRUE)
    message <- paste("City: ", city, "<br/>", pm10_or_25, ": ", formatC(mean_result, format="f", digits=1))
    if(is.na(mean_result)) c("gray", message)
    else {
      if(mean_result < 50) {
        c("green", message)
      } else {
        if(mean_result <= 200) {
          c("orange", message)
        }  else c("red", message)
      }
      
    }
  }
  )
}

ui <- dashboardPage(
  skin = "black",
  dashboardHeader(title = "Pollution App"),
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("About", icon = icon("info-circle"), tabName = "about", badgeLabel = "new",
               badgeColor = "green"),
      menuItem("Yearly pollution", tabName = "dashboard", icon = icon("calendar")),
      menuItem("Monthly pollution", tabName = "monthly", icon = icon("calendar")),
      menuItem("Check your city", tabName = "city_", icon = icon("city"))

    )
  ),
  dashboardBody(
    tabItems(
      tabItem("dashboard",
              fluidPage(
                radioButtons(inputId="chosen_pollution", label="Choose pollution type", choices = c("PM10", "PM2.5"), selected="PM10"),
                radioButtons(inputId="chosen_info", label="Choose pollution aggregation", choices = c("max", "mean"), selected="mean"),
                sliderInput(inputId="chosen_year", label="Choose years range", min = 2012, max = 2017, sep = "", value=c(2017, 2017)),
                leafletOutput("mymap",height="500px"),
                p()
      )),
      tabItem("monthly",
              fluidPage(
                selectInput(inputId="chosen_month", label="Choose month", choices=format(ISOdate(2004,1:12,1),"%B")),
                radioButtons(inputId="chosen_pollution2", label="Choose pollution type", choices = c("PM10", "PM2.5"), selected="PM10"),
                radioButtons(inputId="chosen_info2", label="Choose pollution aggregation", choices = c("max", "mean"), selected="mean"),
                sliderInput(inputId="chosen_year2", label="Choose years range", min = 2012, max = 2017, sep = "", value=c(2017, 2017)),
                leafletOutput("mymap2",height="500px"),
                p()
              )),
      tabItem("city_",
        fluidPage(
          selectInput(inputId="chosen_city", label="Choose your city", choices=cities_coord$cities),
          radioButtons(inputId="chosen_pollution3", label="Choose pollution type", choices = c("PM10", "PM2.5"), selected="PM10"),
          radioButtons(inputId="chosen_info_city", label="Choose pollution aggregation", choices = c("max", "mean"), selected="mean"),
          sliderInput("slider1", "Date Range",
                      min = as.Date("2012/01/01"),
                      max = as.Date("2017/12/31"),
                      timeFormat="%b %Y",
                      value = c(as.Date("2012/01/01"), as.Date("2017/12/31"))
          ),
          plotOutput("myplot")
          )
      ),
      tabItem("about",
              p(),
              "Here you can check historical data about air pollution in Poland's biggest cities in years 2012-2017.",
              br(),
              "You can see maximal and average pollution levels during that period, or how did monthly pollution level changed in different polish cities.",
              br(),
              "It is also possible to check pollution level through the years in your city."
      )
    )
  )
)

server <- function(input, output, session) {
  
  data_r <- reactive({
    get_color(input[["chosen_year"]][1] %% 2000, input[["chosen_year"]][2] %% 2000, input[["chosen_info"]], input[["chosen_pollution"]])
  })
  
  data2_r <- reactive({
    get_color_monthly(input[["chosen_year2"]][1] %% 2000, input[["chosen_year2"]][2] %% 2000, input[["chosen_info2"]],
                      input[["chosen_pollution2"]], which(format(ISOdate(2004,1:12,1),"%B")==input[["chosen_month"]]))
  })
  
  output$mymap <- renderLeaflet({
    
    data <- data_r()
    icons <- awesomeIcons(
      icon = 'ios-close',
      iconColor = 'black',
      library = 'ion',
      markerColor = data[1,]
    )
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron,
                       options = providerTileOptions(noWrap = TRUE)
      ) %>%
      addAwesomeMarkers(data = cities_coord[,c('lat', 'long')], icon = icons, popup =data[2,])
  })
  
  output$mymap2 <- renderLeaflet({
    
    data <- data2_r()
    icons <- awesomeIcons(
      icon = 'ios-close',
      iconColor = 'black',
      library = 'ion',
      markerColor = data[1,]
    )
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron,
                       options = providerTileOptions(noWrap = TRUE)
      ) %>%
      addAwesomeMarkers(data = cities_coord[,c('lat', 'long')], icon = icons, popup =data[2,])
  })
  
  city_data_r <- reactive({
    df_to_use <- if(input[["chosen_info_city"]] == 'max') {
      if(input[["chosen_pollution3"]] == 'PM10') pm10_max else pm25_max
    } else if(input[["chosen_pollution3"]] == 'PM10') pm10_mean else pm25_mean
    start_date <- monthStart(input[['slider1']][[1]])
    end_date <- monthStart(input[['slider1']][[2]])
    (df_to_use %>% filter(date >= start_date, date <= end_date))[[input[['chosen_city']]]]
  })
  
  output$myplot <- renderPlot({
    date_range <- c(seq(input[['slider1']][[1]], input[['slider1']][[2]], "months"))
    ggplot(mapping= aes(x=date_range, y=city_data_r())) +
      geom_line() +
      theme_minimal() +
      xlab("Date") +
      ylab(paste(input[["chosen_pollution3"]],"[\u03BCg/m3]")) +
      ggtitle(label=paste("Pollution in city ", input[['chosen_city']]))
  })
}

shinyApp(ui, server)

