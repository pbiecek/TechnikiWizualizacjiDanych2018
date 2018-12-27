library(shiny)
library(shinydashboard)
library(reshape2)
library(openxlsx)
library(scales)
library(dplyr)
library(ggplot2)
library(ggmap)

metadata <- read.xlsx('Metadane_wer20180829.xlsx')

pm_10 <- read.xlsx('2017_PM10_24g.xlsx')
pm_25 <- read.xlsx('2017_PM25_24g.xlsx')

colnames(pm_10)[1] <- 'Date'
colnames(pm_25)[1] <- 'Date'

colnames(pm_10)[-1] <- pm_10[1,-1]
colnames(pm_25)[-1] <- pm_25[1,-1]

pm_10_columns <- colnames(pm_10)[-1]
pm_25_columns <- colnames(pm_25)[-1]

pm_10 <- pm_10[seq(6,dim(pm_10)[1]),]
pm_25 <- pm_25[seq(6,dim(pm_25)[1]),]

pm_10$Date <- convertToDate(pm_10$Date)
pm_25$Date <- convertToDate(pm_25$Date)

for (col in pm_10_columns) {
  pm_10[,col] <- as.numeric(sub(",", ".", pm_10[,col], fixed = TRUE))
}

for (col in pm_25_columns) {
  pm_25[,col] <- as.numeric(sub(",", ".", pm_25[,col], fixed = TRUE))
}

pm_10_melted <- melt(data = pm_10, id.vars = "Date", measure.vars = pm_10_columns)
pm_25_melted <- melt(data = pm_25, id.vars = "Date", measure.vars = pm_25_columns)

pm_10_stations <- metadata[metadata$"Kod.stacji" %in% intersect(pm_10_columns, metadata$Kod.stacji), "Nazwa.stacji"]
pm_25_stations <- metadata[metadata$"Kod.stacji" %in% intersect(pm_25_columns, metadata$Kod.stacji), "Nazwa.stacji"]

ui <- dashboardPage(
  skin = "black",
  dashboardHeader(title = "Jakość powietrza\n w Polsce"),
  dashboardSidebar(
    sidebarUserPanel(Sys.info()[["effective_user"]],
                     subtitle = a(href = "#", icon("circle", class = "text-success"), "Online")
    ),
    sidebarMenu(
      id = "tabs",
      menuItem("Wyniki pomiarów", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("O aplikacji", icon = icon("info-circle"), tabName = "about")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem("dashboard",
              fluidRow(
                box(selectInput("pollution_type", "Rodzaj zanieczyszczenia: ",
                                                                            choices = c("pm_10", "pm_25"),
                                                                            selected = "pm_10", width = 150)),
                box(valueBoxOutput("count"), valueBoxOutput("maximum"))
              ),
              fluidRow(
                box(title = "Stan na dzień: ",
                    dateInput("measurement_date", "Data pomiarów:", value = "2017-01-01", format = "yyyy-mm-dd"),
                    plotOutput("pollution_map", width = 800, height = 800)),
                
                box(title = "Pomiary roczne dla wybranej stacji: ",selectInput("selected_station", "Stacja pomiarowa",
                                                                                        choices = pm_10_stations,
                                                                                        selected = pm_10_stations[1]
                ),
                plotOutput("pollution_trend_plot")))
      ),
      tabItem("about",
              box(title="O aplikacji", p("Dashboard pokazuje wyniki pomiarów zanieczyszczeń wykonanych w roku 2017 dla dwóch rodzajów substancji PM10 i PM2,5. Wyniki prezentowane są w formie mapy dla wybranego dnia i całorocznego wykresu dla wybranej stacji pomiarowej."))
      )
    )
  )
)

render_map <- function(data_r, date, pollution_type, mid_point) {
  qmplot(lon, lat, data=data_r(), geom = c("point"), color=value ,size = I(5.5), extent = "device") +
    ggtitle(paste("Stężenie", pollution_type, "w dniu", date)) +
    scale_color_gradient2(midpoint=mid_point, low="green", mid="red", high="black", space ="Lab",
                          aesthetics = "colour" ) +
    theme(plot.title = element_text(size=16))
}

render_trend <- function(data, station, pollution_type, normal_level, info_level, alarm_level) {
  p <- ggplot(data, aes_string(x = "Date", y = metadata[metadata$Nazwa.stacji == station, 'Kod.stacji'])) + 
    geom_line(size = I(1.2)) +
    scale_color_discrete(guide = "none") + 
    scale_x_date(labels = date_format("%Y-%m-%d"), date_breaks = "months") + 
    theme(panel.grid = element_blank()) + theme_bw() + 
    ggtitle(paste("Pomiary stężenia", pollution_type, "dla stacji", station)) +
    xlab("Data pomiaru") +
    ylab(paste("Stężenie", pollution_type, "[ug/m3]")) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), plot.title = element_text(size=16))
  
    if (!is.na(normal_level)) {
      p <- p + geom_hline(yintercept=normal_level, size = 2, linetype="solid", color = "green") +
      annotate("text",y=normal_level,x=pm_10$Date[1], label="Poziom dopuszczalny", color = "gray", hjust=.1, vjust = -.5)
    }
    
    if (!is.na(info_level)) {
      p <- p + geom_hline(yintercept=info_level, size = 2, linetype="solid", color = "gold") +
      annotate("text",y=info_level,x=pm_10$Date[1], label="Poziom informowania", color = "gray", hjust=.1, vjust = -.5) 
    }
      
    if (!is.na(alarm_level)) {
      p <- p + 
      geom_hline(yintercept=alarm_level, size = 2, linetype="solid", color = "red") +
      annotate("text",y=alarm_level,x=pm_10$Date[1], label="Poziom alarmowy", color = "gray", hjust=.1, vjust = -.5)
    }
  p
}

server <- function(input, output, session) {
  
  trend_data <- reactive({if (input$pollution_type == "pm_10") pm_10 else pm_25})
  map_data_source <- reactive({if (input$pollution_type == "pm_10") pm_10_melted else pm_25_melted})
  pollution_type_name <- reactive({if (input$pollution_type == "pm_10") "PM10" else "PM2,5"})
  selected_station_code <- reactive(metadata[metadata$Nazwa.stacji == input$selected_station, 'Kod.stacji'])
  
  observe(
    {input$pollution_type
      # Update based on the year change event
      updateSelectInput(session, "selected_station", choices = if (input$pollution_type == "pm_10") pm_10_stations else pm_25_stations)
    })
  
  map_data <- reactive({
    map_data_source() %>%
    filter(Date == input$measurement_date) %>%
    inner_join(metadata, by = c("variable" = "Kod.stacji")) %>%
    rename("lon" = "WGS84.λ.E", "lat" = "WGS84.φ.N")
  })
  
  output$count <- renderValueBox({
    valueBox(
      value = sum(trend_data()[,selected_station_code()] > 50, na.rm = TRUE),
      subtitle = "Liczba przekroczeń",
      icon = icon("exclamation-triangle"),
      color = "yellow"
    )
  })
  
  output$maximum <- renderValueBox({
    valueBox(
      value = max(trend_data()[,selected_station_code()], na.rm = TRUE),
      subtitle = "Najgorszy odczyt",
      icon = icon("exclamation-circle"),
      color = "red"
    )
  })
  
  output[["pollution_map"]] <- renderPlot(
    render_map(map_data, input$measurement_date, pollution_type_name(), 200)
  )
  
  output[["pollution_trend_plot"]] <- renderPlot(
    render_trend(trend_data(), input$selected_station, pollution_type_name(), 50, 200, 300)
  )
  
}

shinyApp(ui, server)
