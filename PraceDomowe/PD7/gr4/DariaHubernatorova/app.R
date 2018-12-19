library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(openxlsx)
library(reshape2)

data2017 <- read.xlsx("pm25_2017.xlsx")
colnames(data2017) <- data2017[1,]
data2017 <- data2017[-c(1, 2, 3, 4, 5),]
data1 <- select(data2017, "Kod stacji", "DsWrocNaGrob", "MzWarWokalna", "WpPoznPolank", 
                "KpBydBerling", "LbLubSliwins", "SlKatoKossut", "PmGdaPowWiel", 
                "MpKrakBujaka", "ZpSzczAndr01", "LdLodzLegion")
miasta <- c("Wroclaw", "Warszawa", "Poznan", "Bydgoszcz", 
            "Lublin", "Katowice", "Gdansk",
            "Krakow","Szczecin","Lodz")
colnames(data1) <- c("Data", "Wroclaw", "Warszawa", "Poznan", "Bydgoszcz", 
                     "Lublin", "Katowice", "Gdansk",
                     "Krakow","Szczecin","Lodz")
Data <- data1$Data
data1 <- as.data.frame(apply(
  data1[,-1],2, function(x) as.numeric(sub(",", ".", x, fixed = TRUE))))
data1$Data <- Data
data1$Data <- as.Date(as.numeric(data1$Data), origin = "1899-12-30")

data2 <- select(data2017, "Kod stacji", "MzWarWokalna", "MzWarKondrat")
colnames(data2) <- c("Data", "MzWarWokalna", "MzWarKondrat")
Data <- data2$Data
data2 <- as.data.frame(apply(
  data2[,-1],2, function(x) as.numeric(sub(",", ".", x, fixed = TRUE))))
data2$Data <- Data
data2$Data <- as.Date(as.numeric(data2$Data), origin = "1899-12-30")

data_dob <- read.xlsx("2017_PM25_1g.xlsx")
colnames(data_dob) <- data_dob[1,]
data_dob <- data_dob[-c(1, 2, 3, 4, 5),]
data3 <- select(data_dob, "Kod stacji", "MzWarWokalna")
colnames(data3) <- c("Data", "MzWarWokalna")
Data <- data3$Data
data3 <- as.data.frame(sapply(
  data3[,-1], function(x) as.numeric(sub(",", ".", x, fixed = TRUE))))
data3$Data <- Data
data3$Data <- convertToDateTime(as.numeric(data3$Data), origin = "1900-01-01", 
                                format = "%Y-%m-%d %H:%M")

ui <- dashboardPage(
  skin = "black",
  dashboardHeader(title = "Zanieczyszczenia"),
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("O aplikacji", icon = icon("info-circle"), tabName = "about")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem("dashboard",
              fluidRow(box(plotOutput("plot1"), width = 10, 
                           title = "Zanieczyszczenia w TOP10 miastach Polski"), 
                       box(checkboxGroupInput("miasta", 
                                         label = "Miasta:",
                                         choices = miasta,
                                        selected = miasta), width = 2)),
              fluidRow(box(plotOutput("plot2"), width = 8, title = 
                             "Zanieczyszczenia w Warszawie"), 
                       box(checkboxGroupInput("stacje", 
                                              label = "Stacje Warszawy:",
                                              choices = c("MzWarWokalna", 
                                                          "MzWarKondrat"),
                                              selected = c("MzWarWokalna", 
                                                           "MzWarKondrat")),
                           dateRangeInput("dates", "Daty:",
                                          start = "2017-01-01",
                                          end = "2017-12-31"), width = 4)),
              fluidRow(box(plotOutput("plot3"), width = 10, 
                           title = "Zanieczyszczenia w Warszawie godzinowo"),
                       box(dateInput("data", "Data:", min = "2017-01-01",
                                     max = "2017-12-31", value = "2017-01-01"), 
                           width = 2))
      ),
      tabItem("about",
              "W podanej wizualizacji możemy zbadać następujące zagadnienia:
          Jak bardzo zanieczyszczone są TOP 10 miast Polski.
              Jako mieszkańców stolicy nas interesuje zanieczyszenie Warszawy.
              Możemy bardziej sczegółowo zobaczyć o jakiej porze dnia i roku
          poziom dopuszczalny (czerwona linia) jest przekroczony"
      )
    )
  )
)

server <- function(input, output) {
  output[["plot1"]] <- renderPlot({
    validate(
      need(input[["miasta"]], 'Wybierz miasta')
    )
    data1 <- select(data1, "Data", input[["miasta"]])
    data1 <- melt(data1, id="Data")
    ggplot(data1, aes(x=Data, y=value, color = variable)) + 
      stat_smooth(se = FALSE, size = 1) +
      labs(y="PM2.5 [ug/m^3]") + 
      scale_colour_brewer("Miasto", palette="Set3") +
      theme_classic()
  })
  output[["plot2"]] <- renderPlot({
    validate(
      need(input[["stacje"]], 'Wybierz stacje')
    )
    data2 <- select(data2, "Data", input[["stacje"]])
    data2 <- melt(data2, id="Data")
    start <- input[["dates"]][1]
    end <- input[["dates"]][2]
    
    data2 <- data2[data2$Data<=end & data2$Data>=start,]
    ggplot(data2, aes(x=Data,y=value, color=variable)) +
      geom_point() +
      stat_smooth() +
      geom_hline(yintercept = 25, color = "red") +
      labs(y="PM2.5 [ug/m^3]") + 
      theme_classic()
  })
  output[["plot3"]] <- renderPlot({
    data3 <- melt(data3, id="Data")
    data3 %>% filter(as.Date(data3$Data)==input[["data"]]) %>%
    
    ggplot(aes(x=Data,y=value)) +
      geom_point() +
      geom_line() +
      scale_x_datetime(date_labels = "%H", date_breaks = "1 hour") +
      geom_hline(yintercept = 25, color = "red") +
      labs(y="PM2.5 [ug/m^3]") +
      theme_classic()
  })
}

shinyApp(ui, server)