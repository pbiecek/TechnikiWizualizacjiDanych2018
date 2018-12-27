library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(scales)
library(shinyjs)

files_path <- ""

pm10_data <- read.csv(paste(files_path, "PM10.csv", sep = ""), sep = ";")
no2_data <- read.csv(paste(files_path, "NO2.csv", sep = ""), sep = ";")
nox_data <- read.csv(paste(files_path, "NOx.csv", sep = ""), sep = ";")

pm10_data$Data <- as.Date(pm10_data$Data, format = "%m/%d/%Y")
pm10_data <- data.frame(pm10_data %>% 
  group_by(Data) %>% 
  summarise(Warszawa.Komunikacyjna = mean(Warszawa.Komunikacyjna, na.rm = TRUE), 
                                           Warszawa.Targowek = mean(Warszawa.Targowek, na.rm = TRUE),
                                           Warszawa.Ursynow = mean(Warszawa.Ursynow, na.rm = TRUE)))

no2_data$Data <- as.Date(no2_data$Data, format = "%m/%d/%Y")
no2_data <- data.frame(no2_data %>% 
                          group_by(Data) %>% 
                          summarise(Warszawa.Komunikacyjna = mean(Warszawa.Komunikacyjna, na.rm = TRUE), 
                                    Warszawa.Targowek = mean(Warszawa.Targowek, na.rm = TRUE),
                                    Warszawa.Ursynow = mean(Warszawa.Ursynow, na.rm = TRUE)))

nox_data$Data <- as.Date(nox_data$Data, format = "%m/%d/%Y")
nox_data <- data.frame(nox_data %>% 
                         group_by(Data) %>% 
                         summarise(Warszawa.Komunikacyjna = mean(Warszawa.Komunikacyjna, na.rm = TRUE), 
                                   Warszawa.Targowek = mean(Warszawa.Targowek, na.rm = TRUE),
                                   Warszawa.Ursynow = mean(Warszawa.Ursynow, na.rm = TRUE)))

ui <- dashboardPage(
  dashboardHeader(title = "Air pollution in Warsaw"
                  
                  ),
  dashboardSidebar(
    sidebarMenu(
      id = "tabs-menu",
      menuItem("Air pollution dashboard", tabName = "main-dashboard", icon = icon("dashboard")),
      menuItem("About", icon = icon("info-circle"), tabName = "about"),
      sliderInput("month_input", "Months:", min = 1, max = 12, step = 1, value = c(1,12)),
      checkboxInput("whole", "Whole Warsaw", value = TRUE),
      checkboxInput("komunikacyjna","Warsaw, Śródmieście"),
      checkboxInput("usrynow","Warsaw, Ursynów"),
      checkboxInput("targowek","Warsaw, Targówek"),
      radioButtons("avg_type",
                   label = "Average per:",
                   choices = c("Month", "Day"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        "main-dashboard",
        plotOutput("pm10-output"),
        plotOutput("no2-output"),
        plotOutput("nox-output")
        ),
      tabItem("about",
              "Dashboard displaying selected indicators(PM10, NO2, NOx) of air pollution in Warsaw in 2017 in selected regions (Ursynów, Śródmieście, Targówek). 
              The user can select if he wants to have an average indicator per month or per day."
      )
      )
    )
  )

server <- function(input, output) {

  x_label_r <- reactive({
    if(input[["avg_type"]] == "Month" | input[["avg_type"]] == "Day") {
      "Month"
    }
  })
  
  data_pm10_max <- reactive({
    d <- data_pm10_r()
    max(d %>% select(Warszawa.Komunikacyjna, Warszawa.Targowek, Warszawa.Ursynow, Whole), na.rm = TRUE)*1.1
  })
  
  data_no2_max <- reactive({
    d <- data_no2_r()
    max(d %>% select(Warszawa.Komunikacyjna, Warszawa.Targowek, Warszawa.Ursynow, Whole), na.rm = TRUE)*1.1
  })
  
  data_nox_max <- reactive({
    d <- data_nox_r()
    max(d %>% select(Warszawa.Komunikacyjna, Warszawa.Targowek, Warszawa.Ursynow, Whole), na.rm = TRUE)*1.1
  })
  
  aggregate_data <- function(d) {
    d <- data.frame(d)
    colnames(d) <- c("Data", "Warszawa.Ursynow", "Warszawa.Komunikacyjna", "Warszawa.Targowek")
    d$Whole <- rowMeans(d %>% select(Warszawa.Targowek, Warszawa.Ursynow, Warszawa.Komunikacyjna), na.rm = TRUE)
    d 
  }
  
  data_pm10_r <- reactive({
    d <- pm10_data %>% 
      filter(as.numeric(format(as.Date(Data), "%m")) <= input[["month_input"]][2] & as.numeric(format(as.Date(Data), "%m")) >= input[["month_input"]][1])
    if(input[["avg_type"]] == "Month") {
      d <- d %>% 
        group_by(format(as.Date(Data), "%m")) %>% 
        summarise(avg_urs = mean(Warszawa.Ursynow, na.rm = TRUE), kom_urs = mean(Warszawa.Komunikacyjna, na.rm = TRUE), tar_urs = mean(Warszawa.Targowek, na.rm = TRUE))
    } 
    
    aggregate_data(d)
  })
  
  data_no2_r <- reactive({
    d <- no2_data %>% 
      filter(as.numeric(format(as.Date(Data), "%m")) <= input[["month_input"]][2] & as.numeric(format(as.Date(Data), "%m")) >= input[["month_input"]][1])
    if(input[["avg_type"]] == "Month") {
      d <- d %>% 
        group_by(format(as.Date(Data), "%m")) %>% 
        summarise(avg_urs = mean(Warszawa.Ursynow, na.rm = TRUE), kom_urs = mean(Warszawa.Komunikacyjna, na.rm = TRUE), tar_urs = mean(Warszawa.Targowek, na.rm = TRUE))
    }
    
    aggregate_data(d)
  })
  
  data_nox_r <- reactive({
    d <- nox_data %>% 
      filter(as.numeric(format(as.Date(Data), "%m")) <= input[["month_input"]][2] & as.numeric(format(as.Date(Data), "%m")) >= input[["month_input"]][1])
    if(input[["avg_type"]] == "Month") {
      d <- d %>% 
        group_by(format(as.Date(Data), "%m")) %>% 
        summarise(avg_urs = mean(Warszawa.Ursynow, na.rm = TRUE), kom_urs = mean(Warszawa.Komunikacyjna, na.rm = TRUE), tar_urs = mean(Warszawa.Targowek, na.rm = TRUE))
    }
    
    aggregate_data(d)
  })
  
  alpha_r <- reactive({
    if(input[["avg_type"]] == "Month") {
      1
    } else {
      0.5
    }
  })
  
  output[["pm10-output"]] <- renderPlot({
    plot <- ggplot(data_pm10_r()) +
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5))+
      ggtitle("Average PM10 per ug/m^3 in Warsaw in 2017")
    
    if(input[["whole"]]) {
      plot <- plot + geom_line(aes(x = Data, y = Whole, group = 1, color = "Whole Warsaw"), size = 1.20, alpha = alpha_r())
    }
    
    if(input[["komunikacyjna"]]) {
      plot <- plot + geom_line(aes(x = Data, y = Warszawa.Komunikacyjna, color = "Śródmieście", group = 1), size = 1.20, alpha = alpha_r())
    }
    
    if(input[["usrynow"]]) {
      plot <- plot + geom_line(aes(x = Data, y = Warszawa.Ursynow, color = "Ursynów", group = 1), size = 1.20, alpha = alpha_r())
    }
    
    if(input[["targowek"]]) {
      plot <- plot + geom_line(aes(x = Data, y = Warszawa.Targowek, color = "Targówek", group = 1), size = 1.20, alpha = alpha_r())
    }
    
    plot <- plot + scale_y_continuous(expand = c(0,0), limits = c(0, data_pm10_max())) +
      labs(y = "MP10 (ug/m^3)", x = x_label_r(), color = "Place")

    plot  
  })
  
  output[["no2-output"]] <- renderPlot({
    plot <- ggplot(data_no2_r()) +
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5))+
      ggtitle("Average NO2 per ug/m^3 in Warsaw in 2017")
    
    if(input[["whole"]]) {
      plot <- plot + geom_line(aes(x = Data, y = Whole, group = 1, color = "Whole Warsaw"), size = 1.20, alpha = alpha_r())
    }
    
    if(input[["komunikacyjna"]]) {
      plot <- plot + geom_line(aes(x = Data, y = Warszawa.Komunikacyjna, color = "Śródmieście", group = 1), size = 1.20, alpha = alpha_r())
    }
    
    if(input[["usrynow"]]) {
      plot <- plot + geom_line(aes(x = Data, y = Warszawa.Ursynow, color = "Ursynów", group = 1), size = 1.20, alpha = alpha_r())
    }
    
    if(input[["targowek"]]) {
      plot <- plot + geom_line(aes(x = Data, y = Warszawa.Targowek, color = "Targówek", group = 1), size = 1.20, alpha = alpha_r())
    }
    
    plot <- plot + scale_y_continuous(expand = c(0,0), limits = c(0, data_no2_max())) +
      labs(y = "NO2 (ug/m^3)", x = x_label_r(), color = "Place")
    
    plot 
  })
  
  output[["nox-output"]] <- renderPlot({
    plot <- ggplot(data_nox_r()) +
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5))+
      ggtitle("Average NOx per ug/m^3 in Warsaw in 2017")
    
    if(input[["whole"]]) {
      plot <- plot + geom_line(aes(x = Data, y = Whole, group = 1, color = "Whole Warsaw"), size = 1.20, alpha = alpha_r())
    }
    
    if(input[["komunikacyjna"]]) {
      plot <- plot + geom_line(aes(x = Data, y = Warszawa.Komunikacyjna, color = "Śródmieście", group = 1), size = 1.20, alpha = alpha_r())
    }
    
    if(input[["usrynow"]]) {
      plot <- plot + geom_line(aes(x = Data, y = Warszawa.Ursynow, color = "Ursynów", group = 1), size = 1.20, alpha = alpha_r())
    }
    
    if(input[["targowek"]]) {
      plot <- plot + geom_line(aes(x = Data, y = Warszawa.Targowek, color = "Targówek", group = 1), size = 1.20, alpha = alpha_r())
    }
    
    plot <- plot + scale_y_continuous(expand = c(0,0), limits = c(0, data_nox_max())) +
      labs(y = "NOx (ug/m^3)", x = x_label_r(), color = "Place")
    
    plot 
  })
}

shinyApp(ui = ui, server = server)

