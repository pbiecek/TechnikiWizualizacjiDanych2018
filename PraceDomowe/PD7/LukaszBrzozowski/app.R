library(ggplot2)
library(dplyr)
library(shiny)
library(shinydashboard)
#Potrzebne do załadowania tła wykresu
library(devtools)
library(ggthemr)
library(eurostat)
library(mapproj)

dane_As <- read.csv2("./dane/Statystki_As.csv", sep = ";", header  = TRUE, dec = ",")
dane_BaP <- read.csv2("./dane/Statystki_BaP.csv", sep = ";", header  = TRUE, dec = ",")
dane_C6H6 <- read.csv2("./dane/Statystki_C6H6.csv", sep = ";", header  = TRUE, dec = ",")
dane_Cd <- read.csv2("./dane/Statystki_Cd.csv", sep = ";", header  = TRUE, dec = ",")
dane_CO <- read.csv2("./dane/Statystki_CO.csv", sep = ";", header  = TRUE, dec = ",")
dane_Ni <- read.csv2("./dane/Statystki_Ni.csv", sep = ";", header  = TRUE, dec = ",")
dane_O3 <- read.csv2("./dane/Statystki_O3.csv", sep = ";", header  = TRUE, dec = ",")
dane_Pb <- read.csv2("./dane/Statystki_Pb.csv", sep = ";", header  = TRUE, dec = ",")
dane_PM2.5 <- read.csv2("./dane/Statystki_PM2_5.csv", sep = ";", header  = TRUE, dec = ",")
dane_PM10 <- read.csv2("./dane/Statystki_PM10.csv", sep = ";", header  = TRUE, dec = ",")
dane_NO2 <- read.csv2("./dane/Statystyki_NO2.csv", sep = ";", header  = TRUE, dec = ",")
dane_NOx <- read.csv2("./dane/Statystyki_NOx.csv", sep = ";", header  = TRUE, dec = ",")
dane_SO2 <- read.csv2("./dane/Statystyki_SO2.csv", sep = ";", header  = TRUE, dec = ",")

dane <- list(dane_As, dane_BaP, dane_C6H6, dane_Cd, dane_CO, dane_Ni, dane_O3, dane_Pb, dane_PM2.5, dane_PM10, dane_NO2, dane_NOx, dane_SO2)
temp <- as.data.frame(cbind((1:13), c("As", "BaP", "C6H6", "Cd", "CO", "Ni", "O3", "Pb", "PM2.5", "PM10", "NO2", "NOx", "SO2")))


lp <- get_eurostat_geospatial(output_class = "df", resolution = "60", nuts_level = 2)

ui <- dashboardPage(
  skin = "black",
  dashboardHeader(title = "Zanieczyszczenie powietrza w Polsce"),
  
  dashboardSidebar(
    sidebarUserPanel(Sys.info()[["effective_user"]],
                     subtitle = a(href = "#", icon("circle", class = "text-success"), "Online")
    ),
    sidebarMenu(
      id = "tabs",
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard"))
    )
  ),
  dashboardBody(
    titlePanel("Średnie zanieczyszczenie powietrza w Polsce w 2017 r."),
    fluidRow(
      infoBox("PM10", signif(mean(filter(dane_PM10, Rok == 2017)$Średnia), 4), "µg/m3", icon = icon("air-freshener"), color = "red"),
      infoBox("PM2.5", signif(mean(filter(dane_PM2.5, Rok == 2017)$Średnia), 4), "µg/m3", icon = icon("air-freshener"), color = "yellow"),
      infoBox("CO", signif(mean(filter(dane_CO, Rok == 2017)$Średnia), 4), "µg/m3", icon = icon("air-freshener"), color = "black")
    ),
    titlePanel("Zanieczyszczenie powietrza w województwach w 2017 roku"),
    selectizeInput(inputId = "dane1", 
                   label = "Wybierz rodzaj zanieczyszczenia:",
                   choices = temp[,2]),
    plotOutput("plot1"),
    titlePanel("Zanieczyszczenie powietrza w Polsce na przestrzeni lat"),
    selectizeInput(inputId = "dane", 
                   label = "Wybierz rodzaj zanieczyszczenia:",
                   choices = temp[,2]),
    
    plotOutput("plot")

      
    
  )
)

server<- function(input, output){
  
  temp_dane <- reactive(dane[[temp[,1][which(temp[,2] == input[["dane"]])]]])
  temp_dane1 <- reactive(dane[[temp[,1][which(temp[,2] == input[["dane1"]])]]])
  
  
  output[["plot"]] <- renderPlot({
    ggthemr("flat")
    ggplot(temp_dane(), aes(x = Rok, y = Średnia)) +
      geom_point() +
      labs(x = "Rok", y = "Pomiary w µg/m3", title = "Pomiary zanieczyszczeń powietrza w Polsce") +
      geom_smooth(method = "loess")
      
  })
  output[["plot1"]] <- renderPlot({
    ggthemr_reset()
    temp1 <- filter(lp, CNTR_CODE == "PL", LEVL_CODE == 2)
    temp1[temp1$NUTS_NAME == "Mazowiecki regionalny",]$NUTS_NAME <- "Mazowieckie"
    temp1[temp1$NUTS_NAME == "Warszawski stołeczny",]$NUTS_NAME <- "Mazowieckie"
    temp1$NUTS_NAME <- tolower(temp1$NUTS_NAME)

    names_df <- temp1 %>%
      group_by(NUTS_NAME) %>% 
      summarise(long = mean(long),
                lat = mean(lat))
    temp2 <- inner_join(temp1, temp_dane1(), by = c("NUTS_NAME" = "Województwo"))
    temp2 %>%
      group_by(NUTS_NAME) %>% 
      ggplot(aes(x = long, y = lat, group = group, fill = Średnia)) + 
      geom_polygon(color = "black") +
      geom_text(data = names_df, aes(x = long, y = lat, label = NUTS_NAME), inherit.aes = FALSE, color = "white") +
      labs(x = "", y = "") +
      coord_map() +
      theme_bw()
  })
    
}


shinyApp(ui, server)