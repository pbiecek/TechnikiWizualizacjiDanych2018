library(shiny)
library(shinydashboard)
library(reshape2)
library(data.table)
library(ggplot2)
library(leaflet)
library(RColorBrewer)

load("dane.RData")

ui <- dashboardPage(
  skin = "black",
  dashboardHeader(title = "Zanieczyszczenie powietrza"),
  dashboardSidebar(
    
    sidebarMenu(
      id = "tabs",
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Mapa", tabName = "mapa", icon = icon("map")),
      menuItem("O aplikacji", icon = icon("info-circle"), tabName = "about",
               badgeColor = "green"),
      htmlOutput("menu")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem("dashboard",
              plotOutput("pm10_plot"),
              plotOutput("pm25_plot")
      ),
      tabItem("mapa",
              leafletOutput("mapa_zanieczysczen", height=800)
      ),
      tabItem("about",
              "Aplikacja służy do wizualizacji zanieczyszczenia powietrza w Polsce. W zakładce dashboard możemy wyświetlać i porównywać przebiegi
                             stężenia pyłów dla wybranych stacji. Można na nich dostrzec dużą zależność między stężeniami pm 10 i pm 2.5 dla pobliskich stacji.
                             Zakładka mapa pozwala wyświetlić dane o zanieczyszczeniu powietrza dla wybranego dnia w całym kraju."
      )
    )
  )
)

server <- function(input, output) {

  wybraneCzas <- reactive({
    start <- input$przedzial[1]
    end <- input$przedzial[2]
    pm10sel <- pm[[1]]
    pm25sel <- pm[[2]]
    pm10sel <- pm10sel[czas>=start & czas<=end,]
    pm25sel <- pm25sel[czas>=start & czas<=end,]
    list(pm10sel, pm25sel)
  })
  
  output$menu <- renderUI({
    
    if(input$tabs=="dashboard"){
      wellPanel(style = "background: #1e282c",
        selectInput("stacje10", "Stacje pomiarowe PM10", nazwy10$`Nazwa stacji`, selected="Kraków, ul. Bujaka", multiple = TRUE),
        selectInput("stacje25", "Stacje pomiarowe PM2.5", nazwy25$`Nazwa stacji`, selected="Kraków, ul. Bujaka", multiple = TRUE),
        dateRangeInput("przedzial", "Przedział czasowy", start="2015-01-01", end="2017-12-31", min="2002-01-01", max="2017-12-31", language="pl", startview="year")
      )
    }else if(input$tabs=="mapa"){
      wellPanel(style = "background: #1e282c",
      dateInput("mapdate", "Wybierz dzień", language="pl",min="2002-01-01", max="2017-12-31", value="2017-12-31"),
      radioButtons("maptype", "Rodzaj zanieczszczeń", choiceValues=c("pm25", "pm10"), selected="pm10", choiceNames = c("PM2.5", "PM10"))
      )
    }
    
  })
  output$pm10_plot <- renderPlot({
    if(length(input$stacje10)==0){
      return()
    }
    dane <- wybraneCzas()
    dane <- dane[[1]]
    kody <- nazwy[nazwy$`Nazwa stacji`%in%input$stacje10,]$`Kod stacji`
    dane <- dane[, c("czas", kody), with=FALSE]
    dane <- melt(dane, id.vars = "czas")
    ggplot(dane, aes(x=czas, y=value, color=variable)) + geom_line() + labs(title="Stężenie pyłów PM10", x="Data", y="Stęzenie PM10 [ug/m3]", color="Kod stacji") +
      geom_hline(aes(yintercept=50, linetype = "Poziom dopuszczalny"), color="green") + 
      geom_hline(aes(yintercept= 200, linetype = "Poziom informowania"), colour= 'red') +
      geom_hline(aes(yintercept= 300, linetype = "Poziom alarmowy"), colour= 'darkorchid4') +
      scale_linetype_manual(name = "Normy stężenia pyłu", values = c(2, 2, 2),breaks=c("Poziom alarmowy", "Poziom informowania", "Poziom dopuszczalny"), 
                            guide = guide_legend(override.aes = list(color = c("darkorchid4", "red", "green"))))
      
  })
  output$pm25_plot <- renderPlot({
    if(length(input$stacje25)==0){
      return()
    }
    dane <- wybraneCzas()
    dane <- dane[[2]]
    kody <- nazwy[nazwy$`Nazwa stacji`%in%input$stacje25,]$`Kod stacji`
    dane <- dane[, c("czas", kody), with=FALSE]
    dane <- melt(dane, id.vars = "czas")
    ggplot(dane, aes(x=czas, y=value, color=variable)) + geom_line() + labs(title="Stężenie pyłów PM2.5", x="Data", y="Stęzenie PM2.5 [ug/m3]", color="Kod stacji")
  })
  output$mapa_zanieczysczen <- renderLeaflet({
    if(length(input$maptype)==0){
      return()
    }
    if(input$maptype=="pm10"){
      pm10 <- pm[[1]]
      dane <- pm10[pm10$czas==input$mapdate,]
    }else{
      pm25 <- pm[[2]]
      dane <- pm25[pm25$czas==input$mapdate,]
    }
    dane <- melt(dane, id.vars="czas")
    setkey(dane, variable)
    dane <- dane[nazwy, nomatch=0]
    dane[,"label"] <- paste(dane$`Nazwa stacji`,": ", dane$value, " ug/m3",sep="")
    leaflet(dane) %>% setView(lng = 21.005995, lat = 52.231838, zoom = 10)  %>% addTiles() %>% addCircleMarkers(lng = ~lng, lat = ~lat, radius = 20,
                                                                                                      popup = ~label, color=~pal(value))
    
  })
  
}

shinyApp(ui, server)