require(shinydashboard)
require(leaflet)
require(plotly)
require(htmlwidgets)
require(ggplot2)

ui <- dashboardPage(skin = "black",
                    dashboardHeader(title = "Dane pomiarowe PM10 w roku 2017", titleWidth = 450),
                    dashboardSidebar(
                      sidebarMenu(
                        menuItem("O projekcie", tabName = "Oprojekcie"),
                        menuItem("Mapy", tabName = "Mapy"),
                        menuItem("Analizy", tabName = "Analizy")
                      )
                    ),
                    dashboardBody(
                      tabItems(
                        tabItem(tabName = "Oprojekcie",
                                includeMarkdown("o projekcie.Rmd")
                        ),
                        tabItem(tabName = "Mapy",
                                fluidRow(
                                  box(leafletOutput("mapa_wojewodztwa", height = 500)),
                                  box(leafletOutput("mapa_miasta", height = 500)),
                                  box(h3("Agregacja według"),
                                      radioButtons("wybor_agregacje",
                                                   label = " ",
                                                   choices = c("Liczba stacji",
                                                               "Wskaźnik pyłu"),
                                                   selected = "Liczba stacji"))
                                  
                                ) 
                        ),
                        tabItem(tabName = "Analizy",
                                box(h3("TOP miast"),
                                    sliderInput("ile_top",
                                                label = "",
                                                min = 1,
                                                max = 10,
                                                value = 5
                                    ),
                                    h3("Pomiar"),
                                    radioButtons("wybor_top",
                                                 label = " ",
                                                 choices = c("Największy",
                                                             "Najmniejszy"),
                                                 selected = "Największy"),
                                    
                                    plotOutput("top")),
                                box(plotlyOutput("szereg"))
                        ))
                    )
)

server <- function(input, output) {
  output$top <- renderPlot({
    czystetop <- read.csv("stacjepm10_per_miasto_top10_czyste.csv")
    czystetop$Miejscowosc <- factor(czystetop$Miejscowosc, levels = czystetop$Miejscowosc[order(czystetop$Wartosc.rok)])
    zanieczyszczonetop <- read.csv("stacjepm10_per_miasto_top10_zanieczyszczone.csv")
    zanieczyszczonetop$Miejscowosc <- factor(zanieczyszczonetop$Miejscowosc, levels = zanieczyszczonetop$Miejscowosc[order(-zanieczyszczonetop$Wartosc.rok)])
    if(input[["wybor_top"]] == "Największy"){
      df <- zanieczyszczonetop[1:input[["ile_top"]],]
      p <- ggplot(df, aes(x = Miejscowosc, y = Wartosc.rok)) + geom_bar(stat = "identity", fill = "#3182bd") + theme_minimal() + labs(x = " ", y =" Wskaźnik pyłu - suma rok") + theme(axis.text.x = element_text(angle = 60, hjust = 1))
      p
    }else{
      df <- czystetop[1:input[["ile_top"]],]
      p <- ggplot(df, aes(x = Miejscowosc, y = Wartosc.rok)) + geom_bar(stat = "identity", fill = "#3182bd")  + theme_minimal() + labs(x = " ", y =" Wskaźnik pyłu - suma rok") + theme(axis.text.x = element_text(angle = 60, hjust = 1))
      p
    }
  })
  
  output$szereg <- renderPlotly({
    dane <- read.csv("sredni_wskaznik_pylu.csv")
    dane$date <- as.Date(dane$date)
    x <- list(
      title = "Data"
    )
    y <- list(
      title = "Średni wskaźnik pyłu w Polsce"
    )
    p <- plot_ly(dane, x = ~date, y = ~values, mode = 'lines', line = list(color = 'rgb(37, 37, 37)', width = 1)) %>%
      layout(xaxis = x, yaxis = y)
    p
    
  })
  
  output$mapa_wojewodztwa <- renderLeaflet({
    if(input[["wybor_agregacje"]] == "Liczba stacji"){wybor = "liczba.stacji"}else{wybor = "wartosc.rok"}
    nycounties <- geojsonio::geojson_read(x = "map.geojson", what = "sp")
    pal <- colorNumeric("viridis", NULL)
    leaflet(nycounties) %>%
      addTiles() %>%
      addPolygons(stroke = FALSE, smoothFactor = 0.3, fillOpacity = 0.6,
                  fillColor = ~pal((get(wybor))),
                  label = ~paste0(name, ", ", formatC(get(wybor), big.mark = " "))) %>%
      addLegend(pal = pal, values = ~get(wybor), opacity = 0.6,  title = if(input[["wybor_agregacje"]] == "Liczba stacji"){"Liczba stacji"}else{"Wskaźnik pyłu"})
    
  })
  output$mapa_miasta <- renderLeaflet({
    options(scipen = 999)
    if(input[["wybor_agregacje"]] == "Liczba stacji"){
      stacjepm10_per_miasto_liczba_stacji <- read.csv("stacjepm10_per_miasto_liczba_stacji.csv")
      pal2 <- colorNumeric("viridis", NULL)
      m1 <- leaflet(stacjepm10_per_miasto_liczba_stacji) %>% setView(lng = 19, lat = 51.75, zoom =6)
      m1 %>% addTiles() %>% addCircleMarkers(radius = ~n, color = ~pal2(n), label = ~paste0(Miejscowosc, ", ", formatC(n, big.mark = " ")))%>%
        addLegend(pal = pal2, values = ~n, opacity = 0.6,  title = "Liczba stacji")
      
    }else{
      stacjepm10_per_miasto <- read.csv("stacjepm10_per_miasto.csv")
      pal <- colorNumeric("viridis", NULL)
      m <- leaflet(stacjepm10_per_miasto) %>% setView(lng = 19, lat = 51.75, zoom =6)
      m %>% addTiles() %>% addCircleMarkers(radius = ~promien, color = ~pal(Wartosc.rok), label = ~paste0(Miejscowosc, ", ", formatC(Wartosc.rok, big.mark = " ")))%>%
        addLegend(pal = pal, values = ~Wartosc.rok, opacity = 0.6,  title = "Wskaźnik pyłu")
      
    }
  })
  
}

shinyApp(ui, server)