
require(shiny)
require(shinydashboard)
#require(reshape2)
require(dplyr)
#require(ggplot2)
require(plotly)
#require(readxl)
require(rgdal)
require(RColorBrewer)
require(classInt)




#setwd("C:/Users/Dawid.Kowalczyk/Google Drive/Studia MiNI PW/III semestr/Techniki wizualizacji danych/PD7/")
setwd("C:/Users/Dawid Kowalczyk/Google Drive/Studia MiNI PW/III semestr/Techniki wizualizacji danych/PD7/")


df_avg = read.csv("df_avg.csv")
srednie = read.csv("srednie.csv")

SO2 = read.csv("SO2.csv")
NO2 = read.csv("NO2.csv")
PM10 = read.csv("PM10.csv")
PM25 = read.csv("PM25.csv")


woj <- readOGR(".", "wojewodztwa")
woj <- spTransform(woj, CRS("+proj=longlat +datum=NAD83"))







ui <- dashboardPage(
  skin = "black",
  dashboardHeader(title = "Praca domowa 7 TWD - jakość powietrza w Polsce"),
  
  dashboardSidebar(
    sidebarUserPanel(Sys.info()[["effective_user"]],
                     subtitle = a(href = "#", icon("circle", class = "text-success"), "Online")
    ),
    sidebarMenu(
      id = "tabs",
      menuItem("Wstęp", tabName = "wstep"),
      menuItem("Średnie stężenie PM25 w Polsce", tabName = "stezenia"),
      menuItem("Średnie w województwach", tabName = "predict_between"),
      menuItem("Mapka zanieczyszczeń", tabName = "mapa")
    )
  ),
  dashboardBody(
    tabItems(
      
      
      tabItem(tabName = "wstep",
              includeMarkdown("wstep.Rmd")
      ),

      tabItem("stezenia",
              fluidPage(dateRangeInput(inputId = 'dateRange',
                                       label = 'Zakres dat: yyyy-mm-dd',
                                       start = "2013-02-01", end = "2017-10-31"),
              plotlyOutput("wykres_lata"))),
              
      tabItem("predict_between",
              fluidPage(
                selectInput(inputId = "chosen_gas",
                            label = "Wybierz rodzaj zanieczyszczenia",
                            choices = c("SO2", "NO2", "PM10", "PM25")
              ),  plotOutput("srednie"))),
      
      tabItem("mapa",
              fluidPage(
                selectInput(inputId = "rok",
                            label = "Wybierz rok",
                            choices = seq(2004, 2017, by=1))
                ),
              plotOutput("wykres_mapa", width = "100%", height = "800px")))
  )

)





server <- function(input, output) {
  

  output[["wykres_lata"]] <- renderPlotly({
   srednie$Czas = as.Date(srednie$Czas)
   srednie1 <- srednie[srednie$Czas  >= as.character(input$dateRange[1]) & srednie$Czas  <= as.character(input$dateRange[2]), ]
   plot_ly(srednie1, x=~Czas, y=~avg, type = "scatter", mode = "line")  %>%
     layout(title = "Średnie stężenie pyłu PM2.5 w Polsce",
            xaxis = list(title = "Data"),
            yaxis = list(title = "Średnie stężenie"))
   })
  
  output[["srednie"]] <- renderPlot({
    df_avg1 <- df_avg[, c("gas", input[["chosen_gas"]]) ]
    ggplot(df_avg1, aes(x=gas, y=df_avg[[input[["chosen_gas"]]]]  )) +
      geom_bar( position = "dodge", stat="identity", width = 0.7) +
      theme(axis.text.x = element_text(angle=60, hjust=1)) +
      scale_fill_brewer(palette = "Paired") +
      labs(title="Stężenie substancji w powetrzu (srednia na podstawie lat 2000 - 2017) ", x="Wojewodztwo", y = "Średnie stezenie")
    
  })
  
  
  
  output[["wykres_mapa"]] <- renderPlot({
    
    table = NO2[which(NO2$Rok ==  input[["rok"]]), c(1,2,8)]
    
    table = select(table, Województwo, Średnia) %>%
      group_by(Województwo) %>%
      mutate(mean = mean(Średnia)) %>% select(Województwo, mean) %>% distinct() %>% arrange(Województwo)
    
    nazwa_woj = data.frame(woj$jpt_nazwa_)
    names(nazwa_woj) = "Województwo"
    
    table = data.frame(left_join(nazwa_woj, table))
    
    
    kolory = brewer.pal(9, "YlOrBr")
    klasy = classIntervals(table$mean, 9, style="fixed", 
                           fixedBreaks= round(seq(from=min(as.numeric(table[,1])), to=max(as.numeric(table[,2])), length.out = 9)),2)
    tabela.kolorów = findColours(klasy, kolory) 
    
    plot(woj, col=tabela.kolorów)
    legend("bottomleft", legend=names(attr(tabela.kolorów, "table")), fill=attr(tabela.kolorów, "palette"), cex=1, bty="n")
    title(main="Średnie stężenie NO2 w województwach")
    
    crds.woj = coordinates(woj)
    woj.df = as.data.frame(woj)
    text(crds.woj, label=woj.df$jpt_nazwa_, cex=0.8, font=2)
    
  })
  
}

shinyApp(ui, server)


