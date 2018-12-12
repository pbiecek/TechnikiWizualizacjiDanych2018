library(shiny)
library(shinydashboard)
library(reshape2)
library(ggplot2)
library(dplyr)

data <- list()
data[['SO2']] <- read.csv('./SO2.csv')  %>% filter(Kompletność > 50)
data[['NO2']] <- read.csv('./NO2.csv')  %>% filter(Kompletność > 50)
data[['C6H6']] <- read.csv('./C6H6.csv')  %>% filter(Kompletność > 50)
data[['PM2.5']] <- read.csv('./PM2.5.csv')  %>% filter(Kompletność > 50)
data[['PM10']] <- read.csv('./PM10.csv')  %>% filter(Kompletność > 50)

dopuszczalne.poziomy <- data.frame(Wskaźnik = c("C6H6","NO2","SO2","PM10","PM2.5"), Wartość = c(5,40,100,40,20))
wskazniki <- c('SO2', 'NO2', 'C6H6', 'PM2.5', 'PM10')
wojewodztwa <- data[['PM2.5']] %>% select('Województwo') %>% distinct
wojewodztwa <- c('wszystkie', levels(wojewodztwa[['Województwo']]))

data[['PM2.5']] %>% group_by(Rok) %>% summarize(Średnia = mean(Średnia), Maks = max(Maks), Min = max(min(Min), 0))

### User Interface
ui <- dashboardPage(
  skin = "black",
  dashboardHeader(title = "Powietrze w Polsce",
                  dropdownMenu(type = "notifications", badgeStatus = "warning",
                               notificationItem(icon = icon("exclamation-triangle"), status = "info",
                                                "Uwaga! Zanieczyszczone powietrze!!!!")
                              )
                  ),
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Informacje", icon = icon("info-circle"), tabName = "Informacje", badgeLabel = "check it!",
               badgeColor = "green")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem("dashboard",
        sidebarLayout(
          sidebarPanel(
            selectInput(inputId = "wskaznik1",
                        label = div("Wskaźniki do porównania.", style = "font-family:Arial"),
                        choices = wskazniki,
                        selected = "PM2.5"),
            selectInput(inputId = "wskaznik2",
                        label = div("", style = "font-family:Arial"),
                        choices = wskazniki,
                        selected = "PM10"),
            hr(),
            radioButtons(inputId = "wojewodztwo",
                         label = div("Województwo", style = "font-family:Arial"),
                         choices = wojewodztwa,
                         selected = "wszystkie")),
          mainPanel(br(),
                    plotOutput(outputId = "punkty_pomiarowe"),
                    br(),
                    selectInput(inputId = "srednia_czy_maks",
                                label = div("Agregat wartości wskaźnika rocznego", style = "font-family:Arial"),
                                choices = c("Maks","Średnia"),
                                selected = "Maks"),
                    plotOutput(outputId = "srednie_i_maks"),
                    br(),
                    plotOutput(outputId = "top3"),
                    br())
        )
      ),
      tabItem("Informacje",
              div("Analizując dane udostępnione na dashboardzie, możemy zobaczyć, że prawdziwym przełomem dla świadomości nt. zagrożenia związanego z zanieczyszczeniem powietrza był rok 2010. Wtedy w całej Polsce zaczęły się pojawiać stację pomiarowe pyłów PM2.5. Widać też, że na południu Polski świadomość na temat zanieczyszczenia powietrza istniała wśród ludzi od dłuższego czasu. Nic dziwnego, skoro są to tereny dużo bardziej skupione na przemyśle niż północna Polska.", 
                  style = "font-family:Arial;font-size:15px"),
              div("Autor: Kacper Siemaszko.", 
                  style = "font-family:Arial;font-size:15px")
      )
    )
  )
)



### Server
server <- function(input, output) {

  ### Reactives
  
  pollution1 <- reactive({
    x <- data[[input[['wskaznik1']]]] %>% filter(input[['wojewodztwo']] == 'wszystkie' | Województwo == input[['wojewodztwo']])
    x[,c("Średnia", "Min","Maks")] <- 100*x[,c("Średnia", "Min","Maks")]/dopuszczalne.poziomy[dopuszczalne.poziomy$Wskaźnik == input[['wskaznik1']],"Wartość"]
    x  
  })
  
  pollution2 <- reactive({
    x <- data[[input[['wskaznik2']]]] %>% filter(input[['wojewodztwo']] == 'wszystkie' | Województwo == input[['wojewodztwo']])
    x[,c("Średnia", "Min","Maks")] <- 100*x[,c("Średnia", "Min","Maks")]/dopuszczalne.poziomy[dopuszczalne.poziomy$Wskaźnik == input[['wskaznik2']],"Wartość"]
    x
  })
  
  pollution <- reactive({
    if(input[['wskaznik1']]!=input[['wskaznik2']]){
    names <- intersect(names(pollution1()), names(pollution2()))
    rbind(pollution1()[,names], pollution2()[,names])
    } else {
      pollution1()
    }
  })
  
  air.quality.index <- reactive({
    pollution() %>% group_by(Rok, Wskaźnik) %>% summarize(Średnia = mean(Średnia), Maks = max(Maks), Min = max(min(Min), 0))
  })
  
  top3 <- reactive({
    if(input[['wskaznik1']]!=input[['wskaznik2']]){
    rbind(
      pollution1()[order(pollution1()[[input[['srednia_czy_maks']]]],decreasing = TRUE)[1:3],
                   c('Kod.stacji', 'Wskaźnik', input[['srednia_czy_maks']])],
      pollution2()[order(pollution2()[[input[['srednia_czy_maks']]]],decreasing = TRUE)[1:3],
                   c('Kod.stacji', 'Wskaźnik', input[['srednia_czy_maks']])]) %>% arrange(Wskaźnik)
    } else {
      pollution1()[order(pollution1()[[input[['srednia_czy_maks']]]],decreasing = TRUE)[1:3],
                   c('Kod.stacji', 'Wskaźnik', input[['srednia_czy_maks']])] %>% arrange(Wskaźnik)
    }
  })
  
  plot1 <- reactive({
     p <- ggplot(data = pollution() %>% group_by(Rok, Wskaźnik) %>% count, aes(x = Rok, y = n, fill = Wskaźnik)) +
      geom_bar(stat = 'identity',
               position = position_dodge(preserve = "single")) +
      xlim(c(2000, 2018))
     if(input[['wojewodztwo']]=='wszystkie'){
       p + ylim(c(0, 400))
     } else {
       p + ylim(c(0, 100))
     }
  })
  
  plot2 <- reactive({
    ggplot(data = air.quality.index(), aes(x = Rok, y = air.quality.index()[[input[["srednia_czy_maks"]]]], color = Wskaźnik)) + 
      geom_line(size = 2) +
      geom_point(size = 4) +
      geom_hline(yintercept = 100, color = "red") +
      geom_label(aes(x = 2015, y = 100, label = "Dopuszczalne stężenie danej substancji"), inherit.aes = FALSE) +
      xlim(c(2000, 2018)) +
      ylim(c(0,NA))
    
  })
  
  plot3 <- reactive({
     ggplot(data = top3(), aes(x = factor(Kod.stacji, levels = top3()[['Kod.stacji']] %>% unique), y = top3()[[input[['srednia_czy_maks']]]], fill = Wskaźnik)) +
       geom_bar(stat = "identity", position = position_dodge(preserve = "single"))
  })
  
  output[['punkty_pomiarowe']] <- renderPlot({
    plot1() + theme_bw() +
      xlab("Rok") +
      ylab("Liczba punktów pomiarowych") +
      labs(title = "
           Zmiana liczby punktów pomiarowych wybranych rodzajów zanieczyszczeń.
           ") +
      theme(plot.title = element_text(size = 18))
    })
  
  output[['srednie_i_maks']] <- renderPlot({
    plot2() +
      theme_bw() +
      xlab("Rok") +
      ylab("% Maksymalnego dopuszczalnego stężenia danej substancji") +
      labs(title = "
           Zmiana agregatu wartości wskaźnika zanieczyszczenia
           ") +
      theme(plot.title = element_text(size = 18))
  })

  output[['top3']] <- renderPlot({
    plot3() +
      theme_bw() +
      xlab("Stacja pomiarowa") +
      ylab("% Maksymalnego dopuszczalnego stężenia danej substancji") +
      labs(title = "
            Top 3 stacje z największym stężeniem danych substancji
           ") +
      theme(plot.title = element_text(size = 18))
  })
}



### Shiny App
shinyApp(ui, server)


