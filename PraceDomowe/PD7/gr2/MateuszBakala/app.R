library(dplyr)
library(ggplot2)
library(shiny)
library(shinydashboard)

load("app.RData")

ui <- dashboardPage(
  dashboardHeader(title = "O polskim powietrzu"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Liczby", tabName = "liczby"),
      menuItem("Wykresy", tabName = "wykresy"),
      menuItem("O aplikacji", tabName = "about")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "liczby",
        fluidRow(
          column(width = 6,
                 infoBoxOutput("sredniaInfo", width = NULL),
                 infoBoxOutput("maxInfo", width = NULL),
                 infoBoxOutput("liczbaInfo", width = NULL)
          ),
          column(width = 6,
            box(title = "Sekcja kontroli liczb", status = "info", solidHeader = TRUE, width = NULL,
              box(radioButtons(inputId = "wskaznik",
                               label = "Wskaznik",
                               choices = c("PM10", "PM2.5")), width = NULL),
              box(selectizeInput(inputId = "wojewodztwo",
                                 label = "Wojewodztwo",
                                 choices = unique(statystykiPM[, "Wojewodztwo"])), width = NULL),
              box(sliderInput(inputId = "rok",
                              label = "Rok",
                              min = 2000,
                              max = 2017,
                              value = c(2008, 2015)), width = NULL)
            )
          )
        )
      ),
      tabItem(tabName = "wykresy",
        fluidRow(
          tabBox(width = 8,
            tabPanel(title = "Srednia wartosc wybranego wskaznika na przestrzeni lat", plotOutput("plotTime"), value = "plotTimeTab"),
            tabPanel(title = "Ilosc wykonanych pomiarow na przestrzeni lat", plotOutput("plotAmount"), value = "plotAmountTab")
          ),
          column(width = 4,
            box(title = "Sekcja kontroli liczb", status = "info", solidHeader = TRUE, width = NULL,
                box(selectizeInput(inputId = "wskaznikPlot",
                                  label = "Wskaznik",
                                  choices = sort(unique(statystyki[, "Wskaznik"]))), width = NULL),
                box(selectizeInput(inputId = "kodStacji",
                                   label = "Stacja",
                                   choices = lapply(split(statystyki[, "NAZWA.STACJI"], statystyki[, "Wojewodztwo"]), unique),
                                   multiple = TRUE), width = NULL)
            )
          )
        )
      ),
      tabItem(tabName = "about",
              h3("Zadania aplikacji"),
              textOutput("aboutText"))
    )
  )
)

server <- function(input, output, session) {
  
  statystykiPM_r <- reactive({
    filter(statystykiPM, Wskaznik == input[["wskaznik"]] & Wojewodztwo == input[["wojewodztwo"]] &
             Rok %in% (input[["rok"]][1]:input[["rok"]][2]))
  })
  
  statystyki_r <- reactive({
    filter(statystyki, Wskaznik == input[["wskaznikPlot"]])
  })
  
  observe({
    updateSelectizeInput(session, inputId = "kodStacji",
                         choices = lapply(split(statystyki_r()[, "NAZWA.STACJI"], statystyki_r()[, "Wojewodztwo"]), unique),
                         selected = input[["kodStacji"]])
  })
  
  output[["sredniaInfo"]] <- renderInfoBox({
    infoBox(title = "srednia wartosc pomiaru",
            value = ifelse(nrow(statystykiPM_r()) != 0, round(mean(statystykiPM_r()[, "Srednia"]), 2), "Brak pomiarow"),
            icon = icon("balance-scale"),
            color = "fuchsia")
  })
  
  output[["maxInfo"]] <- renderInfoBox({
    infoBox(title = "Maksymalny pomiar",
            value = ifelse(nrow(statystykiPM_r()) != 0, round(max(statystykiPM_r()[, "Maks"]), 2), "Brak pomiarow"),
            icon = icon("exclamation-circle"),
            color = "orange")
  })
  
  output[["liczbaInfo"]] <- renderInfoBox({
    infoBox(title = "Liczba pomiarow",
            value = sum(statystykiPM_r()[, "Liczba.pomiarow"]))
  })
  
  output[["plotTime"]] <- renderPlot({
    filter(statystyki_r(), NAZWA.STACJI %in% input[["kodStacji"]]) %>%
      ggplot(aes(x = Rok, y = Srednia, fill = NAZWA.STACJI)) +
      geom_bar(stat = "identity", position = "dodge")
  })
  
  output[["plotAmount"]] <- renderPlot({
    filter(statystyki_r(), NAZWA.STACJI %in% input[["kodStacji"]]) %>%
      ggplot(aes(x = Rok, y = Liczba.pomiarow, fill = NAZWA.STACJI)) +
      geom_bar(stat = "identity", position = "dodge")
  })
  
  output[["aboutText"]] <- renderText({
    "W sklad aplikacji wchodza trzy moduly: pierwszym z nich jest widoczny tutaj modul tekstowy, prezentujacy zawartosc aplikacji.
    Drugi z modulow zawiera mozliwosc wyswietlenia kluczowych informacji liczbowych w formie przystepnych infografik, natomiast w trzeciej
    zakladce dostepne sa dwa wykresy, prezentujace dane w zaleznosci od czasu dla wybranych miejsc oraz substancji. Aplikacja pobiera badz
    nie prywatne dane przegladania uzytkownikow."
  })
}

shinyApp(ui = ui, server = server)
