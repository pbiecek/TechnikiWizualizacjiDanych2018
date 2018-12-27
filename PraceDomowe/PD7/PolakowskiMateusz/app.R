library(shiny)
library(shinydashboard)
library(reshape2)
library(ggplot2)
library(data.table)
library(openxlsx)

### Pre-dashboard stuff
wojewodztwa <- c("małopolskie", "łódzkie", "śląskie", "pomorskie", "zachodniopomorskie",
                 "kujawsko-pomorskie", "lubelskie", "mazowieckie", "dolnośląskie",
                 "podkarpackie", "wielkopolskie", "opolskie", "podlaskie", 
                 "warmińsko-mazurskie", "lubuskie", "świętokrzyskie")
wskazniki <- c("SO2", "NO2", "NOx", "CO", "O3", "C6H6", "PM10", "PM2.5", "Pb(PM10)",
               "As(PM10)", "Cd(PM10)", "Ni(PM10)", "BaP(PM10)")

tab_pm25 <- as.data.table(read.xlsx("Statystyki.xlsx", sheet = 8))[
  , .(mean(Średnia)), by = .(Województwo, Nazwa.strefy, Rok)]

tab_so2 <- as.data.table(na.omit(read.xlsx("Statystyki.xlsx", sheet = 1)))[
  , .(mean(Śr..zimowa), mean(Średnia)), by = .(Województwo, Rok)]
colnames(tab_so2) <- c("Wojewodztwo", "Rok", "Srednia_zima", "Srednia_niezima")
srednia <- tab_so2[, .(mean(Srednia_niezima)), by = .(Rok)]
tab_so2$Srednia_niezima <- tab_so2$Srednia_niezima - (tab_so2$Srednia_zima - tab_so2$Srednia_niezima)

zima <- tab_so2[, .(Wojewodztwo, Rok, Srednia_zima)]
zima$Wojewodztwo <- paste(zima$Wojewodztwo, "_zima", sep = "")
colnames(zima) <- c("Wojewodztwo", "Rok", "Srednia")
niezima <- tab_so2[, .(Wojewodztwo, Rok, Srednia_niezima)]
niezima$Wojewodztwo <- paste(niezima$Wojewodztwo, "_niezima", sep = "")
colnames(niezima) <- c("Wojewodztwo", "Rok", "Srednia")
tab_so2 <- rbind(
  as.data.frame(niezima),
  as.data.frame(zima)
)

tab_pm25_2 <- as.data.table(read.xlsx("Statystyki.xlsx", sheet = 8))[
  , .(mean(Min), mean(Średnia), mean(Maks)), by = .(Województwo, Rok)]
colnames(tab_pm25_2) <- c("Wojewodztwo", "Rok", "min", "sr", "max")



### User Interface
ui <- dashboardPage(
  skin = "black",
  dashboardHeader(title = "Powietrze w Polsce",
                  dropdownMenu(type = "notifications", badgeStatus = "warning",
                               notificationItem(icon = icon("exclamation-triangle"), status = "info",
                                                "Uwaga! Zanieczyszczone powietrze!!!"
                               )
                  )),
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
      
      ### DASHBOARD
      tabItem("dashboard",
              tabsetPanel(
                tabPanel(div("Zanieczyszczenia ogólnie", style = "font-family:Arial"),
                         h1("Zanieczyszczenia ogólnie", style = "font-family:Arial"),
                         br(),
                         div("Poniżej zawarty jest wykres ogólny stężenia wybranego wskaźnika według województw - jego obecność 
                                        w każdym województwie przez lata pomiarów.",
                             style = "font-family:Arial;font-size:20px"),
                         div("Zauważmy, że pokazywane są średnie wyciągane z całego roku dla stacji pomiarowych. Stąd wyniki 
                             są 'obniżone' ze względu na mniejszą emisję pyłów i gazów w ciągu miesięcy ciepłych.",
                             style = "font-family:Arial;font-size:20px"),
                         div("W kolejnych zakładkach zobaczymy, że wrażenie 'nie jest aż tak źle' jako wniosek z poniższej 
                             grafiki jest niebezpiecznie mylne.",
                             style = "font-family:Arial;font-size:20px"),
                         br(),
                         sidebarLayout(
                           sidebarPanel(selectInput(inputId = "wskaznik",
                                                    label = div("Wybierz interesujący Cię wskaźnik", style = "font-family:Arial"),
                                                    choices = wskazniki,
                                                    selected = "SO2"),
                                        hr(),
                                        checkboxGroupInput(inputId = "wojewodztwo1",
                                                           label = div("Wybierz interesujące Cię województwa", style = "font-family:Arial"),
                                                           choices = wojewodztwa,
                                                           selected = "łódzkie"),
                                        hr(),
                                        downloadButton(outputId = "downloadPlot1.png",
                                                       label = "Pobierz wykres")),
                           mainPanel(br(),
                                     plotOutput("powietrze_ov", height = 590),
                                     br())
                         )),
                tabPanel(div("Polskie PM2.5 w tabeli", style = "font-family:Arial"),
                         h1("Polskie PM2.5 w tabeli", style = "font-family:Arial"),
                         br(),
                         div("Warto przyjrzeć się i porównać województwa nie tylko na wykresie liniowym. 
                             W tej zakładce obejrzmy wybrane uśrednione wartości pomiarów.",
                             style = "font-family:Arial;font-size:20px"),
                         div("Kolor intuicyjnie oznacza intensywność występowania pyłków PM2.5 na przestrzeni lat - łatwo 
                             można wyróżnić gorsze jakościowo województwa od lepszych i też nietrudno jest to uzasadnić. 
                             Mimo wszystko różnica między niektórymi może wydawać się nieprawdopodobna.",
                             style = "font-family:Arial;font-size:20px"),
                         br(),
                         sidebarLayout(
                           sidebarPanel(radioButtons(inputId = "po_czym_usredniac",
                                                     label = div("Po jakich wartościach uśredniać?", style = "font-family:Arial"),
                                                     choices = c("minimalnych", "średnich", "maksymalnych"),
                                                     selected = "średnich"),
                                        hr(),
                                        downloadButton(outputId = "downloadPlot4.png",
                                                       label = "Pobierz wykres")),
                           mainPanel(br(),
                                     plotOutput("plot4", height = 550),
                                     br())
                         )),
                tabPanel(div("Powszechne przekraczanie norm", style = "font-family:Arial"),
                         h1("Powszechne przekraczanie norm", style = "font-family:Arial"),
                         br(),
                         div("Po wprowadzeniu i swobodnym przeanalizowaniu stanu każdego województwa przejdźmy dalej.
                             Dla nas - ludzi - największym niebezpieczeństwem są pyłki PM2.5.", 
                             style = "font-family:Arial;font-size:20px"),
                         div("Skupmy się zatem własnie na nich i zobaczmy jak bardzo w Polsce przekraczane są normy roczne.",
                             style = "font-family:Arial;font-size:20px"),
                         br(),
                         sidebarLayout(
                           sidebarPanel(radioButtons(inputId = "wojewodztwo2",
                                                     label = div("Wybierz interesujące Cię województwo", style = "font-family:Arial"),
                                                     choices = wojewodztwa,
                                                     selected = "łódzkie"),
                                        hr(),
                                        checkboxInput(inputId = "pokaz_norme",
                                                      label = div("Pokaż obowiązującą normę", style = "font-family:Arial"),
                                                      value = FALSE),
                                        hr(),
                                        downloadButton(outputId = "downloadPlot2.png",
                                                       label = "Pobierz wykres")),
                           mainPanel(br(),
                                     plotOutput("pm25", height = 550),
                                     br())
                         )),
                tabPanel(div("Lato zaniża średnią", style = "font-family:Arial"),
                         h1("Lato zaniża średnią", style = "font-family:Arial"),
                         br(),
                         div("Jak wspomniane w pierwszej zakładce - lato jest czasem mniejszej emisji pyłów i gazów. Zajmijmy się zatem jednym z bardziej
                             niebezpiecznych, oznaczonym jako SO2 - dwutlenkiem siarki. Jest to bezbarwny gaz silnie drażniący drogi oddechowe, trujący dla zwierząt i szkodliwy dla roślin oraz, 
                             niestety, składnikiem wszechobecnego w Polsce smogu.", 
                             style = "font-family:Arial;font-size:20px"),
                         div("Poniżej zobaczyć można różnicę w jego emisji podczas miesięcy ciepłych a zimnych.",
                             style = "font-family:Arial;font-size:20px"),
                         br(),
                         sidebarLayout(
                           sidebarPanel(radioButtons(inputId = "wojewodztwo3",
                                                     label = div("Wybierz interesujące Cię województwo", style = "font-family:Arial"),
                                                     choices = wojewodztwa,
                                                     selected = "śląskie"),
                                        hr(),
                                        checkboxInput(inputId = "pokaz_srednia",
                                                      label = div("Pokaż wygładzoną średnią wyników z całej Polski", style = "font-family:Arial"),
                                                      value = TRUE),
                                        hr(),
                                        downloadButton(outputId = "downloadPlot3.png",
                                                       label = "Pobierz wykres")),
                           mainPanel(br(),
                                     plotOutput("so2", height = 550),
                                     br())
                         ))
                )
      ),
      
      ### About
      tabItem("Informacje",
              div("Niniejszy dashboard jest wizualizacją pewnych ciekawych, aczkolwiek zatrważających faktów dotyczących stanu polskiego powietrza.", 
                  style = "font-family:Arial;font-size:20px"),
              div("Na przestrzeni lat możemy stwierdzić że pogarsza się on z miesiąca na miesiąc, w szczególności w trakcie zimniejszego okresu roku.", 
                  style = "font-family:Arial;font-size:20px"),
              br(), br(), br(),
              div("Autor: Mateusz Polakowski.", 
                  style = "font-family:Arial;font-size:15px"),
              div("Dashboard zrealizowany jako praca domowa z przedmiotu Techniki Wizualizacji Danych 2018/2019 MiNI PW.", 
                  style = "font-family:Arial;font-size:15px")
      )
    )
  )
)



### Server
server <- function(input, output) {
  
  ### Reactives
  # Page 1, can't speed up, depends on sheet chosen to visualise
  powietrze_ov <- reactive({
    tab <- as.data.table(read.xlsx("Statystyki.xlsx", sheet = match(input$wskaznik, wskazniki)))[
      , .(mean(Średnia)), by = .(Województwo, Rok)
      ]
    colnames(tab) <- c("Wojewodztwo", "Rok", "Wartosc")
    tab <- tab[Wojewodztwo %in% input$wojewodztwo1]
  })
  
  plot1 <- reactive({
    plot1 <- powietrze_ov()
    
    pl1 <- ggplot(data = plot1, aes(x = Rok, y = as.numeric(as.character(Wartosc)), color = Wojewodztwo)) +
      geom_line(size = 2) +
      scale_x_continuous(limits = c(min(plot1$Rok), max(plot1$Rok)),
                         breaks = min(plot1$Rok):max(plot1$Rok),
                         expand = c(0.01, 0.01)) +
      labs(y = "Średnia wartość stężenia w µg/m3\n",
           title = paste("Stężenie wskaźnika", input$wskaznik), 
           subtitle = "w poszczególnych województwach") +
      theme_bw() +
      theme(plot.title = element_text(size = 20, face = "bold"),
            plot.subtitle = element_text(size = 13),
            axis.title = element_text(size = 16),
            legend.title = element_text(size = 14))
  })
  
  # Page 2
  pm25 <- reactive({
    tab <- tab_pm25[Województwo == input$wojewodztwo2]
    colnames(tab) <- c("Wojewodztwo", "Strefa", "Rok", "Wartosc")
    tab <- tab[order(Rok, -Wartosc)][
      , head(.SD, 3), by = .(Rok)]
  })
  
  plot2 <- reactive({
    plot2 <- pm25()
    
    if(input$pokaz_norme == TRUE) {
      pl2 <- ggplot(data = plot2, aes(x = Rok, y = Wartosc, color = Strefa)) +
        geom_point(size = 5) +
        scale_x_continuous(limits = c(min(plot2$Rok), max(plot2$Rok)),
                           breaks = min(plot2$Rok):max(plot2$Rok),
                           expand = c(0.03, 0.03)) +
        scale_y_continuous(limits = c(5, 50), breaks = seq(5, 50, 5)) +
        labs(y = "Średnia wartość stężenia w µg/m3\n",
             title = paste("Niechlubne top 3 stref"),
             subtitle = paste("w województwie:", input$wojewodztwo2)) +
        theme_bw() +
        theme(plot.title = element_text(size = 20, face = "bold"),
              plot.subtitle = element_text(size = 13),
              axis.title = element_text(size = 16),
              legend.title = element_text(size = 14)) +
        geom_line(aes(y = 10), size = 2, color = 'orange') +
        geom_label(aes(min(plot2$Rok + 1), 12), label = "Obowiązująca norma", color = 'black')
    } else {
      pl2 <- ggplot(data = plot2, aes(x = Rok, y = Wartosc, color = Strefa)) +
        geom_point(size = 5) +
        scale_x_continuous(limits = c(min(plot2$Rok), max(plot2$Rok)),
                           breaks = min(plot2$Rok):max(plot2$Rok),
                           expand = c(0.03, 0.03)) +
        scale_y_continuous(limits = c(5, 50), breaks = seq(5, 50, 5)) +
        labs(y = "Średnia wartość stężenia w µg/m3\n",
             title = paste("Niechlubne top 3 stref"),
             subtitle = paste("w województwie:", input$wojewodztwo2)) +
        theme_bw() +
        theme(plot.title = element_text(size = 20, face = "bold"),
              plot.subtitle = element_text(size = 13),
              axis.title = element_text(size = 16),
              legend.title = element_text(size = 14))
    }
  })
  
  # Page 3
  so2plot3 <- reactive({
    tab <- tab_so2
    
    zima <- tab[tab$Wojewodztwo %like% paste("^", input$wojewodztwo3,"_zima", sep = ""), ]
    niezima <- tab[tab$Wojewodztwo %like% paste("^", input$wojewodztwo3,"_niezima", sep = ""), ]
    
    if(input$pokaz_srednia == TRUE) {
      ggplot() +
        geom_label(aes(x = 2016, y = 9, label = "Wygładzona średnia\nwyników z całej Polski"), color = "orange") +
        geom_smooth(data = srednia, aes(x = Rok, y = V1), span = 0.5, level = 0.5, size = 1.5, color = "orange", fill = 'orange') +
        geom_line(data = zima, aes(x = Rok, y = Srednia, color = Wojewodztwo), size = 1) +
        geom_point(data = zima, aes(x = Rok, y = Srednia, color = Wojewodztwo), size = 4) +
        geom_line(data = niezima, aes(x = Rok, y = Srednia, color = Wojewodztwo), size = 1) +
        geom_point(data = niezima, aes(x = Rok, y = Srednia, color = Wojewodztwo), size = 4) +
        scale_x_continuous(limits = c(min(niezima$Rok), max(niezima$Rok)),
                           breaks = min(niezima$Rok):max(niezima$Rok),
                           expand = c(0.03, 0.03)) +
        scale_y_continuous(limits = c(0, 55),
                           breaks = seq(0, 55, by = 5),
                           expand = c(0.02, 0.02)) +
        labs(y = "Średnia wartość stężenia w µg/m3\n", x = "Rok",
             title = paste("Różnica między okresem zimowym a niezimowym"),
             subtitle = "z podziałem na województwa") +
        theme_bw() +
        theme(plot.title = element_text(size = 20, face = "bold"),
              plot.subtitle = element_text(size = 13),
              axis.title = element_text(size = 16),
              legend.title = element_text(size = 14))  
    } else {
      ggplot() +
        geom_line(data = zima, aes(x = Rok, y = Srednia, color = Wojewodztwo), size = 1) +
        geom_point(data = zima, aes(x = Rok, y = Srednia, color = Wojewodztwo), size = 4) +
        geom_line(data = niezima, aes(x = Rok, y = Srednia, color = Wojewodztwo), size = 1) +
        geom_point(data = niezima, aes(x = Rok, y = Srednia, color = Wojewodztwo), size = 4) +
        scale_x_continuous(limits = c(min(niezima$Rok), max(niezima$Rok)),
                           breaks = min(niezima$Rok):max(niezima$Rok),
                           expand = c(0.03, 0.03)) +
        scale_y_continuous(limits = c(0, 55),
                           breaks = seq(0, 55, by = 5),
                           expand = c(0.02, 0.02)) +
        labs(y = "Średnia wartość stężenia w µg/m3\n", x = "Rok",
             title = paste("Różnica między okresem zimowym a niezimowym"),
             subtitle = "z podziałem na województwa") +
        theme_bw() +
        theme(plot.title = element_text(size = 20, face = "bold"),
              plot.subtitle = element_text(size = 13),
              axis.title = element_text(size = 16),
              legend.title = element_text(size = 14))
    }
  })

  # Page 4  
  plot4 <- reactive({
    if(input$po_czym_usredniac == 'minimalnych') {
      ggplot(data = tab_pm25_2, aes(x = as.character(Rok), y = Wojewodztwo, fill = min)) + 
        geom_bin2d() +
        scale_fill_gradientn(colors = c("white", "#ffffb2", "#fecc5c", "#fd8d3c", "#f03b20", "#bd0026")) +
        labs(title = "Jakość powietrza w Polsce", subtitle = 'średnie wartości po minimalnych wynikach województw',
             x = "Rok", y = "Województwo") +
        theme_bw() +
        theme(plot.title = element_text(size = 20, face = "bold"),
              axis.text.y = element_text(size = 12, face = "bold"),
              axis.title = element_text(size = 15),
              title = element_text(size = 16)) +
        guides(fill = guide_legend(title = " µg/m3"))
    } else if(input$po_czym_usredniac == 'maksymalnych') {
      ggplot(data = tab_pm25_2, aes(x = as.character(Rok), y = Wojewodztwo, fill = max)) + 
        geom_bin2d() +
        scale_fill_gradientn(colors = c("white", "#ffffb2", "#fecc5c", "#fd8d3c", "#f03b20", "#bd0026")) +
        labs(title = "Jakość powietrza w Polsce", subtitle = 'średnie wartości po maksymalnych wynikach województw',
             x = "Rok", y = "Województwo") +
        theme_bw() +
        theme(plot.title = element_text(size = 20, face = "bold"),
              axis.text.y = element_text(size = 12, face = "bold"),
              axis.title = element_text(size = 15),
              title = element_text(size = 16)) +
        guides(fill = guide_legend(title = " µg/m3"))
    } else {
      ggplot(data = tab_pm25_2, aes(x = as.character(Rok), y = Wojewodztwo, fill = sr)) + 
        geom_bin2d() +
        scale_fill_gradientn(colors = c("white", "#ffffb2", "#fecc5c", "#fd8d3c", "#f03b20", "#bd0026")) +
        labs(title = "Jakość powietrza w Polsce", subtitle = 'średnie wartości po średnich wynikach województw',
             x = "Rok", y = "Województwo") +
        theme_bw() +
        theme(plot.title = element_text(size = 20, face = "bold"),
              axis.text.y = element_text(size = 12, face = "bold"),
              axis.title = element_text(size = 15),
              title = element_text(size = 16)) +
        guides(fill = guide_legend(title = "µg/m3")) 
    }
  })
  
  
  ### Plots rendering
  # Page 1
  output$powietrze_ov <- renderPlot({
    pl <- plot1()
    pl
  })
  
  # Page 2
  output$pm25 <- renderPlot({
    pl <- plot2()
    pl
  })
  
  # Page 3
  output$so2 <- renderPlot({
    pl <- so2plot3()
    pl
  })
  
  # Page 4
  output$plot4 <- renderPlot({
    pl <- plot4()
    pl
  })
  
  
  
  ### Downloads
  # Page 1
  output$downloadPlot1.png <- downloadHandler(
    filename = function() { "plot1.png" },
    content = function(file) {
      ggsave(file, plot = plot1(), device = "png", width = 14, height = 10)
    }
  )
  
  # Page 2
  output$downloadPlot2.png <- downloadHandler(
    filename = function() { "plot2.png" },
    content = function(file) {
      ggsave(file, plot = plot2(), device = "png", width = 14, height = 10)
    }
  )
  
  # Page 3
  output$downloadPlot3.png <- downloadHandler(
    filename = function() { "plot3.png" },
    content = function(file) {
      ggsave(file, plot = so2plot3(), device = "png", width = 14, height = 10)
    }
  )
  
  # Page 4
  output$downloadPlot4.png <- downloadHandler(
    filename = function() { "plot4.png" },
    content = function(file) {
      ggsave(file, plot = plot4(), device = "png", width = 14, height = 10)
    }
  )
}



### Shiny App
shinyApp(ui, server)