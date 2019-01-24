library(shiny)
library(shinydashboard)
library(ggplot2)
library(data.table)
library(ggiraph)

ui <- dashboardPage(
  
  skin = "black",
  ###############################
  dashboardHeader(title = "Projekt JA"),
  #################################
  dashboardSidebar(
    sidebarUserPanel(Sys.info()[["effective_user"]],
                     subtitle = a(href = "#", icon("circle", class = "text-success"), "Online")
    ),
    sidebarMenu(
      # Setting id makes input$tabs give the tabName of currently-selected tab
      id = "tabs",
      menuItem("Kroki", tabName = "dashboard1", icon = icon("dashboard")),
      menuItem("Kroki a sen", tabName = "dashboard2", icon = icon("dashboard")),
      menuItem("Sen", tabName = "dashboard3", icon=icon("dashboard")),
      menuItem("Napoje", tabName = "dashboard4", icon = icon("dashboard")),
      menuItem("About", icon = icon("info-circle"), tabName = "about", badgeLabel = "new",
               badgeColor = "green")
    )
  ),
  ###########################
  dashboardBody(
    tabItems(
      
      tabItem("dashboard1",
              fluidRow(
                height =20,
                column(width = 12,
                       box(title = "Piesze wycieczki",
                           solidHeader = TRUE,
                           width = 12,
                           HTML(
                             paste(
                               p("Poniższe wykresy opisują piesze wycieczki Wojtka. Dane pochodzą z przełomu grudnia 2018 roku i stycznia 2019 roku.")
                             )
                           )
                       )
                )
              ),
              fluidRow(
                height =20,
                column(width = 6,
                       box(title = "Ile kroków dziennie?",
                           solidHeader = TRUE,
                           collapsible = TRUE,
                           width = 12,
                           plotOutput(outputId="day_steps_plt", height = 600)
                       )
                ),
                
                column(width = 6,
                       box(title = "Jak duże są moje kroki?",
                           solidHeader = TRUE,
                           collapsible = TRUE,
                           width = 12,
                           plotOutput(outputId="day_step_length", height = 600)
                       )
                )
              ),
              fluidRow(
                column(width = 12,
                       box(title = "Piesze wycieczki",
                           solidHeader = TRUE,
                           collapsible = TRUE,
                           width = 12,
                           plotOutput(outputId="series_steps")
                       )
                )
              ),
              fluidRow(
                column(width = 12,
                       box(title = "Piesze wycieczki - długość kroku",
                           solidHeader = TRUE,
                           collapsible = TRUE,
                           width = 12,
                           plotOutput(outputId="series_step_length")
                       )
                )
              )
      ),
      ##################################
      tabItem("dashboard2",
              fluidRow(
                height =20,
                column(width = 12,
                       box(title = "Piesze wycieczki",
                           solidHeader = TRUE,
                           width = 12,
                           HTML(
                             paste(
                               p("Poniższe wykresy opisują zależność długości marszu od snu Wojtka. Dane pochodzą z przełomu grudnia 2018 roku i stycznia 2019 roku.")
                             )
                           )
                       )
                )
              ),
              fluidRow(
                column(width = 12,
                       box(title = "Kroki, sen a dzień",
                           solidHeader = TRUE,
                           collapsible = TRUE,
                           width = 12,
                           plotOutput(outputId="sleep_steps_density", height = 1000)
                       )
                )
              ),
              fluidRow(
                column(width = 6,
                       box(title = "Kroki",
                           solidHeader = TRUE,
                           collapsible = TRUE,
                           width = 12,
                           plotOutput(outputId="day_steps_violin")
                       )
                ),
                column(width = 6,
                       box(title = "Sen",
                           solidHeader = TRUE,
                           collapsible = TRUE,
                           width = 12,
                           plotOutput(outputId="day_sleep_violin")
                       )
                )
              )
      ),
      ########################
      tabItem("dashboard3",
              fluidRow(
                height =20,
                column(width = 12,
                       box(title = "Sen",
                           solidHeader = TRUE,
                           width = 12,
                           HTML(
                             paste(
                               p("Przedstawione w tej zakładce wykresy dotyczą statystyki długości naszego snu w ciągu ostatniego miesiąca.")
                             )
                           )
                       )
                )
              ),
              fluidRow(
                height =20,
                column(width = 12,
                       box(title = "Długość snu",
                           solidHeader = TRUE,
                           collapsible = TRUE,
                           width = 12,
                           HTML(
                             paste(
                               p("Wykres przedstawia dane od 13 grudnia do 7 stycznia. 
                                 Można zaobserwować dużą nieregularność długości snu, różnice między poszczególnymi osobami, a także szczególną datę 31 grudnia.")
                             )
                           ),
                           plotOutput(outputId="czas_snu_plt", height = 600)
                       )
                )
              ),
              fluidRow(
                column(width = 12,
                       box(title = "Ola",
                           solidHeader = TRUE,
                           collapsible = TRUE,
                           width = 12,
                           plotOutput(outputId="sen_Ola_plt")
                       )
                )),
              fluidRow(
                column(width = 12,
                       box(title = "Gosia",
                           solidHeader = TRUE,
                           collapsible = TRUE,
                           width = 12,
                           plotOutput(outputId="sen_Gosia_plt")
                       )
                )
              )
      ),
      #################################
      tabItem("dashboard4",
              fluidRow(
                height =20,
                column(width = 12,
                       box(title = "Pić znaczy żyć",
                           solidHeader = TRUE,
                           width = 12,
                           HTML(
                             paste(
                               p("Przedstawione poniżej wykresy dotyczą napojów wypitych przez Gosię Wachulec przez okres 30 dni. Dane pochodzą z przełomu grudnia 2018 roku i stycznia 2019 roku.")
                             )
                           )
                       )
                )
              ),
              fluidRow(
                height =20,
                column(width = 12,
                       box(title = "Co piję i w jakich ilościach?",
                           solidHeader = TRUE,
                           collapsible = TRUE,
                           width = 12,
                           HTML(
                             paste(
                               p("Ponad 70 litrów na napojów w 30 dni, lecz czego wypiłam najwięcej? Sprawdźmy. \nJedna kratka na wykresie to jeden procent wypitych przez miesiąc napojów.")
                             )
                           ),
                           ggiraphOutput("napoje")
                       )
                )
              ),
              fluidRow(
                height =20,
                column(width = 12,
                       box(title = "Kiedy spożywam dane napoje i w jakich ilościach?",
                           solidHeader = TRUE,
                           collapsible = TRUE,
                           width = 12,
                           HTML(
                             paste(
                               p("Do którego posiłku najchętniej piję kawę, a kiedy sięgam po wino? Na poniższym wykresie widać, kiedy piję dane napoje. Nazwy posiłków i pór dnia są ułożone od najwcześniejszych do najpoźniejszych, co oznacza, że góra wykresu pokazuje jakie napoje spożywam rano, a dół wykresu - co piję popołudniu i wieczorem. Dodatkowo wielkość punktów jest proporcjonalna do liczby spożytych przeze mnie napojów.")
                             )
                           ),
                           ggiraphOutput("posilki")
                       )
                )
              )
      ),
      #################################
      tabItem("about",
              fluidRow(
                height =20,
                column(width = 12,
                       box(title = "Me, my, mine",
                           solidHeader = TRUE,
                           width = 12,
                           HTML(
                             paste(
                               p("Ta aplikacja została utworzona w ramach drugiego projektu grupowego z przedmiotu Techniki Wizualizacji Danych na Politechnice Warszawskiej przez Wojciecha Kretowicza, Małgorzatę Wachulec i Aleksandrę Wichrowską. Wybrany przez nas temat to 'Projekt Ja' i w tej aplikacji przedstawiamy nasze dane dotyczące liczby kroków oraz przebytego przez nas dystans, czasu oraz godzin naszego snu, a także spożywanych przez nas napojów."),'<br/>',
                               p("Zapraszamy!")
                             )
                           )
                       )
                )
              )
      )
    )
  )
  #########################
)

server <- function(input, output) {
  
  output[["day_steps_plt"]] <- renderPlot(
    day_steps_plt
    )
  
  output[["day_step_length"]] <- renderPlot(
    day_step_length
  )
  
  output[["series_step_length"]] <- renderPlot(
    series_step_length
  )
  
  output[["series_steps"]] <- renderPlot(
    series_steps
  )
  
  output[["sleep_steps_density"]] <- renderPlot(
    sleep_steps_density
  )
  
  output[["day_steps_violin"]] <- renderPlot(
    day_steps_violin
  )
  
  output[["day_sleep_violin"]] <- renderPlot(
    day_sleep_violin
  )

  sen_Wojtek <- c(8,7,6,8,6,6,7,7,8,10,10,8,9,10,8,4,9,9,0,9,7,10,10,9,8,8)
  
  sen_Gosia_start <- c(23.5,24,24,23,23,23.5,23,23.5,1.5,24,23.5,24,0.5,24,24,23.5,24,3,2,23,23.5,23.5,23.5,23.5,24,23)
  sen_Gosia_koniec <- c(7.5,7,8,7,7,7,7,7,7,8,9,8.5,10,9,8.5,9.5,9,8,3.5,8,7,7,8,8,8,7)
  sen_Gosia <- (sen_Gosia_koniec - sen_Gosia_start) %% 24
  
  sen_Ola_start <- c(2,2,7,2,1,2,24,4,23,2,3,2,3,3,4,3,2,3,4.5,3,2.5,2.5,4,2,3.5,24)
  sen_Ola_koniec <- c(7,10,12,11,10,9,8,8,11,10,9,10,11,10,10,10,10,12,14,9,8,9,12,9,9,10)
  sen_Ola <- (sen_Ola_koniec-sen_Ola_start) %% 24
  
  daty <- rep(seq(as.Date("2018-12-13"), by=1, len=26),3)
  sen <- data.frame(daty,c(sen_Wojtek, sen_Gosia, sen_Ola),c(rep("Wojtek",26),rep("Gosia",26),rep("Ola",26)))
  colnames(sen) <- c("data", "czas", "osoba")
  
  output[["czas_snu_plt"]] <- renderPlot(
    ggplot() + 
      geom_line(data=sen, aes(x=data,y=czas, col=osoba), size=1) + geom_point(data=sen, aes(x=data,y=czas, col=osoba), size=2) +
      labs (title = 'Długość snu w zależności od daty', x='Data', y='Długość snu') +
      scale_y_continuous(breaks=seq(0,12,2), labels=seq(0,12,2), limits = c(0,12)) +
      theme(plot.title = element_text(hjust = 0.5, size=20),
            axis.title.x = element_text(size=12),
            axis.title.y = element_text(size=12),
            axis.text = element_text(size=10))
  )
  
  sen2 <- data.frame(seq( as.Date("2018-12-13"), by=1, len=26), sen_Ola, sen_Ola_start, sen_Ola_koniec)
  colnames(sen2) <- c('date', 'czas', 'start', 'end')
  sen2$day <- weekdays(strptime(sen2$date, "%Y-%m-%d"))
  sen2$ymin <- seq(from=1, length.out = 26, by=1.2)
  t <- table(sen2$day)
  sen2$start <- (sen2$start + 4 ) %% 24
  sen2$end <- (sen2$end + 4 ) %% 24
  
  output[["sen_Ola_plt"]] <- renderPlot(
    ggplot(sen2, aes(xmin = start, xmax = end, ymin = ymin, ymax = ymin + 1)) + geom_rect() +
      xlab("Pora dnia (godzina)") + scale_x_continuous(breaks=seq(0,24,1),labels=(seq(0,24,1)-4) %% 24, limits = c(3,18)) +
      scale_y_continuous(breaks=sen2$ymin[seq(1,26,2)], labels=sen2$date[seq(1,26,2)]) + labs(title="Wykres snu w ciągu dnia") +
      theme(plot.title = element_text(hjust = 0.5, size=20),
            axis.title.x = element_text(size=12),
            axis.text = element_text(size=10))
  )
  
  sen3 <- data.frame(seq( as.Date("2018-12-13"), by=1, len=26), sen_Gosia, sen_Gosia_start, sen_Gosia_koniec)
  colnames(sen3) <- c('date', 'czas', 'start', 'end')
  sen3$day <- weekdays(strptime(sen3$date, "%Y-%m-%d"))
  sen3$ymin <- seq(from=1, length.out = 26, by=1.2)
  t <- table(sen3$day)
  sen3$start <- (sen3$start + 4 ) %% 24
  sen3$end <- (sen3$end + 4 ) %% 24
  
  
  output[["sen_Gosia_plt"]] <- renderPlot(
    ggplot(sen3, aes(xmin = start, xmax = end, ymin = ymin, ymax = ymin + 1)) + geom_rect() +
      xlab("Pora dnia (godzina)") + scale_x_continuous(breaks=seq(0,24,1),labels=(seq(0,24,1)-4) %% 24, limits = c(3,18)) +
      scale_y_continuous(breaks=sen3$ymin[seq(1,26,2)], labels=sen3$date[seq(1,26,2)]) + labs(title="Wykres snu w ciągu dnia") +
      theme(plot.title = element_text(hjust = 0.5, size=20),
            axis.title.x = element_text(size=12),
            axis.text = element_text(size=10))
  )
  
  output[["napoje"]] <- renderggiraph({
    df <- read.csv2("DataFrameNapoje.csv")
    df$category <- as.character(df$category)
    df$category <- factor(df$category, levels=c("Herbata","Woda","Kawa", "Wino","Mocny Alkohol"))
    g <- ggplot(df, aes(x = x, y = y, fill = category)) + 
      geom_tile_interactive(aes(width = 1, height = 1,tooltip = df$text),color = "black", size = 0.2)+
      scale_x_continuous(expand = c(0, 0)) +
      scale_y_continuous(expand = c(0, 0), trans = 'reverse') +
      labs(title="Procentowy udział napojów", subtitle="Najedź na daną sekcję, żeby zobaczyć dokładne wartości") +
      scale_fill_manual(values=c("lightcyan","darkolivegreen1", "lightyellow","lightsalmon","lightpink")) +
      theme(plot.title = element_text(size = rel(1.6)),
            axis.text = element_blank(),
            axis.title = element_blank(),
            axis.ticks = element_blank(),
            legend.title = element_blank(),
            legend.position = "right")
    ggiraph(code = print(g))
  }) 
  
  output[["posilki"]] <- renderggiraph({
    picie3 <- read.csv2("Picie2.csv")
    picie3 <- picie3[,1:3]
    names(picie3) <- c("kiedy","co","ile")
    picie3$kiedy <- as.character(picie3$kiedy)
    picie3$kiedy <- factor(picie3$kiedy, levels=c("Wieczorem","Obiad","Lunch", "Bez posilku","Cos slodkiego","Sniadanie"))
    picie3$co <- as.character(picie3$co)
    picie3$co <- factor(picie3$co, levels=c("Herbata","Woda","Kawa", "Wino","Alkohol mocny"))
    
    gdot <- ggplot(picie3,aes(x=co,y=kiedy,fill=co))+
      
      geom_point_interactive(stat='identity',tooltip = picie3$ile,shape = 21, colour = "black", size = picie3$ile) +
      guides(fill=FALSE) +
      theme_minimal()+
      scale_fill_manual(values=c("lightcyan","darkolivegreen1", "lightyellow","lightsalmon","lightpink")) +
      scale_y_discrete(expand = c(0.08,0,0.15,0)) +
      labs(title="Napoje a posiłki",subtitle="Najedź na punkt, żeby zobaczyć ile jednostek wypiłam miesięczne")+
      theme(plot.title = element_text(size = rel(1.6)),
            plot.subtitle = element_text(margin = margin(t = 0, r = 0, b = 20, l = 0)),
            axis.title = element_blank(),
            legend.position = "right")
    girafe(code = print(gdot))
  })
  
}

shinyApp(ui, server)