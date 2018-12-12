library(shiny)
library(shinydashboard)
library(reshape2)
library(dplyr)
library(openxlsx)
library(ggplot2)
library(data.table)


dane <- as.data.table(read.xlsx("Statystyki_2000-2018_wer20180828.xlsx"))[, .(mean(Sr..zimowa)), by = .(Nazwa.strefy, Rok)]
colnames(dane) <- c("Strefa", "Rok", "Wartosc")
dane1 <- na.omit(dane[Rok == as.numeric(2017), ][order(as.numeric(as.character(Wartosc)))][1:10, ])
dane2 <- na.omit(dane[Rok == as.numeric(2017), ][order(-as.numeric(as.character(Wartosc)))][1:10, ])
dane3 <- as.data.table(read.xlsx("Statystyki_2000-2018_wer20180828.xlsx"))
dane3 <- na.omit(dane3[Rok == as.numeric(2017), ])
Roznica <- dane3$Maks - dane3$Min
dane3 <- cbind(dane3, Roznica)
dane3 <- dane3[c(77:90), ]
dane3 <- dane3[, c("Kod.stacji","Min", "Maks", "Roznica")]


ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "TWD 2018/19"),
  dashboardSidebar(
    sidebarUserPanel(Sys.info()[["effective_user"]],
                     subtitle = a(href = "#", icon("circle", class = "text-success"), "Online")
    ),
    sidebarMenu(
      id = "zakladki",
      menuItem("Informancje", icon = icon("info-circle"), tabName = "Informacje"),
      menuItem("Wykres 1", tabName = "Wykres1", icon = icon("dashboard")),
      menuItem("Wykres 2", tabName = "Wykres2", icon = icon("dashboard")),
      menuItem("Wykres 3", tabName = "Wykres3", icon = icon("dashboard")),
      menuItem("Wnioski", icon = icon("info-circle"), tabName = "Wnioski")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem("Informacje",
              "Dashboard pokazujący różne zależności zanieczyczenia powietrza w Polsce w roku 2017.
              Na pierwszym wykresie pokazane są najlepsze pod względem stężenia dwutlenku siarki miejsca w Polsce.
              Na drugim zaprezentowane są najgorsze miejsca w Polsce pod tym samym względem.
              Natomiast na trzecim wykresie pokazane jest zróżnicowanie maksymalnych odczytów z województwa pomorskiego."
      ),
      tabItem("Wykres1",
              plotOutput("wykres1")
      ),
      tabItem("Wykres2",
              plotOutput("wykres2")
      ),
      tabItem("Wykres3",
              plotOutput("wykres3")
      ),
      tabItem("Wnioski",
              "W tej zakładce znajduje się pare zdań omówienia każdgo z wykresów.
              Na pierwszym wykresie pokazane są najlepsze stacje badawcze w Polsce, pod względem stężenia dwutlenku siarki.
              Na drugi, wykresie pokazane są najgorsze stacje badawcze w Polsce, pod względem stężenia dwutlenku siarki.
              Łatwo można zauważyć, że najczyściej jest w województwach pomorskich: pomorskim, zachodnio-pomorskim oraz kujawsko-pomorskim,
              natomiast na południu Polski sytuacja jest znacznie gorsza. Po obejrzeniu pierwszego i drugie wykresu przychodzi czas na dokładniejsze
              przyjrzenie się województwu pomorskiem, czyli jednemu z czystrzych w Polsce. Od razu widać, że pomiary są bardzo zróżnicowane, są miejsca
              'czyste' jaki i 'brudne'
              "
      )
    )
  )
)

server <- function(input, output) {
  output[["wykres1"]] <- renderPlot(
    ggplot(data = dane1, aes(x = reorder(Strefa, -as.numeric(as.character(Wartosc))), y = as.numeric(as.character(Wartosc)))) + 
      geom_col(fill = "red") +
      scale_y_continuous(limits = c(0, 30)) +
      theme_dark() +
      labs(title = "Najlepsze miejsca w Polsce", x = "Miejsce", y = "Stężenie SO2") +
      geom_text(aes(label = round(Wartosc, 1)), size = 4, hjust = -0.1, color = "white") +
      theme(panel.background = element_rect(fill = "black"),
            plot.background = element_rect(fill = "black"),
            title = element_text(colour = "white"),
            axis.text.x = element_text(colour = "white", size = 11),
            axis.title.y = element_text(colour = "white"),
            axis.text.y = element_text(colour = "white", size = 11),
            axis.title.x = element_text(colour = "white"),
            panel.grid.major = element_line(colour = "gray20"),
            panel.grid.minor = element_line(colour = "gray20"),
            plot.margin = unit(c(2, 1, 2, 1), "lines")) +
      coord_flip()
  )
  output[["wykres2"]] <- renderPlot(
    ggplot(data = dane2, aes(x = reorder(Strefa, as.numeric(as.character(Wartosc))), y = as.numeric(as.character(Wartosc)))) + 
      geom_col(fill = "red") +
      scale_y_continuous(limits = c(0, 30)) +
      theme_dark() +
      labs(title = "Najgorsze miejsca w Polsce", x = "Miejsce", y = "Stężenie SO2") +
      geom_text(aes(label = round(Wartosc, 1)), size = 4, hjust = -0.1, color = "white") +
      theme(panel.background = element_rect(fill = "black"),
            plot.background = element_rect(fill = "black"),
            title = element_text(colour = "white"),
            axis.text.x = element_text(colour = "white", size = 11),
            axis.title.y = element_text(colour = "white"),
            axis.text.y = element_text(colour = "white", size = 11),
            axis.title.x = element_text(colour = "white"),
            panel.grid.major = element_line(colour = "gray20"),
            panel.grid.minor = element_line(colour = "gray20"),
            plot.margin = unit(c(2, 1, 2, 1), "lines")) +
      coord_flip()
  )
  output[["wykres3"]] <- renderPlot(
    ggplot(data = dane3) + geom_col(aes(x = Kod.stacji, y = Maks), fill = "red") + coord_flip() +
      labs(y = "Maksymalna wartość ze stacji", x = "Unikalny kod stacji",
           title = "Różnica poziomu zanieczyszczenia powietrza na pomorzu") +
      theme(panel.background = element_rect(fill = "black"),
            plot.background = element_rect(fill = "black"),
            title = element_text(colour = "white"),
            axis.text.x = element_text(colour = "white", size = 11),
            axis.title.y = element_text(colour = "white"),
            axis.text.y = element_text(colour = "white", size = 11),
            axis.title.x = element_text(colour = "white"),
            panel.grid.major = element_line(colour = "gray20"),
            panel.grid.minor = element_line(colour = "gray20"),
            plot.margin = unit(c(2, 2, 2, 2), "lines")) +
      scale_y_continuous(limits = c(0,  210)) +
      geom_text(aes(x = Kod.stacji, y = Maks, label = round(Maks, 1)), size = 4, hjust = -0.1, color = "white")
  )
}

shinyApp(ui, server)