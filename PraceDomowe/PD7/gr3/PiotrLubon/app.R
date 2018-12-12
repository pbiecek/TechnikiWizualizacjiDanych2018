library(shiny)
library(shinydashboard)
library(reshape2)
library(dplyr)
library(ggplot2)
library(scales)

nazwyMiesiecy <- list('01'='Styczeń', '02'='Luty', '03'='Marzec', '04'='Kwiecień','05'='Maj','06'='Czerwiec','07'='Lipiec',
                      '08'='Sierpień', '09'='Wrzesień', '10'='Paźdźiernik', '11'='Listopad', '12'='Grudzień')

data_pm10 <- read.csv2('https://pastebin.com/raw/6RJNXZTZ', sep=',', header = TRUE, stringsAsFactors = FALSE)
data_pm25 <- read.csv2('https://gist.githubusercontent.com/plubon/37063a7574dd2f1adddc412573015100/raw/f10c0c590fe4bf6380088d8a870311865256095c/gistfile1.txt', sep=',', header = TRUE, stringsAsFactors = FALSE)
data_pm10 <- data_pm10[data_pm10$Rok > 2007,]
data_pm10$Pomiar <- as.numeric(data_pm10$Pomiar)
data_pm10$CzyPrzekroczono <- ifelse(data_pm10$Pomiar >= 50, 'Tak', 'Nie')
data_pm10$Data <- as.Date(data_pm10$Data)
data_pm10$Month <- strftime(data_pm10$Data, "%m")
data_pm10 <- na.omit(data_pm10)
data_pm25$Pomiar <- as.numeric(data_pm25$Pomiar)
data_pm25 <- na.omit(data_pm25)
data_pm25$Godzina <- sapply(strsplit(data_pm25$Data, ' '), function(x){return(x[2])})
data_pm25$Godzina <- sapply(strsplit(data_pm25$Godzina, ':'), function(x){return(x[1])})
data_pm25$Data <- as.Date(data_pm25$Data)
data_pm25$Month <-  strftime(data_pm25$Data, "%m")
data_pm25$Month <- unlist(sapply(data_pm25$Month, function(x){return(nazwyMiesiecy[[x]])}))
by_month <- aggregate(data_pm10$Pomiar, by=list(data_pm10$Month, data_pm10$Rok), FUN=mean)
by_month$Date <- as.Date(ISOdate(by_month$Group.2, by_month$Group.1, 1))
by_hour <- aggregate(data_pm25$Pomiar, by=list(data_pm25$Godzina, data_pm25$Month), FUN=mean)
by_hour_dev <- aggregate(data_pm25$Pomiar, by=list(data_pm25$Godzina, data_pm25$Month), FUN=sd)
by_hour$sd <- by_hour_dev$x
by_hour$month_f <- factor(by_hour$Group.2, levels=c('Styczeń', 'Luty', 'Marzec', 'Kwiecień', 'Maj', 'Czerwiec', 'Lipiec',
                                                    'Sierpień', 'Wrzesień', 'Paźdźiernik', 'Listopad', 'Grudzień'))
ui <- dashboardPage(
  skin = "black",
  dashboardHeader(title = "Jakość powietrza w Piastowie"),
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("Opis", icon = icon("info-circle"), tabName = "about"),
      menuItem("1)", tabName = "pm10", icon = icon("dashboard")),
      menuItem("2)", tabName = "pm25", icon = icon("dashboard")),
      menuItem("3)", tabName = 'summary', icon = icon("dashboard"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem("about",HTML(
              "Dashboard przedstawia dane dotyczące jakośći powietrza w mojej rodzinnej miejscowości.<br/>
              1) Wykres przedstawia uśrednione miesięczne stężenie pm10, co pozwala sprawdzić czy problem z zanieczyszczeniem powietrza
              występuje od dawna czy pojawiło się w ostatnim czasie.<br/>
              2) Wykres przedstawia średnie stężenie pm2.5 na przestrzeni doby w poszczególnych miesiącach. Dzięki temu widać czy jest ono większe
              np. w zimie w nocy, gdy miekszkańcy ogrzewają domy piecami.<br/>
              3) Wizualizacja pokazuje liczbę dni, w których normy zanieczyszczenia powietrza zostały przekroczone co pozwala ocenić skalę tego problemu")
      ),
      tabItem("pm10",
              plotOutput("pm10_plot")),
      tabItem("pm25",
              plotOutput("pm25_plot")),
      tabItem("summary",fluidPage(
              plotOutput("summary")), htmlOutput("my_text"))
      )
    )
)
server <- function(input, output) {
  output[['pm10_plot']] <- renderPlot(
    ggplot(aes(x=Date, y=x), data=by_month)+
      geom_line()+
      geom_point()+
      geom_hline(aes(yintercept=50, colour='Poziom dopuszczalny'), linetype="dashed")+
      theme(legend.position="bottom")+
      scale_x_date(labels = date_format("%m-%Y"), date_breaks = "3 month")+
      ggtitle('Uśrednione miesięczne stężenie pm10 w Piastowie')+
      xlab('Miesiąc')+
      ylab('Stężenie pm10 w µg/m3')
  )
  output[['pm25_plot']] <- renderPlot(
    ggplot(aes(x=Group.1,y=x), data=by_hour)+
      geom_point()+
      geom_errorbar(aes(ymin=x-sd, ymax=x+sd))+
      xlab('Godzina')+
      ylab('Stężenie pm2.5 w µg/m3')+
      geom_hline(aes(yintercept=25, colour='Dopuszczalna norma'), linetype="dashed")+
      facet_wrap(~month_f)+
      theme(legend.position="bottom")+
      ggtitle('Stężenie pm2.5 w Piastowie na przestrzeni doby')
  )
  output[['summary']] <- renderPlot(
    ggplot(aes(x=CzyPrzekroczono), data=data_pm10)+
      geom_bar(fill='blue')+
      xlab('Czy przekroczono dobowy dopuszczalny poziom pm10?')+
      ylab('Liczba dni')+
      ggtitle('Liczba dni, w których przekroczono dobowy dopuszczalny poziom pm10 w latach 2008-2017')
    )
  output[['my_text']] <- renderUI({
    HTML("Dobowy poziom informowania został przerkoczony tylko jeden raz.")
  })
}

shinyApp(ui, server)