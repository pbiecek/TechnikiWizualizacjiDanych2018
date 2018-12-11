library(readxl)
library(shiny)
library(shinydashboard)
library(dplyr)
library(tidyr)
library(ggplot2)

dt <- read_excel("2017_PM10_24g.xlsx")
dt2 <- read_excel("2017_PM25_24g.xlsx")
dates = seq(from = as.Date("2017-01-01"), to = as.Date("2017-12-31"), by = 'day')

#PM10
KRK_pm10 <- dt[,c("MpKrakBujaka")]
WWA_pm10 <- dt[,c("MzWarAKrzywo")]
LDZ_pm10 <- dt[,c("LdLodzLegion")]
BDG_pm10 <- dt[,c("KpBydPlPozna")]
WRO_pm10 <- dt[,c("DsWrocOrzech")]

KRK_pm10 <- cbind(city=rep('Krakow',length(KRK_pm10)),gather(KRK_pm10),dates)
WWA_pm10 <- cbind(city=rep('Warszawa',length(WWA_pm10)),gather(WWA_pm10),dates)
LDZ_pm10 <- cbind(city=rep('Lodz',length(LDZ_pm10)),gather(LDZ_pm10),dates)
BDG_pm10 <- cbind(city=rep('Bydgoszcz',length(BDG_pm10)),gather(BDG_pm10),dates)
WRO_pm10 <- cbind(city=rep('Wroclaw',length(WRO_pm10)), key = rep('DsWrocOrzech',length(WRO_pm10)),values = WRO_pm10,dates)
colnames(WRO_pm10) <- c("city","key","value","dates")                  
all_pm10 <- rbind(KRK_pm10,WWA_pm10,LDZ_pm10,BDG_pm10,WRO_pm10)

#PM2.5
KRK_pm25 <- dt2[,c("MpKrakBujaka")]
WWA_pm25 <- dt2[,c("MzWarKondrat")]
LDZ_pm25 <- dt2[,c("LdLodzCzerni")]
BDG_pm25 <- dt2[,c("KpBydBerling")]
WRO_pm25 <- dt2[,c("DsWrocNaGrob")]
KRK_pm25 <- cbind(city=rep('Krakow',length(KRK_pm25)),gather(KRK_pm25),dates)
WWA_pm25 <- cbind(city=rep('Warszawa',length(WWA_pm25)),gather(WWA_pm25),dates)
LDZ_pm25 <- cbind(city=rep('Lodz',length(LDZ_pm25)),gather(LDZ_pm25),dates)
BDG_pm25 <- cbind(city=rep('Bydgoszcz',length(BDG_pm25)),gather(BDG_pm25),dates)
WRO_pm25 <- cbind(city=rep('Wroclaw',length(WRO_pm25)),key = rep('DsWrocNaGrob',length(WRO_pm25)),WRO_pm25,dates)
colnames(WRO_pm25) <- c("city","key","value","dates")      
all_pm25 <- rbind(KRK_pm25,WWA_pm25,LDZ_pm25,BDG_pm25,WRO_pm25)


# User Interface
ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "Smog w Polsce"
                  ),
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("Dane dzienne i statystyki", tabName = "dashboard"),
      h2("Oba wykresy"),
      dateRangeInput(inputId = "data",
                     label = "Wybierz przedzial dat",
                     start  = "2017-01-01",
                     end    = "2017-12-31",
                     min    = "2017-01-01",
                     max    = "2017-12-31",
                     format = "yyyy-mm-dd"),
      h2("Wykres dzienny"),
      checkboxInput(inputId = "dopuszczalny", 
                    label = "Poziom dopuszczalny",
                    value = TRUE),
      checkboxGroupInput(
        inputId = "miasto", 
        label = "Wybierz miasta", 
        choices = unique(all_pm10$city),
        selected = "Krakow"),
      h2("Statystyki"),
      radioButtons(inputId = "funkcja", 
                   label = "Wybierz funkcje", 
                   choices = c("max","min","avg","sd"),
                   selected = "max")
    )
  ),
  dashboardBody(
    helpText("Pył PM10, bez problemu wnika do naszych płuc, natomiast pył PM2.5 jest tak małym, że może wnikać nawet do naszej krwi."),
    helpText("W aplikacji można obejrzeć zmianę zawartości tych pyłów w powietrzu w 5 dużych miastach w Polsce."),
    helpText("Ważny jest wykres dotyczący udziału procentowego PM2.5 w PM10 - wiemy wtedy czy smog zagrażał naszemu układu krwionośnemu czy tylko oddechowemu."),
    helpText("Oprócz tego na wykresach po prawej stronie znajdziemy zaagregowane przez wybraną nas funkcję dane w wybranym okresie we wszystkich dostępnych miastach."),
    
    fluidPage(
      fluidRow(
        column(7,
          plotOutput("plot1")
        ),
        column(5,
               plotOutput("plot2")
        )
       
      ),
      h1(""),
      fluidRow(
        column(7,
               plotOutput("plot3")
        ),
        column(5,
               plotOutput("plot4")
        )
        
      )
    )
    
    # tabItems(
    #   tabItem("dashboard",
    #           plotOutput("plot1"),
    #           plotOutput("plot3")
    #   ),
    #   tabItem("stats",
    #           plotOutput("plot2"),
    #           plotOutput("plot4")
    #   )
    # )
    
  )
  
 
)


# Server
server <- function(input, output) {
  dane_react <- reactive({
    dt <- all_pm10 %>% 
      filter(city %in% input$miasto) %>% 
      filter(dates > input$data[1] & dates < input$data[2])
    return(dt)
  })
  dane_react2 <- reactive({
    if(input$funkcja == "min"){
      dt <- all_pm10 %>% 
        filter(dates > input$data[1] & dates < input$data[2]) %>%
        group_by(city) %>%
        summarise(agg = min(value, na.rm = TRUE))
    } else if(input$funkcja == "max"){
      dt <- all_pm10 %>% 
        filter(dates > input$data[1] & dates < input$data[2]) %>%
        group_by(city) %>%
        summarise(agg = max(value, na.rm = TRUE))
    }else if(input$funkcja == "avg"){
      dt <- all_pm10 %>% 
        filter(dates > input$data[1] & dates < input$data[2]) %>%
        group_by(city) %>%
        summarise(agg = mean(value, na.rm = TRUE))
    }else if(input$funkcja == "sd"){
      dt <- all_pm10 %>% 
        filter(dates > input$data[1] & dates < input$data[2]) %>%
        group_by(city) %>%
        summarise(agg = sd(value, na.rm = TRUE))
    }
  })
  dane_react4 <- reactive({
    if(input$funkcja == "min"){
      dt <- all_pm25 %>% 
        filter(dates > input$data[1] & dates < input$data[2]) %>%
        group_by(city) %>%
        summarise(agg = min(value, na.rm = TRUE))
    } else if(input$funkcja == "max"){
      dt <- all_pm25 %>% 
        filter(dates > input$data[1] & dates < input$data[2]) %>%
        group_by(city) %>%
        summarise(agg = max(value, na.rm = TRUE))
    }else if(input$funkcja == "avg"){
      dt <- all_pm25 %>% 
        filter(dates > input$data[1] & dates < input$data[2]) %>%
        group_by(city) %>%
        summarise(agg = mean(value, na.rm = TRUE))
    }else if(input$funkcja == "sd"){
      dt <- all_pm25 %>% 
        filter(dates > input$data[1] & dates < input$data[2]) %>%
        group_by(city) %>%
        summarise(agg = sd(value, na.rm = TRUE))
    }
  })
  dane_react3 <- reactive({
    dt <- all_pm25 %>% 
      filter(city %in% input$miasto) %>% 
      filter(dates > input$data[1] & dates < input$data[2])
    dt <- merge(dt,all_pm10, by = c("city" = "city", "dates" = "dates"))
    dt <- dt %>% 
      mutate(diff = value.y/value.x*100) %>%
      select(key=key.x,dates, city,diff)
    return(dt)
  })
  
  output$plot1 = renderPlot({
    p1 <- ggplot(dane_react(), aes(x = dates, y = value, color = city)) + 
      geom_line() + 
      theme_minimal() +
      ggtitle("Wykres pyłów PM10 w zależności od dnia w wybranych miastach")+
      labs(y = "PM10 ["~mu~"g/m3]", x = element_blank())+
      theme(
        legend.title = element_blank(),                       # usun tytul legendy
        legend.text = element_text(size = 15),                # ustawienia labeli legendy
        plot.title = element_text(hjust=0.5, size = 25),      # ustawienia tytul wykresu
        axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 13),                # ustawienia labeli przy osi x
        axis.text.y = element_text(size = 13)
      )
    if(input$dopuszczalny){
      p1 + geom_hline(yintercept=50,color = "red",size = 1.3)
    }else{
      p1
    }
  })
  
  output$plot2 = renderPlot({
    if(input$funkcja == 'max'){
      t <- "Maksimum"
    }else if(input$funkcja == 'min'){
      t <- "Minimum"
    }else if(input$funkcja == 'avg'){
      t <- "Średnia wartość"
    }else if(input$funkcja == 'sd'){
      t <- "Odchylenie standardowe"
    }
    p2<-ggplot(dane_react2(), aes(x = city, y = agg)) + 
      geom_bar(stat = "identity",fill="steelblue")+
      theme_minimal()+
      ggtitle(paste(t,"ilości pyłu PM10 w wybranym okresie"))+
      labs(y = "PM10 ["~mu~"g/m3]", x = element_blank())+
      theme(
        plot.title = element_text(hjust=0.5, size = 25),      # ustawienia tytul wykresu
        axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 13),                # ustawienia labeli przy osi x
        axis.text.y = element_text(size = 13)
      )
    if(input$dopuszczalny){
      p2 + geom_hline(yintercept=50,color = "red",size = 1.3)
    }else{
      p2
    }
    
  })
  
  output$plot3 = renderPlot({
    ggplot(dane_react3(), aes(x = dates, y = diff, color = city)) + 
      geom_line()+
      theme_minimal()+
      ggtitle("Wykres stosunku pyłów PM2.5 do pyłów PM10 (im więcej tym gorzej)") +
      labs(y = "PM2.5/PM10 [%]", x = element_blank())+
      theme(
        legend.title = element_blank(),                       # usun tytul legendy
        legend.text = element_text(size = 15),                # ustawienia labeli legendy
        plot.title = element_text(hjust=0.5, size = 25),      # ustawienia tytul wykresu
        axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 13),                # ustawienia labeli przy osi x
        axis.text.y = element_text(size = 13)
      )
  })
  
  output$plot4 = renderPlot({
    if(input$funkcja == 'max'){
      t <- "Maksimum"
    }else if(input$funkcja == 'min'){
      t <- "Minimum"
    }else if(input$funkcja == 'avg'){
      t <- "Średnia wartość"
    }else if(input$funkcja == 'sd'){
      t <- "Odchylenie standardowe"
    }
    p4<-ggplot(dane_react4(), aes(x = city, y = agg)) + 
      geom_bar(stat = "identity",fill = "steelblue")+
      theme_minimal()+
      ggtitle(paste(t,"ilości pyłu PM2.5 w wybranym okresie"))+
      labs(y = "PM2.5 ["~mu~"g/m3]", x = element_blank())+
      theme(
        plot.title = element_text(hjust=0.5, size = 25),      # ustawienia tytul wykresu
        axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 13),                # ustawienia labeli przy osi x
        axis.text.y = element_text(size = 13)
      )
    if(input$dopuszczalny){
      p4 + geom_hline(yintercept=50,color = "red",size = 1.3)
    }else{
      p4
    }
  })
  }

shinyApp(ui, server)
