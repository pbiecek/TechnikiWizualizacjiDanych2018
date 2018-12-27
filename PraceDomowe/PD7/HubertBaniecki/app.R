library(shiny)
library(shinydashboard)
library(reshape2)
library(dplyr)
library(ggplot2)
library(openxlsx)
library(RColorBrewer)

data <- read.xlsx("data2017.xlsx")
colnames(data) <- data[1,]
data <- data[-c(1,2,3,4,5),]

## pierwszy wykres
temp1 <- data %>% select("Kod stacji", MzWarAlNiepo, MpKrakBujaka, LdLodzRudzka, DsWrocWybCon, WpPoznChwial,
                        PmGdaLecz08m, ZpSzczPils02, KpBydPlPozna, LbLubSliwins, PdBialWaszyn, SlKatoPlebA4)
colnames(temp1) <- c("Data", "Warszawa", "Kraków", "Łódź", "Wrocław", "Poznań", "Gdańsk", "Szczecin",
                    "Bydgoszcz","Lublin","Białystok","Katowice")
Data <- temp1$Data
temp1 <- as.data.frame(apply(temp1[,-1],2, function(x) as.numeric(sub(",", ".", x, fixed = TRUE))))
temp1$Data <- Data
temp1$Data <- as.Date(as.numeric(temp1$Data), origin = "1899-12-30")


## drugi wykres
temp2 <-  data %>% select("Kod stacji",MzWarAlNiepo, MzWarTolstoj, MzWarAKrzywo)
colnames(temp2) <- c("Data","AlejaNiepodleglosci","Tolostaja","AnieliKrzywon")
temp2$Data <- as.numeric(temp2$Data)
Data <- temp2$Data
temp2 <- as.data.frame(apply(temp2[,-1],2, function(x) as.numeric(sub(",", ".", x, fixed = TRUE))))
temp2$Data <- Data
temp2$Data <- as.Date(as.numeric(temp2$Data), origin = "1899-12-30")

## trzeci wykres
temp3 <- temp1[,c("Katowice","Kraków","Data")]
temp3[is.na(temp3)] <- 0
temp3$Katowice <- ifelse(temp3$Katowice<=50, 1, ifelse(temp3$Katowice<=200,2,3))
temp3$Kraków <- ifelse(temp3$Kraków<=50, 1, ifelse(temp3$Kraków<=200,2,3))
temp3 <- temp3[,-3]
Katowice <- table(temp3$Katowice)
Kraków <- table(temp3$Kraków)
temp3 <- rbind(as.data.frame(Katowice),as.data.frame(Kraków))
temp3$Miasto <- c(rep("Katowice",3), rep("Kraków",3))


ui <- dashboardPage(
  skin = "black",
  dashboardHeader(title= "PD7"),
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Opis", icon = icon("info-circle"), tabName = "about"))
    ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML(
      '.myClass { 
      font-size: 20px;
      line-height: 50px;
      text-align: left;
      font-family: "Helvetica Neue",Helvetica,Arial,sans-serif;
      padding: 0 15px;
      overflow: hidden;
      color: white;
      }
      ')),
    tags$script(HTML('
                     $(document).ready(function() {
                     $("header").find("nav").append(\'<span class="myClass"> <span style="color:black;font-weight:bold">Zanieczyszczenie powietrza w Polsce</span></span>\');
                     })
                     ')),
      tags$style(
        "body{
        min-height: 1000px;
        height: auto;
        max-width: 1200px;
        width: autp;
        margin: auto;
        }"
      )
      ),
    tabItems(
      tabItem("dashboard",
              fluidRow(box(plotOutput("plot1"),
                           width=8, title = "Zanieczyszczenie powietrza w największych miastach Polski", 
                           background = "red",solidHeader = TRUE),
                       
                       box(sliderInput(inputId = "liczbaMiast", label = "Liczba największych miast:",
                                       min = 1, max = 11, value = 5), width=4, background = "black")),
          
              fluidRow(box(plotOutput("plot2"), width = 8, title = "Zanieczyszczenie powietrza w Warszawie",
                           background = "orange",solidHeader = TRUE),
              
              box(
              checkboxGroupInput(inputId = "wybraneStacje", 
                                 label = "Wybierz stacje pomiarowe",
                                 choices = c("AlejaNiepodleglosci", "Tolostaja", "AnieliKrzywon"),
                                 selected = "AlejaNiepodleglosci"),
              dateRangeInput(inputId = "suwakPoDacie", label = "Wybierz zakres daty", 
                             start = "2017-01-01", end = "2017-12-31", 
                             min = NULL,
                             max = NULL, format = "yyyy-mm-dd", startview = "month", weekstart = 0,
                             language = "en", separator = " to ", width = NULL),
              checkboxInput(inputId = "zagrozenie", label = "Pokaż linie zagrożenia"),
              downloadButton('downloadData', 'Pobierz wykres', style="display: block; margin: 0 auto; width: 230px;color: black;"), 
              width=4, background = "black")),
              
              fluidRow(box(plotOutput("plot3"), width = 8, title = "Czy da się oddychać w Krakowie i Katowicach?",
                           background = "yellow", solidHeader = TRUE))
              ),
      tabItem("about",
              box(
                title = "Opis", width = 8, background = "light-blue",
                verbatimTextOutput("text")
              )))
    )
)



server <- function(input, output) {
  wykres2 <- reactive({
    stacje <- input$wybraneStacje
    kolory <- brewer.pal(6,"Paired")
    start <- input$suwakPoDacie[1]
    end <- input$suwakPoDacie[2]
    
    temp2 <- temp2[temp2$Data<=end & temp2$Data>=start,]
    p <- ggplot(temp2)
    if("AlejaNiepodleglosci" %in% stacje){
      p <- p + geom_point(aes(x=Data,y=AlejaNiepodleglosci), color=kolory[1]) +
        stat_smooth(aes(x=Data,y=AlejaNiepodleglosci), color = kolory[2])
    }
    if("Tolostaja" %in% stacje){
      p <- p + geom_point(aes(x=Data,y=Tolostaja), color=kolory[3]) +
        stat_smooth(aes(x=Data,y=Tolostaja), color = kolory[4])
    }
    if("AnieliKrzywon" %in% stacje){
      p <- p + geom_point(aes(x=Data,y=AnieliKrzywon), color=kolory[5]) +
        stat_smooth(aes(x=Data,y=AnieliKrzywon), color = kolory[6])
    }
    if(input$zagrozenie==TRUE){
      p <- p + geom_hline(yintercept = 50, color = "green", size = 1)
      p <- p + geom_hline(yintercept = 200, color = "yellow2", size = 1)
      p <- p + geom_hline(yintercept = 300, color = "red", size = 1)
    }
    p <- p + labs(y="PM10 [ug/m^3]") + theme_classic()
    
    p
  })
  
  output$plot1 = renderPlot({
    
    stop <- input$liczbaMiast

    kolory <- brewer.pal(11,"Set3")[11:1]
    temp1 <- temp1[,c(1:stop,12)]
    temp1 <- melt(temp1, id="Data")
    q <- ggplot(temp1, aes(x=Data, y=value, color = variable)) + stat_smooth(se = FALSE, size = 1.5) +
      scale_colour_manual(name="Miasto",values = kolory) + 
      labs(y="PM10 [ug/m^3]") + theme_classic()
      
    
    q
  }, width = 600, height = 400)
  
  output$plot2 = renderPlot({
    
    wykres2()
  }, width = 600, height = 400)
  
  output$plot3 = renderPlot({
    
    r <- ggplot() + geom_col(data = temp3, aes(x = Var1, y = Freq, fill = Miasto), position = "dodge") +
      scale_fill_brewer(palette="Dark2") + 
      scale_x_discrete("Poziom", labels = c("1" = "dopuszczalny","2" = "informowania","3" = "alarmowy")) +
      labs(y="Liczba dni w roku")
    r
  }, width = 600, height = 400)
  
  output$downloadData <- downloadHandler(
    filename = function() { 
      paste("wykres+", Sys.Date(), ".pdf", sep="")
    },
    content = function(file) {
      ggsave(plot=wykres2(), filename=file)
    })
  
  output$text <- renderText({
    paste("W podanej wizualizacji możemy zbadać następujące zagadnienia:",
          "Jak bardzo zanieczyszczone są największe miasta Polski",
          "Które z tych miast są najbardziej zanieczyszczone",
          "Która pora roku sprzyja zanieczyszczeniu powietrza",
          "Jak bardzo musimy martwić się o zanieczyszczenie stolicy",
          "Szczegółowe dane dotyczące zanieczyszczenia Warszawy",
          "Czy da się oddychać w Katowicach i Krakowie","",
          "Hubert Baniecki"
          ,sep="\n")
  })

}


shinyApp(ui, server)