library(shiny)
library(shinydashboard)
library(reshape2)
library(dplyr)
library(ggplot2)
library(openxlsx)
library(RColorBrewer)

dane <- read.xlsx("Statystyki_2000-2018_wer20180828.xlsx")

dane %>% group_by(Rok, Województwo) %>% summarise(Średnia = mean(Średnia)) -> WojewództwaLata

# -------------------------------------------------------------------------
d2017.10 <- read.xlsx("2017_PM10_1g.xlsx")
d2017.10[,-1] <- as.data.frame(
  apply(
    d2017.10[,-1],
    2,
    function(x) as.numeric(gsub(",", ".", x))
    )
  )


d2017.10[,1] <-  as.POSIXct(d2017.10[,1]*(60*60*24), origin="1899-12-30",tz="GMT")
d2017.10$`2`

colnames(d2017.10) <- c("Czas", 1:123) 
rownames(d2017.10) <- NULL

# -------------------------------------------------------------------------
d2017.25 <- read.xlsx("2017_PM25_1g.xlsx")[-(1:5),]

d2017.25[,-1] <- as.data.frame(
  apply(
    d2017.25[,-1],
    2,
    function(x) as.numeric(gsub(",", ".", x))
  )
)

colnames(d2017.25) <- c("Czas", 1:46) 
rownames(d2017.25) <- NULL
d2017.25[,1] <-  as.POSIXct(as.numeric(d2017.25[,1])*(60*60*24), origin="1899-12-30",tz="GMT")

# -------------------------------------------------------------------------

dane2017.10 <-  d2017.10 %>%
  mutate(Czas = Czas, War = rowMeans(select(d2017.10, as.character(1:123)), na.rm = TRUE)) %>%
  select(Czas, War)
dane2017.25 <-  d2017.25 %>%
  mutate(Czas = Czas, War = rowMeans(select(d2017.25, as.character(1:46)), na.rm = TRUE)) %>%
  select(Czas, War)

# ========================================================================

ui <- dashboardPage(
  dashboardHeader(title = "Zanieczyszczenie powietrza w Polsce"),
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("About", icon = icon("info-circle"), tabName = "about"),
      htmlOutput("menu")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem("dashboard",
              fluidRow(box(plotOutput("2017"), width = 12)),
              fluidRow(box(plotOutput("Lata"), width = 6),
                       box(plotOutput("Rok"), width = 6))
              
      ),
      tabItem("about",
              "7 praca domowa z przedmiotu TWD, 2018r."
      )
    )
  )
)

server <- function(input, output) {
  
  output$menu <- renderUI({
    if(input$tabs=="dashboard"){
      wellPanel(style = "background: #111111",
        dateRangeInput(
          "period2017",
          "Przedział czasowy w 2017 roku",
          start = "2017-01-01",
          end = "2017-02-01",
          min = "2017-01-01",
          max = "2017-12-30",
          format = "dd.mm.2017"
        ),
        checkboxGroupInput(
          "woj",
          "Województwa",
          choices = unique(dane$Województwo),
          selected = unique(dane$Województwo)
        ),
        numericInput(
          "rok",
          "Rok",
          value = 2017,
          max = 2017,
          min = 2000,
          step = 1
        )
      )
        
    } else if (input$tabs=="about") {
      wellPanel(style = "background: #111111")
    } 
    
  })
  
  output$`2017` <- renderPlot({
    ggplot() +
      geom_line(data = dane2017.10 %>% filter(Czas > as.POSIXct(input$period2017[1]), Czas < as.POSIXct(input$period2017[2])), aes(x=Czas,y=War,color="P10"))+
      geom_line(data = dane2017.25 %>% filter(Czas > as.POSIXct(input$period2017[1]), Czas < as.POSIXct(input$period2017[2])), aes(x=Czas,y=War,color="P2.5"))+
      scale_color_discrete(aes(P10 = 1, P2.5 = 2)) +
      theme_minimal() + 
      guides(color=guide_legend(title="Wskaźniki")) +
      xlab("Czas") + 
      ylab("P10") +
      ggtitle("P10 i P2.5 w 2017 roku")
  })
  
  output$Lata <- renderPlot({
    ggplot(WojewództwaLata %>% filter(Województwo %in% input$woj), aes(x = Rok, y = Średnia)) +
      geom_point(aes(color = Województwo)) + 
      geom_smooth() + 
      ggtitle("Zanieczyszczenia na prestrzeni lat") + 
      guides(color=guide_legend(title="Województwa")) +
      theme_minimal() + 
      xlab("Lata") + 
      ylab("Średnie z poszczególnych województw")
  })
  
  output$Rok <- renderPlot({
    ggplot(WojewództwaLata %>% filter(Rok == as.numeric(input$rok)), aes(x = Województwo, y = Średnia, fill = Województwo)) +
      geom_col() +
      theme_minimal() + 
      theme(legend.position="none",
            axis.text.x = element_text(angle = 45, hjust = 1)) +
      xlab("Województwa") + 
      ggtitle("Zanieczyszczenia w danym roku")
    
  })
  
}
  

shinyApp(ui, server)
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
