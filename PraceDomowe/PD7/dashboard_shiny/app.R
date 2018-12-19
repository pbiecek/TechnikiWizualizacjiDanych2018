library(shiny)
library(shinydashboard)
library(reshape2)
library(dplyr)
library(ggplot2)

options(stringsAsFactors = FALSE)
dane<-read.csv2("Powietrze_w_Opolu.csv")
dane[1]<-as.Date(dane[[1]],format="%d-%m-%Y")

ui <- dashboardPage(
  skin = "black",
  dashboardHeader(title = "Powietrze w Opolu"),
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("PM10", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("PM2,5",tabName = "dashboard2",icon = icon("dashboard")),
      menuItem("Opis", icon = icon("info-circle"), tabName = "about",
               badgeColor = "green")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem("dashboard",
              plotOutput("PM10wykres"),
              tableOutput("PM10tabela")
      ),
      tabItem("dashboard2",
              plotOutput("PM2,5wykres"),
              tableOutput("PM2,5tabela")
      ),
      tabItem("about",
              "Aplikacja pokazuje stan powietrza w Opolu od 01-01-2018 do 09-12-2018.Dane są w ug/m3(czytaj mikrogramy na metr sześcienny). 
              Zalecaną normą dla cząsteczek pyłu PM10 jest 50 ug/m3, dla cząsteczek typu PM2,5 wynosi 25 ug/m3. 
              Dane zebrane na podstawie http://powietrze.gios.gov.pl/pjp/archives dla stacji 4 i 5 w Opolu"
      )
    )
  )
)

server <- function(input, output) {
  output[["PM10wykres"]] <- renderPlot(
    ggplot(dane,aes(x=Data,y=Pył.zawieszony.PM10.ug.m3.))+geom_col()+geom_hline(yintercept = 50,color="red")+
      scale_x_date(breaks = "months",date_labels = "%b")+labs(y="Stężenie pyłu PM10(ug/m3)",title = "Czerwona linia jest dopuszczalną normą")
  )
  output[["PM10tabela"]]<- renderTable({data.frame(średnia=mean(dane[[2]]),mediana=median(dane[[2]]),przekroczeń=sum(dane[[2]]>50))})
  
  output[["PM2,5wykres"]] <- renderPlot(
    ggplot(dane,aes(x=Data,y=Pył.zawieszony.PM2.5.ug.m3.))+geom_col()+geom_hline(yintercept = 25,color="red")+
      scale_x_date(breaks = "months",date_labels = "%b")+labs(y="Stężenie pyłu PM2,5(ug/m3)",title = "Czerwona linia jest dopuszczalną normą")
  )
  output[["PM2,5tabela"]]<- renderTable({data.frame(średnia=mean(dane[[3]],na.rm = TRUE),mediana=median(dane[[3]],na.rm = TRUE),przekroczeń=sum(dane[[3]]>25,na.rm = TRUE))})
}

shinyApp(ui, server)