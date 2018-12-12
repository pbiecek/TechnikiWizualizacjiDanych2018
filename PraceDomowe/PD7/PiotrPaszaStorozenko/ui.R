library(ggplot2)
library(ggthemes)
library(gridExtra)
library(forecast)
library(ggpubr)
library(dplyr)
library(lubridate)

library(shiny)
min_date = as_date("2016-01-01") # To jest potrzebne z przyczyn mi absolutnie nie znanych...
max_date = as_date("2017-12-31") # Podobnie jak to, nie dalo sie tego odczytac z .RData


shinyUI(
  fluidPage(
    headerPanel('Zanieczyszczenia w Warszawie'),
    
    column(4, wellPanel(
      
      dateRangeInput('dateRange',
                     label = 'Wybierz zakres danych',
                     start = min_date, end = max_date,
                     min = min_date, max = max_date
      ),
      selectInput('in1', 'Zanieczyszczenie',params, selectize=TRUE),
      selectInput('in2', 'Podział czasowy',time_uni, selectize=TRUE),
      checkboxInput("checkbox", label = "Wiele wykresów", value = FALSE),
      textOutput('opis')
    )
    ),
    mainPanel(
      plotOutput('plot1')
    )
  )
  
)
