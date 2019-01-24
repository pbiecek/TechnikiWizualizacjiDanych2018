
############################################################################################################

require(plotly)
require(stringi)
require(tidytext)
require(wordcloud2)
require(shiny)
require(shinydashboard)
require(SmarterPoland)
require(ggplot2)
require(dplyr)

options(stringsAsFactors = FALSE)

############################################################################################################

ewa_wiadomosci <- read.csv(file = 'wiadomosci_czasy_ewa.csv', header = TRUE, sep = ',')
ewa_slowa_osoby <- read.csv(file = 'zliczone_slowa_do_konkretnych_osob_ewa.csv', header = TRUE, sep = ',')

michal_wiadomosci <- read.csv(file = 'wiadomosci_czasy_michal.csv', header = TRUE, sep = ',')
michal_slowa_osoby <- read.csv(file = 'zliczone_slowa_do_konkretnych_osob_michal.csv', header = TRUE, sep = ',')

ewa_kontakt <- ewa_slowa_osoby %>% select(Kto = kolumna_kto) %>% distinct()
ewa_kontakt <- as.vector(ewa_kontakt$Kto)

michal_kontakt <- michal_slowa_osoby %>% select(Kto = kolumna_kto) %>% distinct()
michal_kontakt <- as.vector(michal_kontakt$Kto)

############################################################################################################

ui <- dashboardPage(
  
  skin = 'blue',
  
  dashboardHeader(title = 'MENU'),
  
  dashboardSidebar(
    
    sidebarMenu(
      
      selectizeInput(inputId = 'kontakt_ewa',
                     label = 'Kontakty Ewy: ',
                     choices = ewa_kontakt),
      
      selectizeInput(inputId = 'kontakt_michal',
                     label = 'Kontakty Michała: ',
                     choices = michal_kontakt)
    
    )
  ),
  
  dashboardBody(
    
    fluidPage(
      
      tabsetPanel(
        tabPanel("Chmura Słów Michał", fluid = TRUE, wordcloud2Output('wordcloud_m')),
        tabPanel("Chmura Słów Ewa", fluid = TRUE, wordcloud2Output('wordcloud_e')),
        tabPanel("Liczba Postów Michał - Godziny", fluid = TRUE, plotlyOutput('plot_m1')),
        tabPanel("Liczba Postów Ewa - Godziny", fluid = TRUE, plotlyOutput('plot_e1')),
        tabPanel("Liczba Postów Michał - Miesiące", fluid = TRUE, plotlyOutput('plot_m2')),
        tabPanel("Liczba Postów Ewa - Miesiące", fluid = TRUE, plotlyOutput('plot_e2'))
        )
      
    )
    
  )
  
)

############################################################################################################

server <- function(input, output) {
  
  
  
  podsumowanie_ewa1 <- reactive({
    podsumowanie_ewa1 <- ewa_slowa_osoby %>% filter(kolumna_kto == input[['kontakt_ewa']]) %>% select(slowo, liczba)
  })
  
  podsumowanie_ewa2 <- reactive({
    kontakt <- input[['kontakt_ewa']]
    dane <- ewa_wiadomosci %>% filter(Kontakt == kontakt) %>% select(Czas, Autor) %>% mutate(Autor = ifelse(Autor == 1, '1. Ewa', paste('2. ', kontakt, sep = '')))
    temp <- dane %>% mutate(Godzina = substr(as.character(Czas), 12, 13)) %>% group_by(Autor, Godzina) %>% summarise(Ile = n())
    podsumowanie_ewa2 <- data.frame(temp)
  })
  
  podsumowanie_ewa3 <- reactive({
    kontakt <- input[['kontakt_ewa']]
    dane <- ewa_wiadomosci %>% filter(Kontakt == kontakt) %>% select(Czas, Autor) %>% mutate(Autor = ifelse(Autor == 1, '1. Ewa', paste('2. ', kontakt, sep = '')))
    temp <- dane %>% mutate(Data = substr(as.character(Czas), 1, 7)) %>% group_by(Autor, Data) %>% summarise(Ile = n())
    podsumowanie_ewa3 <- data.frame(temp)
  })
  
  
  
  podsumowanie_michal1 <- reactive({
    podsumowanie_michal1 <- michal_slowa_osoby %>% filter(kolumna_kto == input[['kontakt_michal']]) %>% select(slowo, liczba)
  })
  
  podsumowanie_michal2 <- reactive({
    kontakt <- input[['kontakt_michal']]
    dane <- michal_wiadomosci %>% filter(Kontakt == kontakt) %>% select(Czas, Autor) %>% mutate(Autor = ifelse(Autor == 1, '1. Michał', paste('2. ', kontakt, sep = '')))
    temp <- dane %>% mutate(Godzina = substr(as.character(Czas), 12, 13)) %>% group_by(Autor, Godzina) %>% summarise(Ile = n())
    podsumowanie_michal2 <- data.frame(temp)
  })
  
  podsumowanie_michal3 <- reactive({
    kontakt <- input[['kontakt_michal']]
    dane <- michal_wiadomosci %>% filter(Kontakt == kontakt) %>% select(Czas, Autor) %>% mutate(Autor = ifelse(Autor == 1, '1. Michał', paste('2. ', kontakt, sep = '')))
    temp <- dane %>% mutate(Data = substr(as.character(Czas), 1, 7)) %>% group_by(Autor, Data) %>% summarise(Ile = n())
    podsumowanie_michal3 <- data.frame(temp)
  })
  
  
  
  output$wordcloud_e <- renderWordcloud2({
    dane <- podsumowanie_ewa1()
    wordcloud2(dane, backgroundColor = '#EBF0F6', shape = 'circle', size = 0.7)
  })
  
  output$plot_e1 <- renderPlotly({
    
    temp <- podsumowanie_ewa2()
    
    g <-ggplot(data = temp, aes(x = Godzina, y = Ile)) +
        geom_bar(aes(fill = Autor), position = 'dodge2', stat = "identity", width = 1) +
        geom_hline(yintercept = mean(temp[which(temp$Autor == '1. Ewa'),]$Ile), colour = '#F8766D', alpha = 0.8, size = 1.5) + 
        geom_hline(yintercept = mean(temp[which(temp$Autor != '1. Ewa'),]$Ile), colour = '#00BEC3', alpha = 0.8, size = 1.5) + 
        theme_minimal() +
        theme(axis.line = element_line(color = 'black'), 
              panel.grid.major.x = element_blank(), 
              panel.background = element_rect(fill = '#EBF0F6'),
              plot.background = element_rect(fill = '#EBF0F6'),
              legend.title = element_blank(),
              legend.text = element_text(size = 10),
              axis.text.x = element_text(angle = 45, size = 10, hjust = 1, face = 'bold'), 
              axis.text.y = element_text(size = 10, face = 'bold'),
              axis.title.x = element_text(size = 15, face = 'bold'),
              axis.title.y = element_text(size = 15, face = 'bold'),
              plot.title = element_text(size = 20, face = 'bold', hjust = 0.5)) +
        labs(title = 'Dzienna aktywność', 
             x = 'Godzina',
             y = 'Liczba wiadomości') 
    
    ggplotly(g)
    
  })
  
  output$plot_e2 <- renderPlotly({
    
    temp <- podsumowanie_ewa3()
    
    g <- ggplot(data = temp, aes(x = Data, y = Ile)) +
          geom_bar(aes(fill = Autor), position = 'dodge2', stat = "identity", width = 1) +
          geom_hline(yintercept = mean(temp[which(temp$Autor == '1. Ewa'),]$Ile), colour = '#F8766D', alpha = 0.8, size = 1.5) + 
          geom_hline(yintercept = mean(temp[which(temp$Autor != '1. Ewa'),]$Ile), colour = '#00BEC3', alpha = 0.8, size = 1.5) + 
          theme_minimal() +
          theme(axis.line = element_line(color = 'black'), 
                panel.grid.major.x = element_blank(), 
                panel.background = element_rect(fill = '#EBF0F6'),
                plot.background = element_rect(fill = '#EBF0F6'),
                legend.title = element_blank(),
                legend.text = element_text(size = 10),
                axis.text.x = element_text(angle = 45, size = 10, hjust = 1, face = 'bold'), 
                axis.text.y = element_text(size = 10, face = 'bold'),
                axis.title.x = element_text(size = 15, face = 'bold'),
                axis.title.y = element_text(size = 15, face = 'bold'),
                plot.title = element_text(size = 20, face = 'bold', hjust = 0.5)) +
          labs(title = 'Historia Relacji', 
               x = 'Data',
               y = 'Liczba wiadomości') 
    
    ggplotly(g)
    
  })
  
  
  
  output$wordcloud_m <- renderWordcloud2({
    dane <- podsumowanie_michal1()
    wordcloud2(dane, backgroundColor = '#EBF0F6', shape = 'circle', size = 0.7)
  })
  
  output$plot_m1 <- renderPlotly({
    
    temp <- podsumowanie_michal2()
    
    g <-ggplot(data = temp, aes(x = Godzina, y = Ile)) +
      geom_bar(aes(fill = Autor), position = 'dodge2', stat = "identity", width = 1) +
      geom_hline(yintercept = mean(temp[which(temp$Autor == '1. Michał'),]$Ile), colour = '#F8766D', alpha = 0.8, size = 1.5) + 
      geom_hline(yintercept = mean(temp[which(temp$Autor != '1. Michał'),]$Ile), colour = '#00BEC3', alpha = 0.8, size = 1.5) + 
      theme_minimal() +
      theme(axis.line = element_line(color = 'black'), 
            panel.grid.major.x = element_blank(), 
            panel.background = element_rect(fill = '#EBF0F6'),
            plot.background = element_rect(fill = '#EBF0F6'),
            legend.title = element_blank(),
            legend.text = element_text(size = 10),
            axis.text.x = element_text(angle = 45, size = 10, hjust = 1, face = 'bold'), 
            axis.text.y = element_text(size = 10, face = 'bold'),
            axis.title.x = element_text(size = 15, face = 'bold'),
            axis.title.y = element_text(size = 15, face = 'bold'),
            plot.title = element_text(size = 20, face = 'bold', hjust = 0.5)) +
      labs(title = 'Dzienna aktywność', 
           x = 'Godzina',
           y = 'Liczba wiadomości') 
    
    ggplotly(g)
    
  })
  
  output$plot_m2 <- renderPlotly({
    
    temp <- podsumowanie_michal3()
    
    g <- ggplot(data = temp, aes(x = Data, y = Ile)) +
      geom_bar(aes(fill = Autor), position = 'dodge2', stat = "identity", width = 1) +
      geom_hline(yintercept = mean(temp[which(temp$Autor == '1. Michał'),]$Ile), colour = '#F8766D', alpha = 0.8, size = 1.5) + 
      geom_hline(yintercept = mean(temp[which(temp$Autor != '1. Michał'),]$Ile), colour = '#00BEC3', alpha = 0.8, size = 1.5) + 
      theme_minimal() +
      theme(axis.line = element_line(color = 'black'), 
            panel.grid.major.x = element_blank(), 
            panel.background = element_rect(fill = '#EBF0F6'),
            plot.background = element_rect(fill = '#EBF0F6'),
            legend.title = element_blank(),
            legend.text = element_text(size = 10),
            axis.text.x = element_text(angle = 45, size = 10, hjust = 1, face = 'bold'), 
            axis.text.y = element_text(size = 10, face = 'bold'),
            axis.title.x = element_text(size = 15, face = 'bold'),
            axis.title.y = element_text(size = 15, face = 'bold'),
            plot.title = element_text(size = 20, face = 'bold', hjust = 0.5)) +
      labs(title = 'Historia Relacji', 
           x = 'Data',
           y = 'Liczba wiadomości') 
    
    ggplotly(g)
    
  })
  
  
  
}

############################################################################################################

shinyApp(ui = ui, server = server)

############################################################################################################
