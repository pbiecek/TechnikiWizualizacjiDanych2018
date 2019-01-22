
require(shiny)
require(shinydashboard)
require(SmarterPoland)
require(ggplot2)
require(dplyr)
require(readxl)
require(hablar)



dane10 <- read_excel('PM10.xlsx', skip = 1)
dane25 <- read_excel('PM25.xlsx', skip = 1)
Kody10 <- c('MzWarAlNiepo', 'MpKrakTelime', 'LdLodzRudzka', 'DsWrocOrzech', 'WpPoznChwial', 'PmGdyJozBema', 'ZpSzczecPrze', 'KpBydPlPozna', 'LbLubSliwins', 'PdBialWaszyn')
Kody25 <- c('MzWarWokalna', 'MpKrakBujaka', 'LdLodzLegion', 'DsWrocNaGrob', 'WpPoznPolank', 'PmGdaPowWiel', 'ZpSzczAndr01', 'KpBydBerling', 'LbLubSliwins', 'PdBialWarsza')
Miasta <- c('Warszawa', 'Kraków', 'Łódź', 'Wrocław', 'Poznań', 'Gdańsk', 'Szczecin', 'Bydgoszcz', 'Lublin', 'Białystok')
Dni = seq(from = as.Date('2017-01-01'), to = as.Date('2017-12-31'), by = 'day')
Stat <- c('Średnia', 'Minimum', 'Maksimum')
TN <- c('NIE', 'TAK')

PM10 <- data.frame(Data = Dni)
PM25 <- data.frame(Data = Dni)

for (i in 1:10){
  
  temp <- dane10[,Kody10[i]]
  temp <- temp[-(1:4),] %>% retype()
  temp <- round(temp, 2)
  colnames(temp) <- Miasta[i]
  PM10 <- cbind(PM10, temp)
  
  temp <- dane25[,Kody25[i]]
  temp <- temp[-(1:4),] %>% retype()
  temp <- round(temp, 2)
  colnames(temp) <- Miasta[i]
  PM25 <- cbind(PM25, temp)
  
}

stat10 <- t(apply(PM10[,-1], 2 , function(x) round(c(mean(x, na.rm = TRUE), min(x, na.rm = TRUE), max(x, na.rm = TRUE)), 2)))
stat10 <- cbind(Miasta, stat10, rep('PM 10', 10))
colnames(stat10) <- c('Miasta', 'Średnia', 'Minimum', 'Maksimum', 'PM')
row.names(stat10) <- NULL
stat10 <- data.frame(stat10) %>% retype()

stat25 <- t(apply(PM25[,-1], 2 , function(x) round(c(mean(x, na.rm = TRUE), min(x, na.rm = TRUE), max(x, na.rm = TRUE)), 2)))
stat25 <- cbind(Miasta, stat25, rep('PM 2.5', 10))
colnames(stat25) <- c('Miasta', 'Średnia', 'Minimum', 'Maksimum', 'PM')
row.names(stat25) <- NULL
stat25 <- data.frame(stat25) %>% retype()

stat <- rbind(stat10, stat25)



ui <- dashboardPage(
  
  skin = 'red',
  
  dashboardHeader(title = 'MENU'),
  
  dashboardSidebar(
    
    sidebarMenu(
      
      h2('Wykres roczny: '),
      
      selectizeInput(inputId = 'chosen_city_1',
                     label = 'Wybierz I miasto: ',
                     choices = Miasta),
      
      selectizeInput(inputId = 'chosen_city_2',
                     label = 'Wybierz II miasto: ',
                     choices = c(Miasta[2], Miasta[1], Miasta[3:10])),
      
      h2('Wykres słupkowy: '),
      
      selectizeInput(inputId = 'chosen_stat',
                     label = 'Rodzaj statystyki: ',
                     choices = Stat),
      
      checkboxGroupInput(inputId = 'chosen_citys',
                         label = 'Wybierz miasta: ',
                         choices = unique(Miasta),
                         selected = unique(Miasta)),
      
      h2('Wyświetlić opis: '),
      
      selectizeInput(inputId = 'chosen_describe',
                     label = 'Wyświetlić opis: ',
                     choices = TN)
      
    )
  ),
  
  
  
  dashboardBody(
    
    fluidPage(
      
      h1('Zanieczyszczenie powietrza w 10 największych miastach w Polsce'),
      
      h1(' '),
      
      textOutput('describe'),
      
      h1(' '),
      
      fluidRow(column(5, plotOutput('distPlot1')), column(5, plotOutput('distPlot2'))),
      
      h1(' '),
      
      fluidRow(column(10, plotOutput('distPlot3')))
      
    )
    
  )
  
)



server <- function(input, output) {
  
  output[['distPlot1']] <- renderPlot({
    
    city1 <- input[['chosen_city_1']]
    city2 <- input[['chosen_city_2']]
    
    pomiary <- cbind(PM25[,c('Data', city1)], PM25[ ,city2])
    colnames(pomiary) <- c('Data', 'Wartosc1', 'Wartosc2')
    
    ggplot(pomiary, aes(x = Data)) +
      geom_line(aes(y = Wartosc1, colour = city1)) +
      geom_line(aes(y = Wartosc2, colour = city2)) +
      geom_hline(yintercept = 25, colour = 'blue', alpha = 0.8, size = 1) +
      theme_minimal() +
      theme(axis.line = element_line(color = 'black'), 
            panel.grid.major.x = element_blank(), 
            legend.title = element_blank(),
            legend.text = element_text(size = 10),
            axis.text.x = element_text(angle = 45, size = 10, hjust = 1, face = 'bold'), 
            axis.text.y = element_text(size = 10, face = 'bold'),
            axis.title.x = element_text(size = 15, face = 'bold'),
            axis.title.y = element_text(size = 15, face = 'bold'),
            plot.title = element_text(size = 20, face = 'bold', hjust = 0.5)) +
      labs(title = 'Pomiar stężenia PM 2.5 w powietrzu', x = 'Data pomiaru', y = 'Wartość PM 2.5 [ug/m3]')
    
  })
  
  
  
  output[['distPlot2']] <- renderPlot({
    
    city1 <- input[['chosen_city_1']]
    city2 <- input[['chosen_city_2']]
    
    pomiary <- cbind(PM10[,c('Data', city1)], PM10[ ,city2])
    colnames(pomiary) <- c('Data', 'Wartosc1', 'Wartosc2')
    
    ggplot(pomiary, aes(x = Data)) +
      geom_line(aes(y = Wartosc1, colour = city1)) +
      geom_line(aes(y = Wartosc2, colour = city2)) +
      geom_hline(yintercept = 50, colour = 'blue', alpha = 0.8, size = 1) +
      theme_minimal() +
      theme(axis.line = element_line(color = 'black'), 
            panel.grid.major.x = element_blank(), 
            legend.title = element_blank(),
            legend.text = element_text(size = 10),
            axis.text.x = element_text(angle = 45, size = 10, hjust = 1, face = 'bold'), 
            axis.text.y = element_text(size = 10, face = 'bold'),
            axis.title.x = element_text(size = 15, face = 'bold'),
            axis.title.y = element_text(size = 15, face = 'bold'),
            plot.title = element_text(size = 20, face = 'bold', hjust = 0.5)) +
      labs(title = 'Pomiar stężenia PM 10 w powietrzu', x = 'Data pomiaru', y = 'Wartość PM 10 [ug/m3]')
    
  })
  
  
  
  output[['distPlot3']] <- renderPlot({
    
    citys <- input[['chosen_citys']]
    choise <- input[['chosen_stat']]
    
    temp <- stat %>% filter(Miasta %in% citys) %>% select(Miasta, choise, PM)
    colnames(temp) <- c('Miasta','S','PM')
    
    ggplot(data = temp, aes(x = Miasta, y = S), fill = PM) +
      geom_bar(aes(fill = factor(PM)), position = 'dodge', stat = 'identity', width = 0.5) + 
      theme_minimal() +
      theme(axis.line = element_line(color = 'black'), 
            panel.grid.major.x = element_blank(), 
            legend.title = element_blank(),
            legend.text = element_text(size = 10),
            axis.text.x = element_text(angle = 45, size = 10, hjust = 1, face = 'bold'), 
            axis.text.y = element_text(size = 10, face = 'bold'),
            axis.title.x = element_text(size = 15, face = 'bold'),
            axis.title.y = element_text(size = 15, face = 'bold'),
            plot.title = element_text(size = 20, face = 'bold', hjust = 0.5)) +
      labs(title = paste('Zanieczyszczenie powietrza -', tolower(choise), 'z całego roku'), x = 'Miasto', y = 'Stężenie [ug/m3]')
    
  })
  
  
  
  output[['describe']] <- renderText({
    
    if (input[['chosen_describe']] == 'TAK'){
      
      'Aplikacja pozwala poznać stan powietrza, a dokładnie stężenie pyłków zawieszonych PM 2.5 oraz PM 10 w 10 największych (pod względem mieszkańców) miastach Polski w 2017 roku.
      Na pierwszych 2 wykresach można porównywać pomiary stężenia obu pyłków zawiszonych w powietrzu dla 2 wybranych miast.
      Dodadkowo niebieską linią zaznaczone są dopuszczalne stężenia pyłków.
      Natomiast wykres słupkowy prezentuje dla wybranych miast roczne zestawienie (średnia, minimum, maksimum) pomiarów stężenia.'
      
    }
    
  })
  
}



shinyApp(ui = ui, server = server)
