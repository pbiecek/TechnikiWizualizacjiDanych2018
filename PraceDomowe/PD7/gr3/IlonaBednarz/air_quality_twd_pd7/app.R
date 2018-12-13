library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(plotly)
library(countrycode) # otrzymanie kontynentow z nazw panstw

### dane WHO
cities <- read.table('world_cities_pollution.csv', sep = '|', header = TRUE, na.strings = c('n/a', 'NA', 'N/A', '', '<NA>'))
cities$nr_and_type_of_monitoring_stations <- tolower(cities$nr_and_type_of_monitoring_stations)
cities['area_type'] <-  sapply(cities$nr_and_type_of_monitoring_stations, function(x) {
  for (name in c('urban', 'industrial', 'rural', 'residential', 'roadside')) {
    if (grepl(name, x, fixed = TRUE)) return(name)
  }
  return('no info')
})

countries <- read.table('world_countries_pollution.csv', sep = '|', header = TRUE, na.strings = c('n/a', 'NA', 'N/A', '#N/A', ''))

df <- merge(cities, countries, by = 'Country', all.x = TRUE, suffixes = c('_city','_country'))
df$continent <- countrycode(sourcevar = df$Country, origin = "country.name", destination = "continent")
df$continent[df$Region_city %in% c('Amr')] <- 'South America'
df$continent[df$Region_city %in% c('AmrHI')] <- 'North America'
df$continent[df$Country %in% c('Chile', 'Uruguay')] <- 'South America'
df <- df[,c(1,4,5,8,14,16,18,22)]

countries <- merge(countries, df[,c('Country', 'continent')], by = 'Country', all.x = TRUE) %>% unique()

bar1 <- df %>% arrange(desc(PM10_annual_mean_ug.m3_city))
bar1$city_country <- paste(bar1$City.station, bar1$Country, sep = ', ')

bar2 <- countries %>% arrange(desc(PM10_annual_mean_ug.m3))

### dane polska
pol <- read.table('polska_pm10.csv', sep = '|', header = TRUE, na.strings = c('n/a', 'NA', 'N/A', '', '<NA>'))
pol <- pol[pol$Czas.uśredniania == '24g' & as.numeric(pol$Rok) >= 2005,]
pol <- pol[,c(1,2,4,5,8:10)]

pol_woj <- pol %>% group_by(Rok, Woj) %>% summarize(woj_srednia = mean(Średnia)) %>% arrange(Rok, desc(woj_srednia))
kraj_srednia <- pol_woj %>% group_by(Rok) %>% summarize(kraj_srednia = mean(woj_srednia))
pol_woj <- merge(pol_woj, kraj_srednia, by = 'Rok', all.x = TRUE) %>% unique() 

ui <- dashboardPage(
  skin = "black",
  dashboardHeader(title = "Jakość powietrza"),
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("Świat - miasta", tabName = "swiat_miasta", icon = icon("dashboard")),
      menuItem("Świat - państwa", tabName = "swiat_panstwa", icon = icon("dashboard")),
      menuItem("Polska", tabName = "polska", icon = icon("home")),
      menuItem("O danych", icon = icon("info-circle"), tabName = "about")
    )
  ),
  
  dashboardBody(
    tabItems(
      
      tabItem("swiat_panstwa",
              fluidPage(
                titlePanel(""),
                sidebarLayout(
                  sidebarPanel(
                    checkboxGroupInput(inputId = "choose_continent_panstwa",
                                       label = "Wybierz kontynenty:",
                                       choiceNames = c("Asia","Africa","Soutch America","South America","Europe","North America","Oceania"),
                                       choiceValues = c("Asia","Africa","Soutch America","South America","Europe","North America","Oceania"),
                                       selected = c("Asia","Africa","Soutch America","South America","Europe","North America","Oceania")
                    ),
                    textOutput("caption_panstwa")
                  ),
                  mainPanel(
                    plotOutput("p1_panstwa", height="800px")
                  )
                )
              )
      ),
      
      tabItem("swiat_miasta",
              fluidPage(
                titlePanel(""),
                sidebarLayout(
                  sidebarPanel(
                    checkboxGroupInput(inputId = "choose_continent",
                                       label = "Wybierz kontynenty:",
                                       choiceNames = c("Asia","Africa","Soutch America","South America","Europe","North America","Oceania"),
                                       choiceValues = c("Asia","Africa","Soutch America","South America","Europe","North America","Oceania"),
                                       selected = c("Asia","Africa","Soutch America","South America","Europe","North America","Oceania")
                    ),
                    textOutput("caption")
                  ),
                  mainPanel(
                    plotOutput("p1", height="800px")
                  )
                )
              )
      ),
      
      tabItem("polska",
              fluidPage(
                sidebarLayout(position = "left",
                              sidebarPanel(
                                           radioButtons("choose_rok", "Wybierz rok:",
                                                        choices = list("2017" = '2017',"2016" = '2016',"2015" = '2015',"2014" = '2014',"2013" = '2013',"2012" = '2012',
                                                                       "2011" = '2011',"2010" = '2010',"2009" = '2009',"2008" = '2008',"2007" = '2007',"2006" = '2006'),
                                                        selected = '2017')
                                           ),
                              mainPanel(
                                plotOutput("polska1")
                              )
                )
              )
      ),
      
      tabItem("about",
              "Aplikacja przedstawia dane o jakości powietrza na świecie i w Polsce, biorąc pod uwagę zawartość pyłu PM10 w powietrzu. Jest to pył
              posiadający najmniejsze cząsteczki, a więc najbardziej szkodliwy dla zdrowia. Źródła wykorzystanych danych są dostępne pod odpowiednimi przyciskami.
              Dane światowe pochodzą z 2012 roku, natomiast dane o Polsce z lat 2006-2017",
              actionButton(inputId='ab1', label="Źródło danych światowych", 
                           icon = icon("th"),
                           onclick ="window.open('https://www.who.int/quantifying_ehimpacts/national/countryprofile/en/', '_blank')"),
              actionButton(inputId='ab1', label="Źródło danych o Polsce", 
                           icon = icon("th"),
                           onclick ="window.open('http://powietrze.gios.gov.pl', '_blank')")
      )
      
    )
  )
)

server <- function(input, output) {
  
  bar11 <- reactive({
    bar1[bar1$continent %in% input[["choose_continent"]],] %>%
      slice(1:50)
  })
  
  output[['p1']] <- renderPlot(
    ggplot(bar11(), aes(x = city_country, y = PM10_annual_mean_ug.m3_city)) +
      theme_minimal() +
      scale_x_discrete(limits = rev(bar11()$city_country)) +
      geom_bar(aes(fill = continent), stat = 'identity') +
      geom_hline(yintercept = 35, color = '#636363', linetype="dashed", size = 0.2) +
      geom_text(aes(7, 35, label = "Poland, Warsaw", angle = 270), size = 5, color = '#636363') +
      geom_hline(yintercept = 60, color = '#636363', linetype="dashed", size = 0.2) +
      geom_text(aes(22, 60, label = "Poland, Sosnowiec", angle = 270), size = 5, color = '#636363') +
      labs(x = "", y = "PM 10 annual mean ug/m^3") +
      theme(legend.position = "top",
            legend.title = element_blank(),
            legend.text = element_text(size = 13, margin = margin(r = 25, l = 5)),
            axis.text.x = element_text(size = 13),
            axis.text.y = element_text(size = 13),
            axis.title.x = element_text(size = 13),
            plot.title = element_text(size = 30)) +
      ggtitle('Miasta na świecie o największej emisji pyłu PM10\n(50 najbardziej zanieczyszczonych miast)') +
      coord_flip()
  )
  
  output[['caption']] <- renderText('Zauważmy, że większość miast w pierwszej pięćdziesiątce leży w Indiach
                                    i ich okolicach. W pierwszej pięćdziesiątce najbardziej zanieczyszczonych miast zdecydowanie króluje Azja.
                                    Po odznaczeniu Azji na wykresie pojawiają się nowe kontynenty.\n\n
                                    Gdy wybierzemy samą Europę,
                                    zauważymy, że \'wygrywa\' Bułgaria, ale polski Sosnowiec znajduje się już na 5. miejscu, a tuż
                                    za nim kolejne polskie miasta,
                                    co nie napełnia optymizmem.\n\n
                                    Zdecydowanie najczystszym kontynentem jest Oceania')
  
  bar_panstwa <- reactive({
    bar2[bar2$continent %in% input[["choose_continent_panstwa"]],] %>%
      slice(1:50)
  })
  
  output[['p1_panstwa']] <- renderPlot(
    ggplot(bar_panstwa(), aes(x = Country, y = PM10_annual_mean_ug.m3)) +
      theme_minimal() +
      scale_x_discrete(limits = rev(bar_panstwa()$Country)) +
      geom_bar(aes(fill = continent), stat = 'identity') +
      geom_hline(yintercept = 39, color = '#636363', linetype="dashed", size = 0.2) +
      geom_text(aes(3, 39, label = "Poland", angle = 270), size = 5, color = '#636363') +
      labs(x = "", y = "PM 10 annual mean ug/m^3") +
      theme(legend.position = "top",
            legend.title = element_blank(),
            legend.text = element_text(size = 13, margin = margin(r = 25, l = 5)),
            axis.text.x = element_text(size = 13),
            axis.text.y = element_text(size = 13),
            axis.title.x = element_text(size = 13),
            plot.title = element_text(size = 30)) +
      ggtitle('Państwa na świecie o największej emisji pyłu PM10\n(50 najbardziej zanieczyszczonych państw)') +
      coord_flip()
  )
  
  output[['caption_panstwa']] <- renderText('Zauważmy, że najwięcej bardzo zanieczyszczonych krajów leży w Azji.
                                            Europa pojawia się dopiero na 27. miescu, a Polska na 41. (spośród 91 krajów, dla których posiadamy dane).')
 
  woj <- reactive({
    pol_woj[pol_woj$Rok == input[['choose_rok']],]
  })

  output[['polska1']] <- renderPlot(
    ggplot(woj(), aes(x = Woj, y = woj_srednia)) +
      theme_minimal() +
      scale_x_discrete(limits = rev(woj()$Woj)) +
      geom_bar(stat = 'identity') +
      geom_hline(yintercept = woj()$kraj_srednia[1], linetype="dashed", size = 0.2) +
      geom_text(aes(4, woj()$kraj_srednia[1], label = "Polska - średnia", angle = 270), size = 5) +
      labs(x = "", y = "średnia roczna emisja PM10 ug/m^3") +
      theme(legend.position = "top",
            legend.title = element_blank(),
            legend.text = element_text(size = 13, margin = margin(r = 25, l = 5)),
            axis.text.x = element_text(size = 13),
            axis.text.y = element_text(size = 13),
            axis.title.x = element_text(size = 13),
            plot.title = element_text(size = 30)) +
      ggtitle(paste('Emisja pyłu PM10 w Polsce w roku',woj()$Rok[1])) +
      coord_flip()
    )
}

shinyApp(ui, server)
