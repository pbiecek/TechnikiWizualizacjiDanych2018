library(plotly)
library(dplyr)
library(ggplot2)
library(shinydashboard)
library(RColorBrewer)
library(shiny)

dfs = NULL
agg = NULL
cities = NULL
wojew = NULL
citiesFull=NULL
wojewFull=NULL
metadata=read.csv('data/metadata.csv', encoding = 'UTF-8')

for (i in seq(2000, 2017)) {
  agg[[i]] = read.csv(paste('data/', i, '-agg.csv', sep=''), encoding = 'UTF-8')
  cities[[i]] = read.csv(paste('data/', i, '-cities.csv', sep=''), encoding = 'UTF-8')
  wojew[[i]] = read.csv(paste('data/', i, '-wojew.csv', sep=''), encoding = 'UTF-8')
  citiesFull <- rbind(citiesFull, cities[[i]] %>%
                        na.omit() %>%
                        mutate(group=cut(mean, breaks=c(0, 20, 40, 60, 80, +Inf), labels = c('0-20', '20-40', '40-60', '60-80', '80+'), na.rm=T)))
  wojewFull <- rbind(wojewFull, wojew[[i]] %>%
                        na.omit() %>%
                        mutate(group=cut(mean, breaks=c(0, 20, 40, 60, 80, +Inf), labels = c('0-20', '20-40', '40-60', '60-80', '80+'), na.rm=T)))
}

g <- list(
  scope = 'europe',
  showland = TRUE,
  landcolor = toRGB("gray95"),
  subunitcolor = toRGB("gray85"),
  countrycolor = toRGB("gray85"),
  countrywidth = 0.5,
  subunitwidth = 0.5,
  lonaxis = list(range = c(14, 24)),
  lataxis = list(range = c(49, 55))
)

ui <- dashboardPage(
  skin = "black",
  dashboardHeader(title = "Jakość powietrza w Polsce w latach 2000-2017", titleWidth = '100%'),
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("Mapa", tabName = "map", icon = icon("map")),
      menuItem("Miasta", tabName = 'cities', icon=icon('city')),
      menuItem("Wojewódstwa", tabName = 'region', icon=icon('flag'))
    ),
    sliderInput('yearSlider', 'Rok', min=2000, max=2017, value=2017)
  ),
  dashboardBody(
    tabItems(
      tabItem("map",
              fluidPage(
                titlePanel('Średnia jakość powietrza w ciągu roku'),
                plotlyOutput("map_plot")
              )
      ),
      tabItem('cities',
              fluidPage(
                titlePanel('Zmiana poziomu PM-10 w ciągu roku w miastach'),
                selectizeInput('citiesInput', 'Miasta', c('Warszawa', 'Łódź', 'Kraków', 'Katowice', 'Poznań')),
                checkboxInput('cityFull', label = 'Dane za wszystkie lata', value = F),
                plotOutput('time_plot')
              )
      ),
      tabItem('region',
              fluidPage(
                titlePanel('Zmiana poziomu PM-10 w ciągu roku w wojewódstwach'),
                selectizeInput('wojewInput', 'Województwa', unique(metadata$Wojewodstwo)),
                checkboxInput('wojewFull', label = 'Dane za wszystkie lata', value = F),
                plotOutput('wojew_plot')
              )
      )
    )
  )
)

colorscale=c('#1a9850', '#91cf60', '#d9ef8b', '#fee08b', '#fc8d59', '#d73027')

server <- function(input, output) {
  
  cty = reactive({
    cities[[input[['yearSlider']]]][cities[[input[['yearSlider']]]][1] == input[['citiesInput']],] %>%
      na.omit() %>%
      mutate(group=cut(mean, breaks=c(0, 20, 40, 60, 80, +Inf), labels = c('0-20', '20-40', '40-60', '60-80', '80+'), na.rm=T))
  })
  
  ctyFull = reactive({
    citiesFull[citiesFull[1] == input[['citiesInput']],]
  })
  
  wjw = reactive({
    wojew[[input[['yearSlider']]]][wojew[[input[['yearSlider']]]][1] == input[['wojewInput']],] %>%
      na.omit() %>%
      mutate(group=cut(mean, breaks=c(0, 20, 40, 60, 80, +Inf), labels = c('0-20', '20-40', '40-60', '60-80', '80+'), na.rm=T))
  })
  
  wjwFull = reactive({
    wojewFull[wojewFull[1] == input[['wojewInput']],]
  })
  
  
  output[["map_plot"]] <- renderPlotly(
    plot_geo(agg[[input[['yearSlider']]]], lat = ~lat, lon = ~lon, colors=colorscale) %>%
      add_markers(
        text = ~paste(city, mean),
        color = ~mean,
        hoverinfo = "text"
      ) %>%
      colorbar(title = "Średnia wartość PM-10", limits = c(0, 85)) %>%
      layout(
        geo = g
      )
  )
  
  output[['time_plot']] <- renderPlot(
    (if (input[['cityFull']] == T) ctyFull() else cty()) %>%
      ggplot(., aes(x=as.Date(date), y=mean)) +
      geom_point(aes(colour=group))+
      scale_colour_manual(values = rev(brewer.pal(5,"RdYlGn"))) +
      
      geom_smooth(method = "auto") +
      xlab('Data') + ylab('PM-10')
  )
  
  output[['wojew_plot']] <- renderPlot(
    (if (input[['wojewFull']] == T) wjwFull() else wjw()) %>%
      ggplot(., aes(x=as.Date(date), y=mean)) +
      geom_point(aes(colour=group))+
      scale_colour_manual(values = rev(brewer.pal(5,"RdYlGn"))) +
      geom_smooth(method = "auto") +
      xlab('Data') + ylab('PM-10')
  )
}

shinyApp(ui, server)


