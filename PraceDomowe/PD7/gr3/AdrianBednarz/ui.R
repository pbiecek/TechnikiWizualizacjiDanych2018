library(shiny)
library(ggplot2)

dataset <- read.csv("data.csv", header = TRUE)

navbarPage("Pollution in Poland",
  tabPanel("Weekly",
     fluidRow(
       column(4, wellPanel(
         checkboxGroupInput("locations", "Locations", sort(unique(dataset$name)), selected = sort(unique(dataset$name))[[1]]),
         radioButtons("elements", "Particle", sort(unique(dataset$element)))
       )),
       column(8, plotOutput("weekly")),
       column(4, 
        p("Even though a foreigner might think that major source of pollution in Poland are factories, 
          these time series show that regardless of analyzed place there is higher pollution in winter period. 
          It is well known among Poles that the society is harming itself by releasing toxic substances into the atmosphere. 
          It's mostly due to single family households combustion of coal and litter.
          The difference is best seen for SO2. It is worth noting that in northern part of Poland the pollution is significantly less severe.")
       )
     )         
  ),
  tabPanel(
    "Yearly",
    fluidRow(
      column(4, wellPanel(
        radioButtons("elements2", "Particle", sort(unique(dataset$element)))
      )),
      column(8, plotOutput("yearly")),
      column(4, p("This plot shows averaged emission across the country. Kraków seems to be the most polluted city analyzed regardless of the particle. 
                  In contrary, for Gdańsk there is only notable level of NO2. It has extraordinarily clean air compared to the rest of analyzed cities.
                  For other cities pollution level has similar level averaged throughout the year."))
    )         
  ),
  tabPanel(
    "Monthly",
    fluidRow(
      column(4, wellPanel(
        selectInput("compare1", "First location", sort(unique(dataset$city)), sort(unique(dataset$city))[[1]]),
        selectInput("compare2", "Second location", sort(unique(dataset$city)), sort(unique(dataset$city))[[2]]),
        radioButtons("elements3", "Particle", sort(unique(dataset$element)))
      )),
      column(8, plotOutput("monthly")),
      column(4, p("This plot shows the difference in particle concentration between cities by month.
                  When it comes to NO2 pollution Kraków has no equal. In every month of 2017 the mean level of pollution was higher than in any other city.
                  What is also interesting - Rzeszów seems to be cleaner than Warsaw in terms of NO2 and PM10 month by month. 
                  This might be the reason why air in this part of Poland is perceived as fresh despite being polluted too."))
      
    )
  )
)
