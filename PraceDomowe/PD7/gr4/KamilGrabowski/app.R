library(shinydashboard)
library(ggplot2)
library(dplyr)
library(scales)

df <- read.csv('pd7.csv')
df$weekday = as.integer(factor(df$weekday, levels=c("poniedziałek", "wtorek", "środa", "czwartek", "piątek", "sobota", "niedziela")))
df$month = as.integer(factor(df$month, levels=c("styczeń", "luty", "marzec", "kwiecień", "maj", "czerwiec", "lipiec", "sierpień", "wrzesień", "październik",
                                                "listopad", "grudzień")))


ui <- dashboardPage(
  dashboardHeader(title = "PM10 w 2017 roku"),
  dashboardSidebar(
    sidebarMenu(
      # Setting id makes input$tabs give the tabName of currently-selected tab
      id = "tabs",
      menuItem("Godziny", tabName = "Godziny", icon = icon("dashboard")),
      menuItem("Dni", tabName = "Dni", icon = icon("dashboard")),
      menuItem("Miesiace", tabName = "Miesiace", icon = icon("dashboard"))

    )
  ),
  dashboardBody(
    # Boxes need to be put in a row (or column)
    # fluidRow(
    #   box(plotOutput("plot1", height = 250)),
    # 
    #   box(
    #     title = "Wybierz okres doby:",
    #     sliderInput("slider", "Godziny:", min = 1, max = 24, value = c(1, 24))
    #   )
    # )
    # ,
    tabItems(
    tabItem("Godziny",
            fluidRow(
              box(plotOutput("plot1", height = 250)),
              
              box(
                title = "Wybierz okres doby:",
                sliderInput("slider", "Godziny:", min = 1, max = 24, value = c(1, 24))
              )
            )
      ),
    tabItem("Dni",
            fluidRow(
              box(plotOutput("plot2", height = 250)),
              
              box(
                title = "Wybierz dzień tygodnia:",
                sliderInput("slider2", "Dni:", min = 1, max = 7, value = c(1, 7))
              )
            )
    ),
    tabItem("Miesiace",
            fluidRow(
              box(plotOutput("plot3", height = 250)),
              
              box(
                title = "Wybierz miesiąc:",
                sliderInput("slider3", "Miesiące:", min = 1, max = 12, value = c(1, 12))
              )
            )
    )
    )
  )
)

server <- function(input, output) {
  set.seed(122)
  
  df_r <- reactive({
    df %>%filter( hour >= input[['slider']][1] & hour <= input[['slider']][2]) %>% group_by(hour) %>%
      dplyr::summarize(mean_sum = mean(sum, na.rm=TRUE))
  })
  
  output$plot1 <- renderPlot({
    ggplot(data = df_r(), aes(x = hour, y = mean_sum)) +
      geom_bar(stat='identity', position = 'dodge')+
      ggtitle("PM10 w 2017 roku dla godzin") +
      ylab("średni PM10 (wartość godzinna)")+
      xlab("godzina")
      # theme(plot.title = element_text(hjust = 0.5),
      #       axis.title.x=element_text("years"),
      #       axis.title.y = element_text("spent in millions USD"),
      #       axis.text.x = element_text(hjust = 0.5, colour = 'darkgray') )
  })
  
  df_d <- reactive({
    df %>% filter( weekday >= input[['slider2']][1] & weekday <= input[['slider2']][2]) %>%group_by(weekday) %>%
      dplyr::summarize(mean_sum = mean(sum, na.rm=TRUE))
  })
  
  output$plot2 <- renderPlot({
    ggplot(data = df_d(), aes(x = weekday, y = mean_sum)) +
      geom_bar(stat='identity', position = 'dodge')+
      ggtitle("PM10 w 2017 roku dla dni tygodnia") +
      scale_x_continuous(breaks= pretty_breaks())+
      ylab("średni PM10 (wartość godzinna)")+
      xlab("dzień tygodnia")
  })
    
    df_m <- reactive({
      df %>% filter( month >= input[['slider3']][1] & month <= input[['slider3']][2]) %>%group_by(month) %>%
        dplyr::summarize(mean_sum = mean(sum, na.rm=TRUE))
    })

    output$plot3 <- renderPlot({
      ggplot(data = df_m(), aes(x = month, y = mean_sum)) +
        geom_bar(stat='identity', position = 'dodge')+
        ggtitle("PM10 w 2017 roku dla miesięcy") +
        scale_x_continuous(breaks= pretty_breaks())+
        ylab("średni PM10 (wartość godzinna)")+
        xlab("miesiąc")

    # theme(plot.title = element_text(hjust = 0.5),
    #       axis.title.x=element_text("years"),
    #       axis.title.y = element_text("spent in millions USD"),
    #       axis.text.x = element_text(hjust = 0.5, colour = 'darkgray') )
  })
}

shinyApp(ui, server)