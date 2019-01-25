library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(plotly)
library(patchwork)

### dane
df <- read.table('yt_history.csv', sep='|', header = TRUE)
df$year <- substr(df$date,1,4)
df$month <- as.Date(paste0(substr(as.character(df$date),1,6),'01'), format = '%Y%m%d')
df$date <- as.Date(x=as.character(df$date), format = '%Y%m%d')

# liczba obejrzanych filmow dziennie, miesiecznie
df_dzienne <- df %>% group_by(date) %>% summarise(count=n())
df_dzienne$type <- 'dzienne'
df_miesieczne <- df %>% group_by(month) %>% summarise(count=n())
names(df_miesieczne)[1] <- 'date'
df_miesieczne$type <- 'miesieczne'
df_pl <- rbind(df_dzienne,df_miesieczne)

# rozklad ogladanych kanalow
channels <- data.frame(sort(table(df$channel_id), decreasing = TRUE))
names(channels) <- c('Channel_id', 'Count')
channels$nr <- 1:nrow(channels)


ui <- dashboardPage(
  skin = "black",
  dashboardHeader(title = "Aktywność na YouTube",titleWidth = 300),
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("Aktywność w czasie", tabName = "wczasie", icon = icon("dashboard")),
      menuItem("Oglądane kanały", tabName = "kanaly", icon = icon("dashboard")),
      menuItem("O stronie", icon = icon("info-circle"), tabName = "about")
    )
  ),
  
  dashboardBody(
    tabItems(
      
      tabItem("wczasie",
              fluidPage(
                titlePanel(""),
                sidebarLayout(
                  sidebarPanel(
                    radioButtons("type", "Wybierz rozdzielczość danych:",
                                 choiceNames = c("dzienna","miesieczna"),
                                 choiceValues = c("dzienne","miesieczne"),
                                 selected = 'dzienne'
                    ),
                    textOutput("caption_wczasie")
                  ),
                  mainPanel(
                    plotlyOutput("wykres_wczasie", height="800px")
                  )
                )
              )
      ),
      
      tabItem("kanaly",
              fluidPage(
                titlePanel(""),
                sidebarLayout(
                  sidebarPanel(
                    sliderInput(inputId = "ile_kanalow",
                                       label = "Wybierz liczbę kanałów:",
                                       min = 2,
                                       max = nrow(channels),
                                       step = 1,
                                       value = 500
                    ),
                    textOutput("caption_kanaly")
                  ),
                  mainPanel(
                    plotlyOutput("wykres_kanaly", height="800px")
                  )
                )
              )
      ),
      
      tabItem("about",
              "Aplikacja przedstawia dane o aktywności na portalu YouTube z okresu półtora roku: 09.2016 - 01.2019. Dane zostały 'zanonimizowane', tzn. nazwy kanałów zastąpiono numerami ID.     \n\n",
              actionButton(inputId='ab1', label="YouTube", 
                           icon = icon("th"),
                           onclick ="window.open('http://youtube.com', '_blank')")
      )
      
    )
  )
)

server <- function(input, output) {
  
  df_plot <- reactive({
    df_pl[df_pl$type==input[['type']],]
  })
  
  output[['wykres_wczasie']] <- renderPlotly(
    
    ggplotly(
      ggplot(data = df_plot(), aes(x = date)) +
        scale_x_date(date_breaks = "1 month", date_labels =  "%b %Y") +
        geom_hline(yintercept = mean(df_plot()$count), color = "darkgray") +
        geom_text(aes(as.Date('20160901', format = '%Y%m%d'), mean(df_plot()$count), label = "Średnia", angle = 0), size = 5, color = '#4d4d4d') +
        geom_point(aes(x = date, y = count), color = '#35978f', alpha = 0.6, size = 1) +
        geom_line(aes(x = date, y = count), color = '#35978f', size = 0.6) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.title = element_blank(),
              title = element_text(size = 19), axis.text = element_text(size=10)) +
        labs(x = '', y = 'Liczba filmów') +
        ggtitle('Liczba obejrzanych filmów dziennie lub miesięcznie')
      )
  )
  
  output[['caption_wczasie']] <- renderText('Zauważmy, że oglądalność dzienna bardzo oscyluje - są dni kiedy jest bardzo duża i kiedy jest praktycznie zerowa, niewiele jest wartości pośrednich. Natomiast oglądalność miesięczna aż tak nie oscyluje, ale można zauważyć, że zwykle spada w okolicach lata.')
  
  ch <- reactive({
    channels[1:input[["ile_kanalow"]],]
  })

  output[['wykres_kanaly']] <- renderPlotly(
    ggplotly(
      ggplot(data = ch(), aes(x = nr, y = Count)) +
        geom_bar(stat='identity') +
        xlab('Nr kanału') +
        ylab('Liczba wyświetleń') +
        scale_x_continuous(breaks = seq(0,nrow(ch()), by=ifelse(nrow(ch())<200,20,100))) +
        ggtitle('Rozkład liczby wyświetleń filmów na poszczególnych kanałach YouTube') +
        theme(title = element_text(size = 19), axis.text = element_text(size=10))
    )
  )
  
  output[['caption_kanaly']] <- renderText('Rozkład zdecydowanie nie jest równomierny. Największa oglądalność (ponad 200 wyświetleń) występuje dla dwóch kanałów, jest kilka kanałów o oglądalności większej od 50, a pozostałe kanały były odwiedzane praktycznie jednorazowo.')
 
}

shinyApp(ui, server)
