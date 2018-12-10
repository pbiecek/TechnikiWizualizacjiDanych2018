library(shiny)
library(shinydashboard)
library(ggplot2)
library(data.table)
library(stringi)

BDG <- as.data.table(read.csv("./dane/PM10_BDG.csv",sep=";",skip = 1))
TOR <- as.data.table(read.csv("./dane/PM10_TOR.csv",sep=";",skip = 1))
GRU <- as.data.table(read.csv("./dane/PM10_GRU.csv",sep=";",skip = 1))


df <- merge(BDG,TOR, by = "Jednostka")

df <- merge(df, GRU, by= "Jednostka")

colnames(df) <- c("time", "BDG", "TOR", "GRU")


x <- stri_datetime_parse(df$time, format = "uuuu-MM-dd HH:mm")

df <- data.table(
  stri_datetime_fields(x),
  BDG$ug.m3,
  TOR$ug.m3,
  GRU$ug.m3
)

colnames(df)[15] <- "BDG"
colnames(df)[16] <- "TOR"
colnames(df)[17] <- "GRU"


#################################################################



ui <- dashboardPage(
  skin = "black",
  dashboardHeader(title = "Zanieczyszczenie powietrza w Kujawsko-Pomorskim"),
  dashboardSidebar(
    sidebarUserPanel(Sys.info()[["effective_user"]],
                     subtitle = a(href = "#", icon("circle", class = "text-success"), "Online")
    ),
    sidebarMenu(
      # Setting id makes input$tabs give the tabName of currently-selected tab
      id = "tabs",
      menuItem("Trzy miasta", tabName = "trzy", icon = icon("heart")),
      menuItem("O aplikacji", icon = icon("info-circle"), tabName = "about")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem("trzy",
              checkboxGroupInput(inputId = "City",
                             label = "Wybierz miasto",
                             choices = c("BDG", "TOR", "GRU"),
                             selected = c("BDG", "TOR", "GRU")),
              plotOutput("hour"),
              plotOutput("day"),
              plotOutput("month"),
              plotOutput("year")
              
      ),
      tabItem("about",
              "Aplikacja pokazuje stan powietrza w trzech miastach wojewodztwa Kujawsko-Pomorskiego."
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  
  avg_hours <- df[, .(BDG = mean(BDG, na.rm = TRUE), TOR = mean(TOR, na.rm = TRUE), GRU = mean(GRU, na.rm = TRUE)), by = "Hour"]
  
  avg_hours <- melt(avg_hours, id.vars = "Hour")
  
   output[["hour"]] <- renderPlot({
     ggplot(data = avg_hours[avg_hours$variable %in% input$City ,], aes(x = Hour, y = value, fill = variable)) + geom_col(position = 'dodge') + 
       scale_x_continuous(breaks = 0:23, labels = as.character(0:23), name = "Godziny") + 
       scale_y_continuous(breaks = seq(0,50,10), name = "PM10") +
       theme_bw() + 
       scale_fill_brewer(palette = 'Accent') + 
       ggtitle('Srednie zanieczyszczenie w danych godzinach') +
       theme(title = element_text(size = 30), axis.text = element_text(size=20),legend.text = element_text(size=20)) +
       guides(fill=guide_legend(title="Miasta"))
   })
   
   #############################
   
   avg_day <- df[, .(BDG = mean(BDG, na.rm = TRUE), TOR = mean(TOR, na.rm = TRUE), GRU = mean(GRU, na.rm = TRUE)), by = "DayOfWeek"]
   
   avg_day <- melt(avg_day, id.vars = "DayOfWeek")
   
   output[["day"]] <- renderPlot({
     ggplot(data = avg_day[avg_day$variable %in% input$City,], aes(x = DayOfWeek, y = value, fill = variable)) + geom_col(position = 'dodge') +
       scale_x_continuous(breaks = 1:7, labels = c('PON','WT','SR','CZW','PT','SOB','ND'), name = "Dni") + 
       scale_y_continuous(breaks = seq(0,50,10), name = "PM10") +
       theme_bw() + 
       scale_fill_brewer(palette = 'Accent') + 
       ggtitle('Srednie zanieczyszczenie w danych dniach tygodnia') +
       theme(title = element_text(size = 30), axis.text = element_text(size=20),legend.text = element_text(size=20)) +
       guides(fill=guide_legend(title="Miasta"))
   })
   
   ##############################
   
   avg_month <- df[, .(BDG = mean(BDG, na.rm = TRUE), TOR = mean(TOR, na.rm = TRUE), GRU = mean(GRU, na.rm = TRUE)), by = "Month"]
   
   avg_month <- melt(avg_month, id.vars = "Month")
   
   output[["month"]] <- renderPlot({
     ggplot(data = avg_month[avg_month$variable %in% input$City], aes(x = Month, y = value, fill = variable)) + geom_col(position = 'dodge') +
       scale_x_continuous(breaks = 1:11, labels = c('STY','LUT','MAR','KWI','MAJ','CZE','LIP','SIE','WRZ','PAZ','LIS'), name = "Miesiace") + 
       scale_y_continuous(breaks = seq(0,50,10), name = "PM10") +
       theme_bw() + 
       scale_fill_brewer(palette = 'Accent') + 
       ggtitle('Srednie zanieczyszczenie w danych miesiacach') +
       theme(title = element_text(size = 30), axis.text = element_text(size=20),legend.text = element_text(size=20)) +
       guides(fill=guide_legend(title="Miasta"))
   })
   
   ###############################
   
   avg_year <- data.frame(
     miasto = c("BDG","TOR","GRU"),
     wart = c(mean(df$BDG,na.rm = TRUE), mean(df$TOR,na.rm=TRUE), mean(df$GRU, na.rm = TRUE))
   )
   
   output[["year"]] <- renderPlot({
     ggplot(data = avg_year[avg_year$miasto %in% input$City,], aes(x = miasto, y = wart, fill = miasto)) + geom_col() +
       scale_y_continuous(breaks = seq(0,50,10), name = "PM10") +
       theme_bw() + 
       scale_fill_brewer(palette = 'Accent') + 
       ggtitle('Srednie zanieczyszczenie w ciagu roku') + 
       theme(title = element_text(size = 30), axis.text = element_text(size=20),legend.text = element_text(size=20)) + 
       guides(fill=guide_legend(title="Miasta"))
   })
   
}

# Run the application 
shinyApp(ui = ui, server = server)

