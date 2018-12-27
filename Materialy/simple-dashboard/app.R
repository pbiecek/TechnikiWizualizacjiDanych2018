library(shiny)
library(shinydashboard)
library(reshape2)
library(dplyr)
library(ggplot2)

dat <- read.csv2("https://raw.githubusercontent.com/michbur/soccer-data/master/PL_dat.csv")

n_matches <- select(dat, season, home_team, away_team) %>% 
  melt(id.vars = "season") %>% 
  group_by(value) %>% 
  summarise(n = length(value)) %>% 
  arrange(desc(n))

ui <- dashboardPage(
  skin = "black",
  dashboardHeader(title = "Football App",
                  
                  dropdownMenu(type = "notifications", badgeStatus = "warning",
                               notificationItem(icon = icon("exclamation-triangle"), status = "info",
                                                "This app is underdeveloped"
                               )
                  )),
  dashboardSidebar(
    sidebarUserPanel(Sys.info()[["effective_user"]],
                     subtitle = a(href = "#", icon("circle", class = "text-success"), "Online")
    ),
    sidebarMenu(
      # Setting id makes input$tabs give the tabName of currently-selected tab
      id = "tabs",
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("About", icon = icon("info-circle"), tabName = "about", badgeLabel = "new",
               badgeColor = "green")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem("dashboard",
              plotOutput("n_matches_plot")
      ),
      tabItem("about",
              "About the app"
      )
    )
  )
)

server <- function(input, output) {
  output[["n_matches_plot"]] <- renderPlot(
    ggplot(n_matches, aes(x = value, y = n)) +
      geom_col()
  )
  
}

shinyApp(ui, server)
