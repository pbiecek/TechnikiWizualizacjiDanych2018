library(shiny)
library(shinydashboard)
library(reshape2)
library(dplyr)
library(ggplot2)

dat <- read.csv2("https://raw.githubusercontent.com/michbur/soccer-data/master/PL_dat.csv")

dat$winner <- ifelse(dat$home_team_goal > dat$away_team_goal,"home", ifelse(dat$home_team_goal == dat$away_team_goal, "draw", "away"))

n_matches <- select(dat, home_team, away_team, winner) %>% 
  melt(id.vars = "winner")
  # group_by(value) %>% 
  # summarise(n = length(value)) %>% 
  # arrange(desc(n))
n_matches$status <- "draw"
n_matches$status[n_matches[["winner"]] == "draw"] <- "draw"
n_matches$status[(n_matches$winner == "home" & n_matches$variable == "home_team") |
            (n_matches$winner == "away" & n_matches$variable == "away_team")] <- "won"
n_matches$status[(n_matches$winner == "home" & n_matches$variable == "away_team") |
            (n_matches$winner == "away" & n_matches$variable == "home_team")] <- "lost"

n_matches <- n_matches %>% group_by(value, status) %>% aggregate(by = list(.$value, .$status), FUN = length) %>% select(Group.1, Group.2, value)

n_matches <- n_matches %>% arrange(desc(value)) %>% arrange(desc(Group.2))

winner_home_away <- select(dat, winner) %>%
  group_by(winner) %>%
  summarise(n = length(winner)) %>%
  arrange(desc(n))



teams <- levels(dat$home_team)

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
               badgeColor = "green"),
      menuItem("Home or away", tabName = "winner_home_away", icon = icon("home")),
      menuItem("Match", tabName = "predict_between", icon = icon("ball"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem("dashboard",
              plotOutput("n_matches_plot")
      ),
      tabItem("about",
              "About the app"
      ),
      tabItem("winner_home_away",
              plotOutput("winner_home_away_plot")),
      tabItem("predict_between",
              fluidPage(
                selectizeInput("home_team", "Home team", teams),
                selectizeInput("away_team", "Away team", teams),
                fluidRow(
                box(tableOutput("matches_table")),
                box(plotOutput("stacked_bar_comparison"))
                )
              )
      )
    )
  )
)

server <- function(input, output) {
  
  comparison <- reactive({
    n_matches[n_matches$Group.1 %in% c(input[["home_team"]], input[["away_team"]]),]
  }
  )
  
  output[["n_matches_plot"]] <- renderPlot(
    ggplot(n_matches, aes(x = factor(Group.1, levels = n_matches$Group.1[1:23]), y = value, fill = Group.2)) +
      scale_fill_brewer(palette = "Dark2") +
      geom_bar(stat = "identity") +
      xlab("Team") +
      ylab("Matches") +
      theme(axis.text.x = element_text(angle = 45, vjust = 0.5))
  )
  
  output[["winner_home_away_plot"]] <- renderPlot(
    ggplot(winner_home_away, aes(x = winner, y = n, fill = winner)) +
      geom_bar(stat = "identity")
  )
  
  output[["matches_table"]] <- renderTable(
    comparison()
  )
  
  output[["stacked_bar_comparison"]] <- renderPlot(
    ggplot(comparison(),aes(x = factor(Group.1, levels = c(input[["home_team"]], input[["away_team"]])), y = value, fill = Group.2)) +
      scale_fill_brewer(palette = "Dark2") +
      geom_bar(stat = "identity") +
      xlab("Team") +
      ylab("Matches") +
      theme(axis.text.x = element_text(angle = 45, vjust = 0.5))
  )
  
}

shinyApp(ui, server)
