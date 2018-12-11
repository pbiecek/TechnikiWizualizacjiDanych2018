library(shiny)
library(shinydashboard)
library(reshape2)
library(dplyr)
library(ggplot2)

dat <- read.csv2("https://raw.githubusercontent.com/michbur/soccer-data/master/PL_dat.csv")

# n_matches <- select(dat, season, home_team, away_team) %>% 
#   melt(id.vars = "season") %>% 
#   group_by(value) %>% 
#   summarise(n = length(value)) %>% 
#   arrange(desc(n))
dat <- dat %>%   mutate(goal_diff = home_team_goal - away_team_goal)

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
    ),
    selectInput("season", "Choose season:", choices = levels(dat$season))
  ),
  dashboardBody(
    tabItems(
      tabItem("dashboard",
              infoBoxOutput("most_won"),
              infoBoxOutput("most_lose"),
              plotOutput("tile_plot")
              
      ),
      tabItem("about",
              "About the app"
      )
    )
  )
)

server <- function(input, output) {
  
  wins <- reactive({    dat %>% 
      filter(season == input[["season"]]) %>% 
      group_by(home_team) %>% 
      summarise(wins = sum(goal_diff > 0), loses = sum(goal_diff < 0))
    })

  
  output[["most_won"]] <- renderInfoBox({
    team <- wins() %>% filter(wins == max(wins, na.rm = T)) %>% pull(home_team)
    infoBox("Most won matches:", team, width = 6, icon = icon("far fa-futbol"))
  })
  output[["most_lose"]] <- renderInfoBox({
    team <- wins() %>% filter(loses == max(loses, na.rm = T)) %>% pull(home_team)
    infoBox("Most lost matches:", team, width = 6, icon = icon("far fa-futbol"))
  })
  
  
  
  output[["tile_plot"]] <- renderPlot(
    dat %>% 
      filter(season == input[["season"]]) %>% 
      left_join(wins(), by = "home_team") %>% 
      mutate(wins = wins == max(wins, na.rm =T)) %>% 
      ggplot(aes(home_team, away_team, fill = goal_diff, colour = wins)) + 
      geom_tile() + 
      scale_fill_gradient2() + 
      scale_color_manual(values = c("#EEEEEE", "#EE0000")) + 
      theme_bw() + 
      labs(x = "Home team", y = "Away team", title = "Difference in geals") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
    
  )
  
}

shinyApp(ui, server)
