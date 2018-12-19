library(shiny)
library(dplyr)
library(ggplot2)
library(tidyr)
library(eurostat)
library(lubridate)
library(mapproj)
library(ggrepel)

data <- read.csv("data.csv", header = TRUE)

map <- get_eurostat_geospatial(output_class = "df", resolution = "60", nuts_level = "2") %>%
  filter(CNTR_CODE == "PL", LEVL_CODE == 2) %>%
  group_by(NUTS_NAME)

weekly <- data %>% 
  mutate(date = week(date)) %>% 
  group_by(name, date, element) %>% 
  dplyr::summarize(value = mean(value, na.rm = TRUE))

monthly <- data %>% 
  mutate(date = month(date)) %>% 
  group_by(city, date, element) %>% 
  dplyr::summarize(value = mean(value, na.rm = T))
  
yearly <- data %>% 
  group_by(x, y, element, city) %>% 
  dplyr::summarize(value=mean(value, na.rm=TRUE))
  
function(input, output) {
  weekly_ui <- reactive({
    weekly %>%
      ungroup() %>% 
      filter(name %in% input$locations) %>% 
      filter(element == input$elements)
  })
  
  max_weekly <- reactive({
    max(weekly %>% ungroup() %>% filter(element == input$elements) %>% select(value), na.rm = TRUE)
  })
  
  output$weekly <- renderPlot({
    p <- ggplot(weekly_ui(), aes(date, value, color = name)) +
      geom_line() +
      theme_minimal() +
      ylim(0, max(max_weekly(), 1)) +
      theme(axis.text = element_text(size = 15),
            axis.title = element_text(size = 15),
            legend.text = element_text(size = 15),
            legend.title = element_text(size = 15)) +
      scale_x_continuous(1:52, breaks = seq(1, 52, 3), limits = c(0, 53), name = "Week") +
      ylab(bquote('Pollution ['~ug~'/'~m^3~']')) +
      labs(color = "Location")
    
    print(p)
  }, height=800)
  
  yearly_ui <- reactive({
    yearly %>% filter(element == input$elements2)
  })
  
  output$yearly <- renderPlot({
    p <- ggplot(map, aes(x = long, y = lat, group = group)) + 
      geom_polygon(color = "black", fill = "#cccccc") +
      coord_map() +
      theme_minimal() +
      theme(
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        title = element_text(size = 15)
      ) +
      geom_point(data=yearly_ui(), mapping=aes(x, y, color = value), size = 7, inherit.aes = F) +
      geom_text_repel(data=yearly_ui(), mapping=aes(x, y, label = city), color = "black", inherit.aes = F, size = 5) +
      scale_color_continuous(limits=c(0, max(yearly_ui()$value))) +
      xlab("") +
      ylab("") +
      labs(color = bquote('Pollution ['~ug~'/'~m^3~']')) +
      ggtitle("Average pollution in 2017")
    
    print(p)
  }, height=800)
  
  
  left_ui <- reactive({
    inner_join(
      monthly %>% filter(city == input$compare1) %>% filter(input$elements3 == element) %>% select(date, cityX = city, valueX = value),
      monthly %>% filter(city == input$compare2) %>% filter(input$elements3 == element) %>% select(date, cityY = city, valueY = value),
      by = "date"
    ) %>% dplyr::mutate(value = -max(valueX - valueY, 0)) %>% select(city = cityX, value, date)
  })
  right_ui <- reactive({
    inner_join(
      monthly %>% filter(city == input$compare1) %>% filter(input$elements3 == element) %>% select(date, cityX = city, valueX = value),
      monthly %>% filter(city == input$compare2) %>% filter(input$elements3 == element) %>% select(date, cityY = city, valueY = value),
      by = "date"
    ) %>% dplyr::mutate(value = max(valueY - valueX, 0)) %>% select(city = cityY, value, date)
  })
  
  output$monthly <- renderPlot({
    max_y <- max(abs(c(left_ui()$value, right_ui()$value)))
    max_y <- max(((max_y + 4) %/% 5) * 5, 5)
    ggplot(monthly, aes(x = date, y = value, fill = city)) +
      geom_bar(data = left_ui(), stat="identity") +
      geom_bar(data = right_ui(), stat="identity") +
      coord_flip() +
      theme_bw() +
      theme(axis.text = element_text(size = 15),
            axis.title = element_text(size = 15),
            legend.text = element_text(size = 15),
            legend.title = element_text(size = 15),
            title = element_text(size = 15),
            panel.grid.minor = element_blank()
      ) +
      scale_y_continuous(breaks = seq(-max_y, max_y, 5), labels = c(seq(max_y, 0, -5), seq(5, max_y, 5)), limits = c(-max_y, max_y)) +
      scale_x_continuous(breaks = seq(1, 12), labels = seq(1, 12), limits=c(0.5, 12.5)) +
      xlab("Month") +
      ylab(bquote('Pollution ['~ug~'/'~m^3~']')) +
      labs(fill = "Location") + 
      ggtitle("Absolute difference in pollution congestion between cities")
  }, height=800)
}
