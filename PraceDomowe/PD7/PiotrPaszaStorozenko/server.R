library(ggplot2)
library(ggthemes)
library(gridExtra)
library(forecast)
library(ggpubr)
library(dplyr)
library(lubridate)


function(input, output) {
  
  output$dateRangeText  <- renderText({
    paste("input$dateRange is", 
          paste(as.character(input$dateRange), collapse = " do ")
    )
  })
  
  output$out1 <- renderPrint(input$in1)
  output$out2 <- renderPrint(input$in2)
  output$dateRangeText  <- renderPrint(input$dateRange)
  output$opis <- renderText(paste0("Aplikacja prezentuje dane na temat zanieczyszczeń na stacji pogodowej w Warszawie, zlokalizowanej przy Al. Niepodległości 227/233 z okresu 2016-2017r.\n",
                              "Dane na temat zanieczyszczeń zestawione zostały z danymi o sile wiatru ze stacji meteorologicznej przy Wydziale Fizyki PW.\n",
                              "Obserwując wzajemną relację krzywej odpowiadającej za siłę wiatru oraz za poziom zanieczyszczeń, widać ujemną kroskorelację tych sygnałów.\n",
                              "Efekt jest szczególnie widoczny w miesiącach zimowych, np. w okresie 2017-11-01 - 2017-12-31 dla benzenu C6H6.",
                              "Po kliknięciu opcji wiele wykresów wyliczana jest kroskorelacja sygnałów.\n",
                              "Warto zauważyć, że krosskorelacja liczona dla rozdzielczości godzinowej raczej silnie ujemna, zależność nie jest tak silna dla wyższczych rozdzielczości."))
  
  output$plot1 <- renderPlot({
    proc_data <- data_pd7[,c("datetime", input$in1)]
    colnames(proc_data) = c("datetime", "signal")
    daty <- as_datetime(input$dateRange)
    proc_data <- proc_data %>% 
      filter(datetime >= daty[1]) %>% 
      filter(datetime <= daty[2]) %>% 
      mutate(datetime = floor_date(datetime, input$in2)) %>%
      group_by(datetime) %>%
      summarize(signal = mean(signal, na.rm = TRUE))
    
    df_wiatr %>% mutate(datetime = floor_date(datetime, input$in2)) %>%
      group_by(datetime) %>%
      summarize(wspeed = mean(wspeed, na.rm = TRUE))
    
    df <- proc_data %>% left_join(df_wiatr, by = 'datetime')
    
    res <-  NULL
    if (!input$checkbox) {
      fact <- mean(df$signal, na.rm = TRUE) / mean(df$wspeed, na.rm = TRUE) / 2
      res <- ggplot(df, aes(x = datetime)) +
        geom_line(aes(y = signal, colour ="Zanieczyszczenie")) +
        geom_line(aes(y = wspeed * fact, colour ="Wiatr")) +
        scale_y_continuous(sec.axis = sec_axis(~./fact, name = "Prędkość wiatru [m/s]")) +
        xlab("Data") +
        ylab(input$in1) +
        labs(colour = NULL) +
        theme_gdocs() + 
        theme(legend.position = c(0.8, 0.9))
    }
    else
    {
      p1 <- ggplot(df, aes(x = datetime)) +
        geom_line(aes(y = signal)) +
        xlab(NULL) +
        ylab(input$in1) +
        theme_gdocs()
      
      p2 <- ggplot(df, aes(x = datetime)) +
        geom_line(aes(y = wspeed)) +
        xlab("Data") +
        ylab("Prędkość wiatru [m/s]") +
        theme_gdocs()
      
      p3 <- ggCcf(df$signal, df$wspeed) + 
        ggtitle("Krosskorelacja sygnałów") +
        theme_gdocs()
      res <- grid.arrange(p1, p2, p3,ncol = 1, nrow = 3, heights=c(4,4,5))
    }
    
    res 
      
  })
}

