library(shiny)
library(ggplot2)
library(dplyr)

unfactor <- function(factor) {
  return(levels(factor)[factor])
}

kody <- read.csv("Kody.csv", sep = ";", fileEncoding = "UTF-8")
PM10 <- read.csv("PM10.csv", sep = ";", fileEncoding = "UTF-8")
PM25 <- read.csv("PM25.csv", sep = ";", fileEncoding = "UTF-8")

miasta <- c("Warszawa", "Kraków", "Łódź", "Wrocław", "Poznań", "Gdańsk", "Szczecin", "Bydgoszcz", "Lublin", "Białystok")
data <- data.frame(Miasto = miasta, Ilosc = rep(0,10))
data[,1] <- levels(data[,1])[data[,1]]

city_calc <- function(city,PM) {
  kody_city <- kody %>% filter(MIEJSCOWOSC ==city) %>% select(KOD.NOWY) %>% unlist() %>% unfactor()
  unused <- c()
  for(i in 1:length(unlist(kody_city))) {
    if(sum(colnames(PM) == kody_city[i]) == 0) {
      unused <- c(unused, i)
    }
  }
  kody_city <- kody_city[-unused]
  PM_city <- PM[,kody_city]
  if(is.null(ncol(PM_city))) {
    return(PM_city)
  }
  result <- c()
  for(i in 1: nrow(PM_city)) {
    result <- c(result, mean(na.omit(unlist(PM_city[i,]))))
  } 
  return(result)
}

ui <- fluidPage(
  sidebarPanel(
    
    radioButtons("type", "Używany wskażnik:",
                 c("PM10" = "pm10",
                   "PM2,5" = "pm25"))
  ),
  mainPanel(
    titlePanel("Czy powietrze w Warszawie jest zdrowe?"),
    div("W tej pracy domowej skupimy się na jakości powietrza w Warszawie w porównaniu do innych miast w Polsce.
        W celu wykonania zadania sprawdziliśmy najpierw jak bardzo powietrze Warszawie jest zanieczyszczone. W tym celu przeanalizowaliśy wskażniki dla wszystkich stacji w Warszawie w 2017 roku."),
    div("Na poniższym wykresiem pojedyńczym kolorem zaznaczona wartość zmierzona przez pojednyńczą stację.
        Kolorem czerwoym zaznaczona natomiast poziom dopuszczalny przez Unię Europejską"),
    plotOutput(outputId = "plot1"),
    div("Na podstawie powyższego wykresu widzimy, że powietrze w Warszawie nie należy do najzdrowszych. Normy bardzo często są przekroczone"),
    div("Sprawdźmy teraz, jak kształtuje się średnia zanieczyszczeń w całym roku na tle innych największych miast"),
    plotOutput(outputId = "plot2"),
    div("Na tym wykresie wygląda to trochę lepiej. Normy są przekroczone tylko w dwóch miastach. Mimo wszystko wyniki z poprzedniego wykresu było trochę niepokojące."),
    div("Sprawdźmy zatem, przez ile dni w roku w poszczególnych miastach notowano wartości przekraczające dopuszczalne normy"),
    plotOutput(outputId = "plot3"),
    div("Nie wygląda to ciekawie. W samej Warszawie normy były przekroczone przez ponad dwa miesiące w ciągu ubiegłego roku. A mieszkańcy Krakowa mają jeszcze gorzej!")
    )
  )

server <- function(input, output) {
  output$plot1 <- renderPlot({
    if(input$type == "pm10") {
      PM <- PM10
    } else {
      PM <- PM25
    }
    kody_wav <- kody %>% filter(MIEJSCOWOSC == "Warszawa") %>% select(KOD.NOWY) %>% unlist() %>% unfactor()
    unused <- c()
    for(i in 1:length(kody_wav)) {
      if(sum(colnames(PM) == kody_wav[i]) == 0) {
        unused <- c(unused, i)
      }
    }
    kody_wav <- kody_wav[-unused]
    
    
    PM_wav <- PM[,kody_wav]
    x <- seq(0,12, length.out = 365)
    colors <- c("red","blue", "yellow", "green", "black", "grey", "purple", "#CD5700", "#6E6F2F")
    for(i in 1:ncol(PM_wav)) {
      y <- PM_wav[,i]
      if(i == 1) {
        plot(x,y, type="l", col = colors, xlab = "Miesiąc", ylab = "Stężenie")
      } else {
        lines(x,y, col = colors[i])
      }
    }
    #Poziom 
    if(input$type == "pm10") {
      abline(h=40, col = "red")
    } else {
      abline(h=25, col = "red")
    }
    
  })
  output$plot2 <- renderPlot({
    if(input$type == "pm10") {
      PM <- PM10
      norma <- 40
    } else {
      PM <- PM25
      norma <- 25
    }
    for(i in 1:nrow(data)) {
      data[i,2] <- data[i,1] %>% city_calc(PM) %>% na.omit() %>% mean() %>% round(digits=0)
    }
    
    data <- data[order(data$Ilosc, decreasing = T),]
    data$Miasto <- factor(data$Miasto, levels = data$Miasto)
    
    
    ggplot(data=data, aes(x=Miasto, y=Ilosc)) +
      geom_bar(stat="identity") +
      geom_text(aes(label=Ilosc), hjust=1.5, color="white", size=3.5) +
      geom_hline(yintercept=norma, color = "red") +
      coord_flip() +
      theme_minimal()
  })
  output$plot3 <- renderPlot({
    if(input$type == "pm10") {
      PM <- PM10
      norma <- 40
    } else {
      PM <- PM25
      norma <- 25
    }
    for(i in 1:nrow(data)) {
      data[i,2] <- sum(na.omit(city_calc(data[i,1], PM) > norma))
    }
    
    data <- data[order(data$Ilosc, decreasing = T),]
    data$Miasto <- factor(data$Miasto, levels = data$Miasto)
    
    
    ggplot(data=data, aes(x=Miasto, y=Ilosc)) +
      geom_bar(stat="identity") +
      geom_text(aes(label=Ilosc), hjust=1.5, color="white", size=3.5) +
      coord_flip() +
      theme_minimal()
    
  }) 
}

shinyApp(ui, server)
