
############################################################################################################
############################################################################################################

require(shiny)
require(shinydashboard)
require(SmarterPoland)
require(ggplot2)
require(dplyr)
require(readxl)
require(hablar)

############################################################################################################
############################################################################################################

trojkat <- function(n, l){
  
  a1 <- runif(n, 0, l)
  a2 <- runif(n, 0, l)
  
  for (i in 1:n){
    
    if( a2[i]>(-a1[i] + l) ){
      a2[i] <- l - a2[i]
      a1[i] <- a1[i] - l
    }
    
  }
  
  c(a1, a2)
  
}

############################################################################################################

przeciecie <- function(a1,a2,b1,b2){
  
  W <- a1-a2
  Wx <- b2-b1
  
  x <- Wx/W
  x
  
}

############################################################################################################

lancuch <- function(a1,a2,a3,b1,b2,b3,n){
  
  x1 <- przeciecie(a1,a3,b1,b3)
  x2 <- przeciecie(a2,a3,b2,b3)
  
  lx <- runif(n, x2, x1)
  ly <- a3*lx + b3
  
  res <- c(lx,ly)
  res
  
}

############################################################################################################
############################################################################################################

n1 <- 1800
n2 <- 400
n3 <- 200
n4 <- 400
n5 <- 30

m1 <- 200
m2 <- 100
m3 <- 50

l1 <- 2
l2 <- 1 
l3 <- 1/2

############################################################################################################

t1 <- trojkat(n1, l1)
t2 <- trojkat(n2, l2)
t3 <- trojkat(n3, l3)

l <- lancuch(-1, 1, (1/8), 2, 2, (1/2), m1)
lx <- l[1:m1]
ly <- l[(m1+1):(2*m1)]

l <- lancuch(-1, 1, (-1/8), 2, 2, (1), m1)
lx <- c(lx, l[1:m1])
ly <- c(ly, l[(m1+1):(2*m1)])

l <- lancuch(-1, 1, (1/8), 2, 2, (1.5), m1)
lx <- c(lx, l[1:m1])
ly <- c(ly, l[(m1+1):(2*m1)])

l <- lancuch(-1, 1, (-1/8), 3, 3, (2.2), m2)
lx <- c(lx, l[1:m2])
ly <- c(ly, l[(m2+1):(2*m2)])

l <- lancuch(-1, 1, (1/8), 3, 3, (2.6), m2)
lx <- c(lx, l[1:m2])
ly <- c(ly, l[(m2+1):(2*m2)])

l <- lancuch(-1, 1, (-1/8), 3.5, 3.5, (3.2), m3)
lx <- c(lx, l[1:m3])
ly <- c(ly, l[(m3+1):(2*m3)])

px <- runif(n4, -1/5, 1/5) 
py <- runif(n4, -1, 0)

sx <- rep(0, n5)
sy <- runif(n5, 0, 1/3) + (l1+l2+l3)

bx <- c(-2, 2, -1, 1, -1/2, 1/2)
by <- c(0, 0, 2, 2, 3, 3)

############################################################################################################
############################################################################################################

rodzaj_choinki <- data.frame( rodzaj = c('Świerk', 'Świerk srebrzysty', 'Jodła'),
                              kolor = c('limegreen', 'mediumaquamarine', 'springgreen4'))

############################################################################################################

rodzaj_lampek <- data.frame( rodzaj = c('Brak', 'Białe', 'Złote', 'Kolorowe'),
                             kolor1 = c(k1, 'whitesmoke', 'darkgoldenrod1', 'blue1'),
                             kolor2 = c(k1, 'whitesmoke', 'darkgoldenrod1', 'firebrick1'),
                             kolor3 = c(k1, 'whitesmoke', 'darkgoldenrod1', 'greenyellow'))

############################################################################################################

rodzaj_bombek <- data.frame( rodzaj = c('Brak', 'Czerwone', 'Niebieskie', 'Złote'),
                             kolor = c('white', 'red', 'blue', 'darkgoldenrod1'))

############################################################################################################
############################################################################################################

x <- c(t1[1:n1], t2[1:n2], t3[1:n3], px, sx, lx, bx)
y <- c(t1[(n1+1):(2*n1)], t2[(n2+1):(2*n2)] + l1, t3[(n3+1):(2*n3)] + (l1+l2), py, sy, ly, by)
k <- factor(c(rep(0, (n1+n2+n3)), rep(1, n4), rep(2, n5), sample(3:5, (3*m1+2*m2+m3), replace = TRUE), rep(6,6)))
s <- c(rep(1, (n1+n2+n3)), rep(1, n4), rep(1, n5), rep(1.2, (3*m1+2*m2+m3)), c(10, 10, 8, 8, 6, 6))

dane <- data.frame(X = x, Y = y, K = k, S = s)

############################################################################################################
############################################################################################################



ui <- dashboardPage(
  
  skin = 'red',
  
  dashboardHeader(title = 'MENU'),
  
  dashboardSidebar(
    
    sidebarMenu(
      
      selectizeInput(inputId = 'chosen_tree',
                     label = 'Wybierz drzewko: ',
                     choices = rodzaj_choinki$rodzaj),
      
      selectizeInput(inputId = 'chosen_lights',
                     label = 'Wybierz lampki: ',
                     choices = rodzaj_lampek$rodzaj),
      
      selectizeInput(inputId = 'chosen_baubles',
                     label = 'Wybierz bombki: ',
                     choices = rodzaj_bombek$rodzaj)
      
    )
  ),
  
  
  
  dashboardBody(
    
    fluidPage(
      
      h1('Zanieczyszczenie powietrza w 10 największych miastach w Polsce'),
      
      h1(' '),
      
      lotOutput('distPlot')
    
    )
    
  )
  
)



server <- function(input, output) {
  
  output[['distPlot']] <- renderPlot({
    
    tree <- input[['chosen_tree']]
    lights <- input[['chosen_lights']]
    baubles <- input[['chosen_baubles']]
    
    tree
    
    temp <- rodzaj_choinki %>% filter(rodzaj == 'Jodła')
    k1 <- as.character(temp$kolor)
    
    temp <- rodzaj_lampek %>% filter(rodzaj == 'Kolorowe')
    k2 <- as.character(temp$kolor1)
    k3 <- as.character(temp$kolor2)
    k4 <- as.character(temp$kolor3)
    
    k5 <- rodzaj_bombek %>% filter(rodzaj == 'Złote')
    k5 <- as.character(k5$kolor)
    

    
    ggplot(dane, aes(x = X, y = Y, color = K)) + 
      geom_point(aes(size = S)) + 
      scale_color_manual(values=c(k1, "brown", "gold", k2, k3, k4, k5)) +
      theme_void() +
      guides(fill = FALSE, color = FALSE, size = FALSE) +
      labs(x = '', y = '', title = '')
    
  })
  
}



shinyApp(ui = ui, server = server)
