library(rbokeh)
library(dplyr)

url = "http://cdn.onlinewebfonts.com/svg/img_448351.png"


figure() %>%  ly_image_url(4, 4,, w=1.5,h=1.5,image_url = url,
                           anchor = "center")%>% ly_image_url(4, 4,, w=2,h=2,image_url = url,
                                                              anchor = "center")%>%
  ly_image_url(-2, -5,, w=0.8,h=0.8,image_url = url,
               anchor = "center")%>%
  ly_image_url(-4, -4,, w=2,h=2,image_url = url,
               anchor = "center")%>%
  ly_image_url(-5, -0,, w=1.75,h=1.75,image_url = url,
               anchor = "center")%>%
  ly_image_url(4.9, 0,, w=2,h=2,image_url = url,
               anchor = "center")%>%
  ly_hexbin(rnorm(1000000), rnorm(1000000)) %>% ly_hexbin(rnorm(1000,-2,0.15), rnorm(1000,-2,0.15), palette = "white" ) %>%
ly_hexbin(rnorm(10000,0,0.15), rnorm(10000,-4,0.15), palette = "red" ) %>% 
ly_hexbin(rnorm(10000,-4,0.15), rnorm(10000,0.6,0.15), palette = "blue" ) %>% 
ly_hexbin(rnorm(100000,-1,0.15), rnorm(100000,-2.5,0.15), palette = "red" ) %>% 
ly_hexbin(rnorm(100000,2,0.15), rnorm(100000,2,0.15), palette = "blue" ) %>%
ly_hexbin(rnorm(10000,1,0.15), rnorm(10000,3.71,0.15), palette = "red" ) %>%
ly_hexbin(rnorm(100000,3,0.15), rnorm(100000,-2,0.15), palette = "blue" ) %>%
ly_hexbin(rnorm(100000,3,0.15), rnorm(100000,0.7,0.15), palette = "red" ) %>%
ly_hexbin(rnorm(100000,-3.3,0.15), rnorm(100000,-1.3,0.15), palette = "blue" ) %>%
  ly_hexbin(rnorm(100000,-2,0.15), rnorm(100000,2,0.15), palette = "red" ) %>%
  ly_hexbin(rnorm(10000,-1.5,0.15), rnorm(10000,4,0.15), palette = "blue" ) %>%
  ly_hexbin(rnorm(100000,-2.5,0.15), rnorm(100000,3.6,0.15), palette = "red" ) %>%
  ly_hexbin(rnorm(10000,2,0.15), rnorm(10000,-4,0.15), palette = "blue" ) %>%
  ly_hexbin(rnorm(10000,-2.3,0.15), rnorm(10000,-3.7,0.15), palette = "blue" ) %>%
  ly_hexbin(rnorm(10000,4,0.15), rnorm(10000,2.8,0.15), palette = "red" ) %>%
ly_hexbin(rnorm(1000,0,0.15), rnorm(1000,-2,0.15), palette = "white" ) %>% 
ly_hexbin(rnorm(1000,-2,0.15), rnorm(1000,0,0.15), palette = "white" ) %>% 
ly_hexbin(rnorm(1000,-1,0.12), rnorm(1000,-2.5,0.15), palette = "white" ) %>% 
ly_hexbin(rnorm(1000,2,0.15), rnorm(1000,2,0.15), palette = "white" ) %>%
ly_hexbin(rnorm(1000,4,0.15), rnorm(1000,0,0.1), palette = "white" ) %>%
ly_hexbin(rnorm(1000,3,0.15), rnorm(1000,-2,0.15), palette = "white" ) %>%
ly_hexbin(rnorm(1000,3,0.12), rnorm(1000,3,0.15), palette = "white" ) %>%
ly_hexbin(rnorm(1000,1,0.15), rnorm(1000,-1,0.13), palette = "white" ) %>%
ly_hexbin(rnorm(1000,-1,0.15), rnorm(1000,1.5,0.15), palette = "white" ) %>%
ly_hexbin(rnorm(1000,2,0.14), rnorm(1000,2,0.15), palette = "white" ) %>%
ly_hexbin(rnorm(1000,0,0.15), rnorm(1000,2.5,0.15), palette = "white" ) %>%
ly_hexbin(rnorm(1000,-3.5,0.15), rnorm(1000,1,0.15), palette = "white" ) %>%
ly_hexbin(rnorm(1000,4,0.15), rnorm(1000,0,0.15), palette = "white" ) %>%
ly_hexbin(rnorm(1000,2,0.11), rnorm(1000,-3.5,0.15), palette = "white" ) %>%
ly_hexbin(rnorm(1000,-2,0.15), rnorm(1000,3.5,0.15), palette = "white" ) %>%
ly_hexbin(rnorm(1000,-4,0.15), rnorm(1000,0,0.15), palette = "white" ) %>%
ly_hexbin(rnorm(1000,0,0.15), rnorm(1000,-4,0.16), palette = "white" ) %>%
ly_hexbin(rnorm(1000,2.5,0.15), rnorm(1000,0,0.15), palette = "white" ) %>%
ly_hexbin(rnorm(100000,0,0.25), rnorm(100000,0,0.29), palette = "gold" ) %>%
  ly_hexbin(rnorm(100000,0,0.25), rnorm(100000,0,0.29), palette = "gold" )

