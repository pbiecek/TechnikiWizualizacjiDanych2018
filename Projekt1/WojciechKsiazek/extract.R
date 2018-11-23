library(jsonlite)
library(tidyverse)
library(lubridate)
library(forcats)
library(hms)

xray <- read_json("Archive 18-11-08 17-29-35.har")
data <- xray$log$entries[[1]]$response$content$text %>% fromJSON(simplifyVector = F)
scenes <- data$page$sections$left$widgets$widgetList
partitionedChangeList <- scenes[[1]]$widgets$widgetList[[2]]$partitionedChangeList
partitionedChangeList <- partitionedChangeList[-1]


p2 <- partitionedChangeList %>% transpose

timeRange <- p2$timeRange %>% transpose %>% as_data_frame()

changesCollection <- 
  partitionedChangeList %>%
  map(~ getElement(., "changesCollection") %>% bind_rows) %>% 
  bind_rows()

initial <- 
  partitionedChangeList %>%
  map(~ getElement(., "initialItemIds")) %>% 
  unlist

scene <- partitionedChangeList[[2]]

processScene <- function(scene){
  start <- scene$timeRange$startTime
  end <- scene$timeRange$endTime
  init <- scene$initialItemIds %>% unlist
  # browser()
  if(!is.null(init)){
    init <- 
      data_frame(name = init, start = start, end = end)
  }else{
    init <- list()
  }
  # browser()
  if(length(scene$changesCollection) != 0){
    change <- 
      scene$changesCollection %>% bind_rows() %>% 
      select(name = itemId, start = timePosition) %>% 
      mutate(end = end)
  }else{
    change <- list()
  }

  
  bind_rows(init, change) %>%    
    filter(grepl(x = name, pattern = "name")) 

}

timeline <- partitionedChangeList %>% map(processScene) %>% bind_rows()

timeline <- 
timeline %>% 
  mutate(name = gsub(pattern = '/name/(nm.+)/', replacement = "", x = name))

timeline %>% 
  gather(key = "key", value = "duration", -name) %>% 
  ggplot(aes(y = name, x = duration, color = key)) + geom_point()
# 

timeline %>% 
  mutate(name = as_factor(name),
         name = fct_inorder(name),
         name = fct_rev(name), start = start / 1000, end = end / 1000) %>% 
  mutate( start = as.hms( start ),
          end = as.hms(end) ) %>%
  ggplot(aes(x = name, ymin = start, ymax = end)) + 
  geom_linerange(size = 2.5, color = "#636363") +
  labs(x = "", y = "") +
  coord_flip()+ 
  theme_bw() + 
  theme(legend.position = "none") +
  # scale_x_time()+
  ggtitle("Kto i kiedy pojawia się na ekranie?", "Okresy występowania postaci w filmie")

# cairo_ps()

summaries <-
timeline %>% 
  mutate(duration = end - start) %>% 
  group_by(name) %>% 
  summarise(screenTime = sum(duration),
            firstAppearance = min(start))

ggplot(summaries, aes(reorder(name, screenTime), screenTime)) + geom_col() + coord_flip()

timeline <- timeline %>% mutate(name = as_factor(name))

                                