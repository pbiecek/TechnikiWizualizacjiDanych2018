#Pakiet worldcloud2 należy zainstalować z githuba przy pomocy devtools ponieważ na CRAN jest stara wersja
#devtools::install_github("lchiffon/wordcloud2")
#Aplikacja nie wyswietla się prawidłowo w przeglądarce wbudowanej w rstudio, należy kliknąć 'open in browser'
library(shiny)
library(shinythemes)
library(ggplot2)
library(devtools)
library(stringr)
library(wordcloud2)
annotations <- data.frame(
  xpos = c(-Inf,-Inf,Inf,Inf),
  ypos =  c(-Inf, Inf,-Inf,Inf),
  annotateText = c("Sad/Gloomy","Angry"
                   ,"Peaceful","Joyful"),
  hjustvar = c(-0.05,-0.05,1.05,1.05) ,
  vjustvar = c(-1,2,-1,2))

theme_black = function(base_size = 20, base_family = "") {
  
  theme_grey(base_size = base_size, base_family = base_family) %+replace%
    
    theme(
      # Specify axis options
      axis.line = element_blank(),  
      axis.text.x = element_text(size = base_size*0.8, color = "white", lineheight = 0.9),  
      axis.text.y = element_text(size = base_size*0.8, color = "white", lineheight = 0.9),  
      axis.ticks = element_line(color = "white", size  =  0.2),  
      axis.title.x = element_text(size = base_size, color = "white", margin = margin(0, 10, 0, 0)),  
      axis.title.y = element_text(size = base_size, color = "white", angle = 90, margin = margin(0, 10, 0, 0)),  
      axis.ticks.length = unit(0.3, "lines"),   
      # Specify legend options
      legend.background = element_rect(color = NA, fill = '#2B3E50'),  
      legend.key = element_rect(color = "white",  fill = '#2B3E50'),  
      legend.key.size = unit(1.2, "lines"),  
      legend.key.height = NULL,  
      legend.key.width = NULL,      
      legend.text = element_text(size = base_size*0.8, color = "white"),  
      legend.title = element_text(size = base_size*0.8, face = "bold", hjust = 0, color = "white"),  
      legend.position = "right",  
      legend.text.align = NULL,  
      legend.title.align = NULL,  
      legend.direction = "vertical",  
      legend.box = NULL, 
      # Specify panel options
      panel.background = element_rect(fill = '#2B3E50', color  =  NA),  
      panel.border = element_rect(fill = NA, color = "white"),  
      panel.grid.major = element_line(color = "grey35"),  
      panel.grid.minor = element_line(color = "grey20"),  
      panel.margin = unit(0.5, "lines"),   
      # Specify facetting options
      strip.background = element_rect(fill = "grey30", color = "grey10"),  
      strip.text.x = element_text(size = base_size*0.8, color = "white"),  
      strip.text.y = element_text(size = base_size*0.8, color = "white",angle = -90),  
      # Specify plot options
      plot.background = element_rect(color = '#2B3E50', fill = '#2B3E50'),  
      plot.title = element_text(size = base_size*1.2, color = "white"),  
      plot.margin = unit(rep(1, 4), "lines")
      
    )
  
}
tracks_long <- read.csv('https://gist.githubusercontent.com/plubon/ae07b85d773f6550800f8a56df7525d4/raw/dea0d99e1d772e89cfe323def2ebc688ea4c95a4/tracks_medium.csv', stringsAsFactors = FALSE)
tracks_long$length <- paste((tracks_long$duration/1000)%/%60, str_pad(floor(tracks_long$duration/1000)%%60,2,pad='0'),sep = ':')
artists_long <- read.csv('https://gist.githubusercontent.com/plubon/28f9421df8165a7add6b6d37eed7fb26/raw/b5c7600e9f39598b743eb5a3f2ed3434d39aee30/artists_longs.csv', stringsAsFactors = FALSE)
tracks_long$is_selected = FALSE
genre_list <- strsplit(artists_long$genres, split=':', fixed = TRUE)
wordcloud_df <- as.data.frame(table(unlist(genre_list)))
genre_counts <- sapply(genre_list, length)
artist_genre <- data.frame(artist=rep(artists_long$name, genre_counts), img=rep(artists_long$images, genre_counts), genre=unlist(genre_list))
ui <- fluidPage(
  theme=shinytheme('superhero'),
  
  titlePanel("What Spotify thinks my favourite music is"),
  tabsetPanel(
    tabPanel("Favourite genres and artists",
    fluidRow(
      wordcloud2Output("my_wc"),
      htmlOutput("print"),
      htmlOutput('html_images')
    )),
    tabPanel("Favourite tracks from last 6 months",fluidRow(
      plotOutput('longPlot',click = "plot_hover"),
      htmlOutput('selected_track')))
  )
)

server <- function(input, output, session) {
  
  observeEvent(input$plot_hover, {
    if(is.null(input$plot_hover))
    {
      session$userData$artist <- NULL
      session$userData$track <- NULL
    }
    else
    {
      hover = input$plot_hover
      dist <- sqrt((hover$x - tracks_long$valence)^2 + (hover$y - tracks_long$energy)^2)
      if(min(dist) < 0.03) {
        artist <- tracks_long$artist_name[which.min(dist)]
        track <- tracks_long$track_name[which.min(dist)]
        session$userData$track <- track
        tracks_long$is_selected <- tracks_long$artist_name == artist 
        session$userData$artist <- artist
      }
      else
      {
        session$userData$artist <- NULL
        session$userData$track <- NULL
      }
    }
    output$longPlot <- renderPlot({
      if(!is.null(session$userData$artist))
      {
        tracks_long$is_selected <- tracks_long$artist_name == session$userData$artist
      }
      gg <- ggplot(aes(x=valence, y=energy, color=is_selected), data=tracks_long)+
        geom_point(size=4)+
        theme_black()+
        theme(legend.position="bottom",legend.direction="horizontal")+
        xlim(0,1)+
        ylim(0,1)+
        geom_text(data=annotations,aes(x=xpos,y=ypos,hjust=hjustvar,vjust=vjustvar,label=annotateText),color='white',size=7)+
        theme(axis.text=element_text(size=20))+
        theme(legend.position="none")
      return(gg)
    })
    output$selected_track <- renderText({
      if(!is.null(session$userData$track))
      {
        row <- tracks_long[tracks_long$track_name == session$userData$track,]
        html <- paste0(c('<div style="margin-left: 30px;">',
               '<img src="',as.character(row$image),'"style="width:300px;height:300px;float:left;">',
               '<ul style="float:left;list-style-type: none;">',
               '<li style="font-size: xx-large;">',row$artist_name,'</li>',
               '<li style="font-size: x-large;">',row$track_name,'</li>',
               '<li style="font-size: large;">',row$album_name,'</li>',
               '<li style="font-size: large;">',row$length,'</li>',
               '</ul>',
               '</div>'),collapse='')
      }
    })
  })
  output$selected_track <- renderText({
      return('')
  })
  output$longPlot <- renderPlot({
    if(!is.null(session$userData$artist))
    {
      tracks_long$is_selected <- tracks_long$artist_name == session$userData$artist
    }
    gg <- ggplot(aes(x=valence, y=energy, color=is_selected), data=tracks_long)+
      geom_point(size=4)+
      theme_black()+
      theme(legend.position="bottom",legend.direction="horizontal")+
      xlim(0,1)+
      ylim(0,1)+
      geom_text(data=annotations,aes(x=xpos,y=ypos,hjust=hjustvar,vjust=vjustvar,label=annotateText),color='white',size=7)+
      theme(axis.text=element_text(size=20))+
      theme(legend.position="none")
    return(gg)
  })
  output$my_wc <- renderWordcloud2(
    {
      return(wordcloud2(data=wordcloud_df, backgroundColor = '#2B3E50')) 
    })
  output$html_images <- renderText(
  {
    if(is.character(input$my_wc_clicked_word))
    {
      rows = artist_genre[artist_genre$genre==strsplit(input$my_wc_clicked_word, split=':')[[1]][1],]
      imgs <- apply(rows,1, function(x)
      {
          return(paste0(c('<figure style="white-space:nowrap;float:left;display:table;"><img src="',as.character(x[2]),'"style="width:175px;height:175px;">','<figcaption style="width:175px;display:table-caption;caption-side:bottom; word-wrap: break-word;">', as.character(x[1]),'</figcaption></figure>'), collapse=''))
      })
      return(c('<div style="margin-left: 30px;">',imgs,'</div>'))
    } 
  }
  )
}
shinyApp(ui = ui, server = server)

