library(sf)
library(tidyverse)
uk <- st_read("uk_650_wpc_2017_full_res_v1.8/uk_650_wpc_2017_full_res_v1.8.shp", stringsAsFactors = FALSE, quiet = TRUE) %>% 
  st_transform(4326) %>% 
  filter(REGN == "London") %>% 
  select(ons_id = PCONCODE, name=PCONNAME)
uk

points <- read.csv('sherlock.csv')


library(ggplot2)
library(rgdal)
p=ggplot() +
  geom_sf(data = uk, fill='#c7a575', colour = "#d7c09e") +
  geom_sf_text(data = uk, aes(label=name), size=3) +
  geom_point(data=points,aes(x=longitude, y=latitude, alpha=0.1), colour='#35526C') +
  coord_sf(xlim = c(-0.3, 0),ylim = c(51.4, 51.57)) +
  theme(axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),legend.position="none")
p 
library(gridSVG)
svg()
p
dev.off()

