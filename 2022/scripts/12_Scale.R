# 30DayMapChallenge
# 12 Scale
# Selected 6 national parks in California

# Libraries
library(tidyverse)
library(sf)
library(tigris)
library(patchwork)
library(ggtext)
library(showtext)
showtext_opts(dpi = 300)
showtext_auto(enable = TRUE)

# Font
font_add_google("DM Sans")
f1 = "DM Sans"

# Data
landmarks= landmarks(state="CA", type="area", year=2022)

landmarks1= landmarks |>
  filter(str_detect(FULLNAME,"Park") | str_detect(FULLNAME,"Pk")) |>
  filter(str_detect(FULLNAME,"Natl")) |>
  arrange(desc(ALAND)) |> 
  mutate(centroid = sf::st_centroid(geometry)) |>
  filter(FULLNAME %in% c("Death Valley Natl Pk","Yosemite Natl Pk","Kings Canyon Natl Pk","Lassen Volcanic Natl Pk","Rainbow Basin Natl Pk","Sequoia Natl Pk")) %>%
  mutate(FULLNAME = str_replace(FULLNAME, " Natl Pk","<br>National Park"))
  
# Colors (title and map)
# Code adapted from Samia @samiasab90, https://github.com/samiaab1990/30-day-map-challenge/blob/main/lines/netherlands_bike_map.R
pal<-colorRampPalette(c("#006466","#4d194d"))
title<-"National Parks in California"
title_no_space<-str_remove_all(title," ")
pal_n<-pal(nchar(title_no_space)+4)
make_title<-""

for(i in 1:nchar(title))
{
  j = i 
  if(substr(title,i,i) !=" ")
  {
    make_title<-paste0(make_title,"<span style='color:",pal_n[j],";'>",substr(title,i,i),"</span>")
  } else
  {
    make_title<-paste0(make_title," ")
    j = i - 1
  }
  
}

map_color<-colorRampPalette(colors=c(pal_n[as.integer(nchar(title)/2)],"#FFFFFF"))(10)[2]

# Scaled maps
# Code adapted from Jonathan Kitt @KittJonathan, https://github.com/KittJonathan/30DayMapChallenge/blob/main/2022/R/18_blue.R
padding <- 1

graph <- function(x){
  ggplot2::ggplot(landmarks1[x,]) +
    ggpattern::geom_sf_pattern(color=map_color,linewidth=.3, pattern = 'stripe', pattern_colour=map_color, fill="white") +
    labs(subtitle = paste0(landmarks1[x, ]$FULLNAME)) +
    coord_sf(xlim = c(landmarks1$centroid[[x]][1]-padding , 
                      landmarks1$centroid[[x]][1]+padding), 
             ylim = c(landmarks1$centroid[[x]][2]-padding , 
                      landmarks1$centroid[[x]][2]+padding), 
             expand = FALSE) +
    cowplot::theme_map(12) +
    theme(text=element_text(family="DM Sans", color=map_color),
          plot.subtitle=element_markdown(hjust=.5, lineheight = 1.3),
          )
}

plot_list <- lapply(X = 1:nrow(landmarks1), FUN = graph)
(plot_list[[1]] + plot_list[[2]] + plot_list[[3]]) /
  (plot_list[[4]] + plot_list[[5]] + plot_list[[6]]) +
  plot_annotation(title = make_title, caption="#30DayMapChallenge Day 12 Scale | Source: US Census | Method from @KittJonathan and @samiasab90") & 
  theme(plot.title = element_markdown(family=f1, hjust=.5, face="bold", size=15),
        plot.caption=element_text(size=7.5, hjust=.5, color=map_color),
        plot.margin=margin(.5,.5,.3,.5,unit="cm"))
        
ggsave("12_scale.png", bg="white")
  