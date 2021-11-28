#30DayMapChallenge 06 Red
#Red light camera locations in the City of Toronto
#Data source: https://open.toronto.ca/dataset/red-light-cameras/ (last updated: Nov 1, 2021)

# load libraries 
library(sf)
library(tidyverse)
library(osmdata)
library(ggtext)

# load fonts
library(sysfonts)
library(showtext)
font_add_google("Libre Franklin")
font_add_google("Roboto")
showtext_auto()

# read RLC shp file
camera = read_sf('data/Red_Light_Cameras_Data/Red Light Cameras Data.shp')

# get road lines from OSM
bbox <- get_bbox (c (-79.61409, 43.5934, -79.14622, 43.8366))

big_streets <- get_bbox (c (-79.61409, 43.5934, -79.14622, 43.8366)) %>%
  opq()%>%
  add_osm_feature(key = "highway", 
                  value = c("motorway", "primary", "motorway_link", "primary_link")) %>%
  osmdata_sf()

med_streets <- get_bbox (c (-79.61409, 43.5934, -79.14622, 43.8366)) %>%
  opq()%>%
  add_osm_feature(key = "highway", 
                  value = c("secondary", "tertiary", "secondary_link", "tertiary_link")) %>%
  osmdata_sf()

small_streets <- get_bbox (c (-79.61409, 43.5934, -79.14622, 43.8366)) %>%
  opq()%>%
  add_osm_feature(key = "highway", 
                  value = c("residential", "living_street",
                            "unclassified",
                            "service", "footway"
                  )) %>%
  osmdata_sf()

# plot
ggplot() +
  geom_sf(data = med_streets$osm_lines,
          inherit.aes = FALSE,
          color = "black",
          size = .3,
          alpha = .5) +
  geom_sf(data = small_streets$osm_lines,
          inherit.aes = FALSE,
          color = "#666666",
          size = .2,
          alpha = .3) +
  geom_sf(data = big_streets$osm_lines,
          inherit.aes = FALSE,
          color = "black",
          size = .5,
          alpha = .6) +
  geom_sf(data = camera, size = 1.5, shape = 23, fill = "#ff002b", alpha=.7, stroke = .5) +
  coord_sf(xlim = c(-79.62, -79.14), 
           ylim = c(43.59, 43.84),
           expand = FALSE) +
  annotate(geom="richtext",fill = NA, label.color = NA, x=-79.26, y=43.65, 
           size=3, family="Libre Franklin",hjust=0, lineheight=1.3,fontface="bold",
           label="Locations of 194<br><span style='color:#b3001e;'>Red Light Cameras</span><br>in the City of Toronto") +
  annotate(geom="richtext",fill = NA, label.color = NA, x=-79.26, y=43.62, 
           size=1.8, family="Roboto",hjust=0, lineheight=1.3,
           label="The points identifies the intersections<br>where red light cameras are located<br>as of 01 November 2021.<br><br>Data source: open.toronto.ca<br>#30DayMapChallenge Day 6 Red") +
  theme_void() 

# save plot
ggsave(file="06_red.png", width=7, height=5, bg="white")
ggsave(file="06_red.pdf", width=7, height=5)