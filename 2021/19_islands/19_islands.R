#30DayMapChallenge 19 Island(s)
#Abu Dhabi (Al Lulu Island, Al Maryah Island, Al Reem Island, Umm Yifenah Island, Saadiyat Island and surrounding wetlands) 
#Data source: © OpenStreetMap contributors

# load libraries 
library(tidyverse)
library(osmdata)
library(osmplotr)
library(ggtext)

# load fonts
library(sysfonts)
library(showtext)
font_add_google("Roboto Condensed")
font_add_google("Libre Franklin")
font_add_google("Lato")
showtext_auto()

f1 = "Roboto Condensed"
f2 = "Libre Franklin"
f3 = "Lato"

# data osm data
bbox <- get_bbox(c(54.328879,24.471307,54.478396,24.588745))

coast<- bbox %>%
  opq()%>%
  add_osm_feature(key = "natural", 
                  value = "coastline") %>%
  osmdata_sf()
  
wetland<- bbox %>%
  opq()%>%
  add_osm_feature(key = "natural", 
                  value = "wetland") %>%
  osmdata_sf()

big_streets <- opq(bbox) %>%
  add_osm_feature(key = "highway", 
                  value = c("motorway", "primary", "motorway_link", "primary_link")) %>%
  osmdata_sf()

med_streets <- bbox %>%
  opq()%>%
  add_osm_feature(key = "highway", 
                  value = c("secondary", "tertiary", "secondary_link", "tertiary_link")) %>%
  osmdata_sf()

small_streets <- bbox %>%
  opq()%>%
  add_osm_feature(key = "highway", 
                  value = c("residential", "living_street","unclassified","service", "footway"
                  )) %>%
  osmdata_sf()

# plot
ggplot() +
  #wetlands
  geom_sf(data = wetland$osm_polygons,inherit.aes = FALSE,
          fill = "#006d77", color="#458d99",lwd=.2) +
  geom_sf(data=wetland$osm_multipolygons,inherit.aes = FALSE,
          fill = "#006d77", color="#458d99",lwd=.2) +
  # coast
  geom_sf(data=coast$osm_lines, size=.2) +
  geom_sf(data=coast$osm_polygons, fill="#D5EC4D", color="#D5EC4D", size=.2,
          show.legend=F) +
  # street
  geom_sf(data=med_streets$osm_lines, color="white",alpha = .8, size=.3) +
  geom_sf(data=big_streets$osm_lines, color="white",alpha = .8) +
  geom_sf(data=small_streets$osm_lines, color="white",alpha = .6, size=.2) +
  # annotate
  annotate(geom="text", x=54.345, y=24.50, label="Al Lulu Island", size=2.2, family=f1) +
  annotate(geom="text", x=54.41, y=24.49, label="Al Reem Island", size=2.2, family=f1) +
  annotate(geom="text", x=54.39, y=24.502, label="Al\nMaryah\nIsland", size=2.2, family=f1,lineheight=0.9) +
  annotate(geom="text", x=54.44, y=24.53, label="Saadiyat Island", size=2.2, family=f1) +
  annotate(geom="text", x=54.45, y=24.487, label="Umm Yifenah Island", size=2.2, family=f1) +
  annotate(geom="text", x=54.372, y=24.53, label="Al Mina", size=2.2, family=f1) +
  annotate(geom="text", x=54.372, y=24.49, label="Abu Dhabi", size=3, family=f1) +
  annotate(geom="richtext", x=54.333, y=24.57, 
           label="<span style = 'font-size:14pt;color:black;'>**ABU DHABI**</span><br>**Al Lulu** <span style = 'color:#ddef70;'>**Island**</span><br>**Al Maryah** <span style = 'color:#ddef70;'>**Island**</span><br>**Al Reem** <span style = 'color:#ddef70;'>**Island**</span><br>**Umm Yifenah** <span style = 'color:#ddef70;'>**Island**</span><br>**Saadiyat** <span style = 'color:#ddef70;'>**Island**</span><br><span style = 'color:black;font-size:6pt'>and surrounding</span> <span style = 'color:#006d77;font-size:8pt'>**wetlands**</span>", 
           size=3, fill = NA, label.color = NA, family=f2, hjust=0, color="white",
           lineheight=1.4) +
  annotate(geom="richtext", x=54.333, y=24.554, fill = NA, label.color = NA, family=f3, hjust=0,label="#30DayMapChallenge Nov 2021<br>Data: © OpenStreetMap contributors", size=1.7) +
  coord_sf(expand=F,
           xlim=c(54.328879,54.478396),
           ylim=c(24.471307,24.588745)) +
  theme_void() +
  theme(plot.background=element_rect(fill="#8e9aaf", color=NA)) 
  
# save
ggsave("19_islands.pdf", width=7, height=6, unit="in")
ggsave("19_islands.png", width=7, height=6, unit="in")


