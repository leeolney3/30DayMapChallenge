#30DayMapChallenge 05 OSM
#Park Spaces in The Woodlands, U.S.

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

# park data
park <- getbb("The Woodlands United States")%>%
  opq()%>%
  add_osm_feature(key = "leisure", value = "park") %>%
  osmdata_sf()

# streets data
big_streets <- getbb("The Woodlands United States")%>%
  opq()%>%
  add_osm_feature(key = "highway", 
                  value = c("motorway", "primary", "motorway_link", "primary_link")) %>%
  osmdata_sf()

med_streets <- getbb("The Woodlands United States")%>%
  opq()%>%
  add_osm_feature(key = "highway", 
                  value = c("secondary", "tertiary", "secondary_link", "tertiary_link")) %>%
  osmdata_sf()


small_streets <- getbb("The Woodlands United States")%>%
  opq()%>%
  add_osm_feature(key = "highway", 
                  value = c("residential", "living_street",
                            "unclassified",
                            "service", "footway"
                  )) %>%
  osmdata_sf()

# river data
river <- getbb("The Woodlands United States")%>%
  opq()%>%
  add_osm_feature(key = "waterway", value = "river") %>%
  osmdata_sf()

# railway data
railway <- getbb("The Woodlands United States")%>%
  opq()%>%
  add_osm_feature(key = "railway", value="rail") %>%
  osmdata_sf()

# plot
ggplot() +
  geom_sf(data = park$osm_polygons,
          inherit.aes = FALSE,
          color = "#c1d033", fill="#c1d033") +
  geom_sf(data = river$osm_lines,
          inherit.aes = FALSE,
          color = "steelblue",
          size = .8,
          alpha = .6) +
  geom_sf(data = railway$osm_lines,
          inherit.aes = FALSE,
          color = "black",
          size = .2,
          linetype="dotdash",
          alpha = .5) +
  geom_sf(data = med_streets$osm_lines,
          inherit.aes = FALSE,
          color = "black",
          size = .3,
          alpha = .5) +
  geom_sf(data = small_streets$osm_lines,
          inherit.aes = FALSE,
          color = "black",
          size = .2,
          alpha = .4) +
  geom_sf(data = big_streets$osm_lines,
          inherit.aes = FALSE,
          color = "black",
          size = .5,
          alpha = .6) +
  coord_sf(xlim = c(-95.608, -95.43), 
           ylim = c(30.1, 30.237),
           expand = FALSE) +
  theme_void(base_family = "Libre Franklin") +
  theme(plot.title = element_text(size = 12,face="bold", hjust=.5),
        plot.caption=element_text(size=6, hjust=.5, family="Roboto"),
        plot.margin = margin(t = .5, r = 0, b = .5, l = 0, unit = "cm"),
        plot.background=element_rect(fill="white", color=NA),
        plot.subtitle = element_markdown(size = 8, hjust=.5, lineheight=1.25, margin=margin(2, 0, 5, 0))) +
  labs(title = "THE WOODLANDS, U.S.", 
       subtitle = "30.1658°N / 95.4613°W<br><span style='color:#c1d033;'>**PARK SPACES**</span>",
       caption="#30DayMapChallenge Day 05 | Nov 2021 | Map data © OpenStreetMap contributors")

# save plot
ggsave(file="05_OSM.png", units="in", width = 6, height=6.5)
ggsave(file="05_OSM.pdf”, units="in", width = 6, height=6.5)
