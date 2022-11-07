# 30DayMapChallenge 2022
# Day 7 Raster
# Denmark elevation in meters

# Libraries 
library(tidyverse)
library(sf)
library(ggtext)
library(showtext)
showtext_opts(dpi = 300)
showtext_auto(enable = TRUE)

# Font
font_add_google("Fira Sans")
f1 = "Fira Sans"
# Data
r= geodata::elevation_30s(country="DNK", path=tempdir(),mask=TRUE)
rdf<-as.data.frame(r,xy=TRUE)%>%drop_na()
denmark = rnaturalearth::ne_countries(scale=10, country = "denmark", returnclass = "sf")

# Raster
pal= PNWColors::pnw_palette("Lake",5)
title= "<span style = 'font-size:15pt'>Denmark</span><br>Elevation (in meters)"

ggplot()+
  #geom_raster(data=rdf,aes(x=x,y=y,fill=DNK_elv_msk)) +
  geom_tile(data=rdf,aes(x=x,y=y,fill=DNK_elv_msk, color=DNK_elv_msk)) +
  scale_fill_stepsn(colors = pal) +
  scale_color_stepsn(colors = pal) +
  geom_sf(data=denmark, color="transparent", fill="transparent") +
  coord_sf() +
  cowplot::theme_map(11) +
  theme(legend.position=c(.7,.7),
        legend.direction = "horizontal",
        legend.title=element_markdown(family=f1, lineheight=1.2),
        plot.caption=element_text(family=f1, color="grey40", hjust=.5)) +
  guides(fill=guide_colorsteps(title.position="top",barwidth = unit(8, "lines"),barheight = unit(.6, "lines")),color=guide_colorsteps(title.position="top",barheight = unit(.6, "lines"), barwidth = unit(8, "lines"))) +
  labs(fill=title, color=title,
       caption="#30DayMapChallenge Day 7 Raster | Source: SRTM via {geodata} R package")

ggsave("07_Raster.png", height=6, width=8, bg="white")