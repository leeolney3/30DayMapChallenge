# 30 mins by car, bike and foot from Ravenna Ln, Katy, TX
# Isochrone map, method from @VictimOfMaths (https://twitter.com/VictimOfMaths/status/1462891155998355456)

library(showtext)
font_add_google("Roboto Mono")
showtext_auto()
library(osrm)
library(sf)
library(tidyverse)
library(ggmap)
library(ragg)
library(ggtext)
library(tmaptools)
library(ggspatial)

car <- osrmIsochrone(loc = c(-95.7688245, 29.7240658), breaks = c(30),
                     returnclass="sf", res=100, osrm.profile="car")

bike <- osrmIsochrone(loc = c(-95.7688245, 29.7240658), breaks = c(30),
                     returnclass="sf", res=70, osrm.profile="bike")

foot <- osrmIsochrone(loc = c(-95.7688245, 29.7240658), breaks = c(30),
                     returnclass="sf", res=70, osrm.profile="foot")

 
bb(car, ext=1.1) # get bounding box

bbox <- c(bottom=29.45779, top=30.08189, left=-96.08594, right=-95.38632)

map <- get_stamenmap(bbox, zoom=12, maptype="toner-background")

agg_png("30_isochrone1.png", units="in", width=7.3, height=8, res=500)

ggmap(map, darken=.5) +
	geom_sf(data=car, aes(geometry=geometry), 
	fill="#ff9f1c", alpha=0.5, inherit.aes=FALSE, colour=NA)+
  	geom_sf(data=bike, aes(geometry=geometry), 
  	fill="#00a896", alpha=0.5, inherit.aes=FALSE, colour=NA)+
  	geom_sf(data=foot, aes(geometry=geometry), 
  	fill="#4059ad", alpha=0.6, inherit.aes=FALSE, colour=NA) +
  	theme_void() +
  	theme(
  		plot.title=element_markdown(face="bold", size=rel(7), margin=margin(b=5),
  		family="Roboto Mono"),
  		plot.caption=element_markdown(size=rel(4), margin=margin(t=11),family="Roboto Mono", hjust=.5),
  		plot.subtitle=element_markdown(size=rel(5), margin=margin(b=8),family="Roboto Mono"),
  		plot.margin=margin(.5,.5,.5,.5, unit="cm")
  	) +
  	labs(title="30 MINUTES BY <span style='color:#ff9f1c;'>CAR</span>, <span style='color:#00a896;'>BIKE</span> and <span style='color:#4059ad;'>FOOT</span>",
  		subtitle="from Ravenna Ln, Katy, TX",
  		caption="#30DayMapChallenge Nov 2021 | Method from @VictimOfMaths | Data: © OpenStreetMap contributors")
  	
dev.off()

