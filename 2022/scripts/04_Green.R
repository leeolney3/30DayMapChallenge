# 30DayMapChallenge 2022
# Day 4 Color Friday: Green
# Parks in Arlington County, VA
# Data source: https://gisdata-arlgis.opendata.arcgis.com/datasets/ArlGIS::park-polygons/explore?location=38.882168%2C-77.106700%2C13.00

# Libraries
library(tidyverse)
library(sf)
library(ggtext)
library(showtext)
showtext_opts(dpi = 300)
showtext_auto(enable = TRUE)

# Font
font_add_google("Barlow")
f1 = "Barlow"

# Data
park = read_sf("Park_Polygons/Park_Polygons.shp") #https://gisdata-arlgis.opendata.arcgis.com/maps/park-polygons
boundaries= read_sf("Arlington_County_Boundary_Polygon/County_poly.shp") #https://gisdata-arlgis.opendata.arcgis.com/maps/arlington-county-boundary-polygon
centerline = read_sf("Street_Centerline_RRCL_Arl_Only/RRCL_Streets_Edit_arc.shp") #https://gisdata-arlgis.opendata.arcgis.com/search?q=park&sort=-modified

park1 = park %>%
  mutate(grp=case_when(Ownership=="Arlington County Park"~"Arlington County Park",
                       TRUE~"Others"),
         grp=factor(grp,levels=c("Arlington County Park","Others")))

# Map
pal=c("#386641","#5BBA6F")
title= "<span style = 'font-size:13pt'>**Parks in<br>Arlington County, VA**</span><br><br>Ownership:"
ggplot() +
  geom_sf(data=boundaries, color=NA, fill="grey90") +
  geom_sf(data=centerline, color="white", size=.3) +
  geom_sf(data=park1,aes(fill=grp, color=grp)) +
  scale_fill_manual(values=pal) +
  scale_color_manual(values=pal) +
  cowplot::theme_map(10) +
  theme(text=element_text(family=f1),
        legend.position = c(.055,.18),
        legend.title=element_markdown(size=9,lineheight = 1.3,face="plain",hjust=0),
        plot.caption=element_markdown(size=7.5, lineheight=1.2, color="grey30",hjust=0, margin=margin(l=25)),
  ) +
  labs(fill=title, color=title,
       caption="Note: Others ownership includes Arlington County DES, NVRPA Park, Private Open Space and State Department<br>#30DayMapChallenge Day 4 Green  •  Source: Arlington GIS Open Data Site")

ggsave("04_Green.png", height=7, width=7, bg="#fafafa")