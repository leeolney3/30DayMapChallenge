# 30DayMapChallenge 2022, 18 Colour Friday: Blue
# Source: US Census via {tigris} R package

# Libraries
library(tidyverse)
library(tigris)
library(sf)
library(ggtext)
library(showtext)
showtext_opts(dpi = 300)
showtext_auto(enable = TRUE)

# Fonts
font_add_google("Sora")
f1 = "Sora"
font_add_google("Open Sans")
f2 = "Open Sans"

# Data
harris_area = area_water("texas", "harris county", year = 2022) 
harris_linear = linear_water("texas", "harris county", year = 2022) 

# Map
ggplot() +
  geom_sf(data=harris_areawater, fill="#173C6D",color="#173C6D", size=.4) +
  geom_sf(data=harris_linear_water, color="#0096c7", size=.3) +
  coord_sf() +
  theme_void() +
  theme(plot.subtitle=element_markdown(size=13, lineheight = 1, face="bold", hjust=.5, margin=margin(t=-10)),
        plot.title = element_markdown(family=f2, size=7.5, lineheight=1.4,margin=margin(l=20,t=350, b=-350)),
        plot.caption=element_text(family=f2,color="#33415c",hjust = .5, size=7, margin=margin(t=-5)),
        plot.margin=margin(t=-1.2,b=.3,unit="cm")) +
  labs(subtitle="Harris County, Texas", 
       title="<span style='font-size:12pt;color:#173C6D'><b>Area hydrography</b></span><br><span style='color:#173C6D;'>(ponds, lakes, oceans, swamps)</span><br><br><span style='font-size:12pt;color:#0096c7'><b>Linear hydrography</b></span><br><span style='color:#0096c7'>(streams/rivers, braided streams, canals,<br>ditches, artificial paths, aqueducts)</span>", 
       caption="#30DayMapChallenge 18 Color Friday: Blue | Source: US Census via {tigris} R package")

ggsave("18_Blue.png", height=6, width=7.5, bg="#eff2f1") 