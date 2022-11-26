# 30DayMapChallenge 2022
# Day 26 Island(s)
# Data source: https://geoportal.hawaii.gov/datasets/6c4ad137c0e54d398712fd2e3e68e50e_1/about 

# Libraries
library(tidyverse)
library(sf)
library(tigris)
library(showtext)
showtext_opts(dpi = 300)
showtext_auto(enable = TRUE)

# Font
font_add_google("DM Sans")
font_add_google("Roboto")

# Data
maui = tigris::counties(state="HI", year=2022, resolution = "20m") %>% filter(NAME=="Maui")
roads = tigris::roads(state="HI", county="Maui", year=2022)
area  = tigris::area_water(state="HI", county="Maui", year=2022)
#landmarks = landmarks(state="HI",type="area", year=2022)
#landmarks= st_intersection(landmarks, maui)

resv = read_sf("Reserves/Reserves.shp")
resv = st_transform(resv, crs=raster::crs(maui)) 
resv = st_intersection(resv, maui)

resv1 = resv %>% 
  mutate(type_defin1 = fct_lump(type_defin,4, other_level="Other reserves, preserves, parks"))
  
# Map
pal= c("#006241","#EED350","#D3705A","#48628A","#6fb29f")
ggplot() +
  geom_sf(data=maui, linewidth=.2, color=NA, fill="grey") +
  geom_sf(data=area, linewidth=.1, fill="white", color=NA) +
  geom_sf(data=resv1, linewidth=.1, aes(fill=type_defin1), color="white") +
  geom_sf(data=roads, color="white", linewidth=.2) +
  scale_fill_manual(values=pal) +
  scale_color_manual(values=pal) +
  coord_sf(expand=FALSE) +
  cowplot::theme_map(11.5) +
  theme(text=element_text(family="DM Sans"), 
        legend.position = c(.04,.15),
        legend.title=element_blank(),
        plot.title=element_text(hjust=.5, face="plain"),
        plot.subtitle=element_text(hjust=.5, family="Roboto"),
        plot.caption=element_text(size=8, color="grey30", hjust=.5, family="Roboto"),
        plot.margin=margin(t=.4,b=.2, r=.15,unit="cm"),
        ) +
  labs(title="Maui County, Hawaii, USA", subtitle="Reserves, Preserves and Parks as of August 2022",
       caption="#30DayMapChallenge Day 26: Island(s) | Source: Hawaii State Department of Land and Natural Resources, Division of Forestry and Wildlife")
       
ggsave("26_Islands.png",height=5.8, width=8, bg="white")