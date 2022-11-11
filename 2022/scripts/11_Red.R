# 30DayMapChallenge 2022
# Day 10 Color Friday: Red
# Data source: US Census 2022 via {tigris} R package

# Libraries
library(tidyverse)
library(tigris)
library(sf)
library(showtext)
showtext_opts(dpi = 300)
showtext_auto(enable = TRUE)

# Font
font_add_google("Barlow",bold.wt = 600)
f2 = "Barlow"

# Data
c = counties(state="TX", year=2022) %>% filter(NAME=="Red River")
r = roads(state="TX", county="Red River", year=2022)

# Map
ggplot() +
  geom_sf(data=c, fill="#692323", color=NA) +
  geom_sf(data=r, size=.11, color="white") +
  coord_sf(expand=FALSE) +
  cowplot::theme_map() +
  theme(plot.background=element_rect(fill="#F9F9F9", color=NA),
        plot.margin=margin(.5,2.5,.3,2.5,unit="cm"),
        plot.caption=element_text(hjust=.5, color="grey40", family=f2, size=7)) +
  annotate(geom="text", x=-94.75, y=33.9, label="Red River County, TX", hjust=1, family=f2,color="#692323", fontface="bold", size=4.8) +
  annotate(geom="text", x=-94.75, y=33.88, label="Road Network", hjust=1, family=f2,color="#692323", size=4.5) +
  labs(caption="#30DayMapChallenge Day 10 Color Friday: Red | Source: US Census via {tigris} R package")
  
ggsave("day11.png",height=7,width=7)