#30DayMapChallenge 29 NULL
#Data from data.boston.gov
#Central Boston: Parcels with 0% change in tree canopy from 2014 to 2019

#parcels: https://bostonopendata-boston.opendata.arcgis.com/datasets/boston::canopy-change-assessment-tree-canopy-change-metrics?layer=8
#neighborhood: https://bostonopendata-boston.opendata.arcgis.com/datasets/boston::canopy-change-assessment-tree-canopy-change-metrics?layer=6

library(tidyverse)
library(sf)

library(ggtext)
library(showtext)
font_add_google("Lato")
font_add_google("Libre Franklin")
showtext_auto()

# parcel canopy change
canopy = read_sf("data/Canopy_Change_Assessment%3A_Tree_Canopy_Change_Metrics/Canopy_Change_Assessment%3A_Tree_Canopy_Change_Metrics.shp")
c2 = st_make_valid(canopy)

# nbhd area boundaries
areas = read_sf("data/Canopy_Change_nbh/Canopy_Change_Assessment%3A_Tree_Canopy_Change_Metrics.shp") 
a2 = st_make_valid(areas)

# check crs
raster::crs(c2)
raster::crs(a2)

# central boston mask
central_boston = a2 %>% filter(Name=="Central Boston")
canopy_cb = st_intersection(c2, central_boston)
# create group
canopy_cb = canopy_cb %>% mutate(grp = case_when(Change_P_1==0 ~ "no change",
                               TRUE ~ "change"))
                               
# plot
p1 = ggplot() +
  geom_sf(data=canopy_cb, aes(fill=grp),size=.12, color="#fafafa", show.legend=F) +
  scale_fill_manual(values=c("#317A6D","#D3A035")) +
  annotate(geom="richtext",fill = NA, label.color = NA,
           label.padding = grid::unit(rep(0, 4), "pt"),label="<span style='color:#fafafa'>CENTRAL BOSTON</span><br>Parcels with **0%** change in tree<br>canopy from 2014 to 2019",x=-71.076, y=42.357,  color="#D3A035", hjust=0, family="Libre Franklin", lineheight=1.4, fontface="bold", size=3.5,
           ) +
  coord_sf(expand=F) +
  theme_void() +
  theme(plot.caption=element_text(size=7.5, family="Lato",face="bold", color="#fafafa",
                                  margin=margin(t=10)),
        plot.margin=margin(.5,.5,.5,.1, unit="cm")) +
  labs(caption="#30DayMapChallenge Nov 2021 | Data from data.boston.gov")
  
# save
ggsave("29_null.pdf", p1, height=7, width=7, unit="in", bg="#317A6D")