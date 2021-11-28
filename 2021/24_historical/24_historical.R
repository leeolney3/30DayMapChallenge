#30DayMapChallenge 24 Historical Map
#Portland downtown buildings built year
#Data from PortlandMaps - Open Data
#https://gis-pdx.opendata.arcgis.com/datasets/buildings/explore
#https://gis-pdx.opendata.arcgis.com/datasets/neighborhoods-regions/explore

# read shp
library(sf)
region = read_sf("data/Neighborhoods_(Regions)/Neighborhoods_(Regions).shp")
build = read_sf("data/Buildings/Buildings.shp")

# prepare data 
library(tidyverse)
downtown = region %>% filter(MAPLABEL=="Portland Downtown")
downtown2 = st_transform(downtown, "+proj=longlat +datum=WGS84 +no_defs")
build2 = st_transform(build, "+proj=longlat +datum=WGS84 +no_defs")

downtown3 = st_make_valid(downtown2)
build3 = st_make_valid(build2)
build_mask = st_intersection(build3, downtown3)

b1 = build_mask %>% mutate(YEAR_BUILT= na_if(YEAR_BUILT,0)) %>%
  mutate(year_bins = cut(YEAR_BUILT, breaks=c(1870,1899,1949,1999,2016),
                         labels=c("1900 before","1900 to 1949","1950 to 1999", "2000 to 2016")))

# load fonts                         
library(sysfonts)
library(showtext)
font_add_google("Lato")
showtext_auto()

# plot
ggplot() +
  #geom_sf(data =downtown3, color="white", fill="transparent") +
  geom_sf(data=b1, aes(fill=year_bins, color=year_bins),size=.2, alpha=.5)  +
  scale_fill_manual(values=c("#dc2f02","#ffb703","#a3d4eb","#1d8ca6"),
                    labels=c("Before 1900","1900 to 1949","1950 to 1999","2000 to 2016")) +
  scale_color_manual(values=c("#dc2f02","#ffb703","#a3d4eb","#1d8ca6"),
                    labels=c("Before 1900","1900 to 1949","1950 to 1999","2000 to 2016")) +
  coord_sf(expand=F) +
  theme_void(base_size = 9, base_family = "Lato") +
  theme(text=element_text(color="white"),
        plot.caption=element_text(size=6),
        plot.caption.position = "plot",
        plot.margin=margin(t=.25, b=.25, unit="cm"),
        legend.title=element_text(face="bold", lineheight = 1.2, margin=margin(b=4)),
        legend.key.size = unit(.8,"line"),
        plot.background=element_rect(fill="black", color=NA),
        ) +
  labs(fill="PORTLAND\nDowntown Buildings\nBuilt Year",
       color="PORTLAND\nDowntown Buildings\nBuilt Year",
       caption="#30DayMapChallenge Nov 2021\nData from PortlandMaps - Open Data")

# save
ggsave(".pdf", units = "in", width=7, height=7)