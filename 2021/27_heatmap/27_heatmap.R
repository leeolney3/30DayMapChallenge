#30DayMapChallenge 27 Heatmap
#Portland bicycle parking heatmap as of Aug 6, 2021
#Data from PortlandMaps - Open Data and © OpenStreetMap contributors

# load libaries
library(tidyverse)
library(sf)
library(osmdata) 
library(osmplotr) 

# load fonts 
library(showtext)
font_add_google("Lato")
font_add_google("Libre Franklin")
showtext_auto()

# bicycle parking shp file
#source: https://gis-pdx.opendata.arcgis.com/datasets/bicycle-parking (updated: Aug 6, 2021)
parking = read_sf("data/Bicycle_Parking/Bicycle_Parking.shp")

p2 = st_transform(parking, crs="+proj=longlat +datum=WGS84 +no_defs") #projection
st_write(p2, "p2.csv", layer_options = "GEOMETRY=AS_XY") 
p3 = read_csv("p2.csv")

# get osm data
#raster::extent(p2)
osm_bbox <- get_bbox (c(-122.7859,45.45963,-122.497,45.59913))

big_streets = opq(osm_bbox)%>%
  add_osm_feature(key = "highway", 
                  value = c("motorway", "primary", "motorway_link", "primary_link")) %>%
  osmdata_sf()
  
med_streets <- opq(osm_bbox)%>%
  add_osm_feature(key = "highway", 
                  value = c("secondary", "tertiary", "secondary_link", "tertiary_link")) %>%
  osmdata_sf()


small_streets <- opq(osm_bbox)%>%
  add_osm_feature(key = "highway", 
                  value = c("residential", "living_street","unclassified","service", "footway")) %>%
  osmdata_sf()

water <- opq(osm_bbox)%>%
  add_osm_feature(key = "natural", value = c("water")) %>%
  osmdata_sf()
  
# basemap
basemap1 = ggplot() +
  geom_sf(data=water$osm_multipolygons, color=NA) + 
  geom_sf(data = big_streets$osm_lines,inherit.aes = FALSE,
          color = "black",size = .45,alpha = .6) +
  geom_sf(data = med_streets$osm_lines,inherit.aes = FALSE,
          color = "black",size = .3,alpha = .5) +
  geom_sf(data = small_streets$osm_lines,inherit.aes = FALSE,
          color = "#666666",size = .2,alpha = .3) +
  coord_sf(expand=F, xlim=c(-122.709, -122.605), ylim=c(45.499,45.567))
  
# add points and density 2d
map = basemap1 + 
  stat_density2d(data=p3,  aes(x=X, y=Y, fill=..level.., alpha=..level..), 
                 geom="polygon") +
  rcartocolor::scale_fill_carto_c(palette = "ag_GrnYl") +
  #scale_fill_gradientn(colours=rev(brewer.pal(7, "Spectral"))) +
  scale_alpha_continuous(range=c(.3,.5)) +
  geom_point(data=p3, aes(x=X, y=Y), size=.3, alpha=.15, color="#f72585") +
  theme_void(base_family="Lato") +
  theme(legend.position="none",
        plot.margin=margin(t=.3, b=.2, unit="cm"),
        plot.title=element_text(size=28,family="Libre Franklin",hjust=.5,face="bold", margin=margin(b=4)),
        plot.caption=element_text(size=17, hjust=.5),
        plot.subtitle=element_text(hjust=.5, size=20,margin=margin(b=7))
        ) +
  labs(caption="#30DayMapChallenge Nov 2021  |  Data: PortlandMaps - Open Data and © OpenStreetMap contributors",
       title="Portland Bicycle Parking Heat Map",
       subtitle="Bike racks installed in the public right-of-way in the City of Portland.")
       
# save png
ggsave("26_heatmap.png", height=5, width=5, units="in", bg="white")
  

