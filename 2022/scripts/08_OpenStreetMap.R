# 30DayMapChallenge 2022
# Day 8 Data: OpenStreetMap

# Libraries
library(osmdata)
library(sf)
library(tidyverse)
library(ggtext)
library(showtext)
showtext_opts(dpi = 300)
showtext_auto(enable = TRUE)

# Font
font_add_google("Inter")
f1 = "Inter"

# Boundary from Analyze Boston
boundary = read_sf("City_of_Boston_Boundary/City_of_Boston_Boundary.shp") #source: https://data.boston.gov/dataset/city-of-boston-boundary
street= read_sf("City_of_Boston_Managed_Streets/City_of_Boston_Managed_Streets.shp") #source: https://data.boston.gov/dataset/city-of-boston-managed-streets
boundary2 = st_transform(boundary,crs=4326)
st_bbox(boundary2)

bbox= matrix(c(-71.19084,-70.92347,42.22789,42.39694), byrow = TRUE, nrow = 2, ncol = 2, dimnames = list(c('x','y'),c('min','max')))

# Parks polygons
# Reference: http://joshuamccrain.com/tutorials/maps/streets_tutorial.html
park <- bbox %>% opq()%>%
  add_osm_feature(key = "leisure", value = "park") %>%
  osmdata_sf()

# Libraries points 
# Reference: https://rpubs.com/stragu/sotm2021
library = bbox %>% opq()%>%
  add_osm_feature(key = "amenity", value = "library") %>%
  osmdata_sf()
point = library$osm_points %>% dplyr::filter(amenity == "library")
centroid = library$osm_polygons %>% st_centroid()
library2 = bind_rows(centroid, point)

# Get intersection and transform crs
sf_use_s2(FALSE)
parkpoly2 = st_intersection(parkpoly,boundary2)
parkpoly2 = st_transform(parkpoly2,crs=raster::crs(boundary))
library3 = st_intersection(library2,boundary2)
library3 = st_transform(library3,crs=raster::crs(boundary))

# Plot
ggplot() +
  geom_sf(data=boundary, color=NA, fill="grey95") +
  geom_sf(data=street, size=.1, color="black") +
  geom_sf(data=parkpoly2, color="#0C5449", fill="#0C5449", size=.1) +
  geom_sf(data=library3, color="#FB5606", fill="#FB5606", alpha=.7, size=1) +
  coord_sf(expand=FALSE) +
  cowplot::theme_map(10) +
  theme(plot.title=element_markdown(family=f1,lineheight=1.3,color="grey60",margin=margin(l=320,t=280, b=-280)),
        plot.caption=element_text(size=7,family=f1,color="grey40",hjust=.5, margin=margin(t=10))) +
  labs(title="<span style='color:#000000'>City of Boston</span><br><span style='color:#0C5449'>Parks</span> and <span style='color:#FB5606'>Libraries</span>",
       caption="#30DayMapChallenge Day 8 Data: OpenStreetMap | Source: © OpenStreetMap contributors")

ggsave("08_OpenStreetMap.png", height=6, width=7, bg="#fafafa")