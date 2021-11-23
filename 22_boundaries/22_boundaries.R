#30DayMapChallenge 22 Boundaries
#Sedimentary Basin Boundaries in Lower 48 States (10/8/2019)

library(tidyverse)
library(sf)
library(tmap)
library(raster)
library(ggtext)

library(sysfonts)
library(showtext)
font_add_google("Libre Franklin")
font_add_google("Lato")
showtext_auto()

# source: https://www.eia.gov/maps/maps.htm
basin = read_sf("data/SedimentaryBasins_US_EIA/SedimentaryBasins_US_May2011_v2.shp")
basin2 = st_make_valid(basin)
st_bbox(basin2)

# source: https://www.arcgis.com/home/item.html?id=ca593122db784b2faefe39309fa519c7
states = read_sf("data/cb_2020_us_state_20m/cb_2020_us_state_20m.shp")
states2 = st_transform(states, crs(basin))

ggplot() +
  geom_sf(data=states2, col="#6b9080", fill=NA, size=.2) +
  geom_sf(data=basin3, col="#F2F2F2", fill="#f6bd60") +
  geom_sf_text(data=basin2 %>% dplyr::filter(name!="Ardmore"& name!="Marietta"), 
               aes(label=name), size=1.8, family="Lato") + 
  ggrepel::geom_text_repel(
    data=basin2 %>% dplyr::filter(name=="Ardmore" | name=="Marietta"),
    aes(label=name, geometry=geometry),
    stat="sf_coordinates",
    min.segment.length = 0,
    size=1.8,
    color="black",
    family="Lato"
  ) +
  coord_sf(expand=F, xlim=c(-124, -73.60354),
           ylim=c(25.77249,49.00245)) +
  theme_void() +
  theme(plot.background = element_rect(fill="#F2F2F2", color=NA)) +
  annotate(geom="richtext", x=-123, y=26.5, hjust=0, vjust=0, size=3, family="Lato",
           fill = NA, label.color = NA,label.padding = grid::unit(rep(0, 4), "pt"),
           label="Sedimentary Basin **Boundaries** in Lower 48 States<br><span style = 'font-size:5pt;'>U.S. Oil and Gas, 10/8/2019<br>#30DaysMapChallenge 2021 | Data from U.S. Energy Information Administration</span>")
           
# save
ggsave("22_boundaries.pdf", width=7, height=3.95, unit="in")
ggsave("22_boundaries.png", width=7, height=3.95, unit="in")


#NY zipcode and borough boundaries with water

# source: https://data.cityofnewyork.us/City-Government/Borough-Boundaries/tqmj-j8zm
borough_water = read_sf("data/Borough_Boundaries _Water_Areas_Included/geo_export_750f6aad-8ccd-46d1-b43b-9f6f3858ec32.shp")

# source: https://data.cityofnewyork.us/Business/Zip-Code-Boundaries/i8iw-xf4u
zipcode = read_sf("data/ZIP_CODE_040114/ZIP_CODE_040114.shp")
zipcode2 = st_transform(zipcode, raster::crs(borough))

font_add_google("Fira Sans Condensed")
f1= "Fira Sans Condensed"

p2 = tm_shape(borough_water) + tm_fill(col="#023d67") +
  tm_shape(borough) + tm_fill(col="#4a4e69") +
  tm_shape(zipcode2) + tm_polygons(col="#f25c54", lwd=.65, border.col = "#023d67")+
  tm_shape(borough_water) + tm_borders(col="white") + 
  tm_text("boro_name", size=.6, col="white", fontfamily = f1) +
  tm_credits("#30DayMapChallenge Nov 2021 | Data from New York Open Data", size=.65,
             fontfamily = f1)  +
  tm_layout(frame=FALSE, 
            title='NEW YORK\nZip code boundaries and\nBoroughs boundaries with water area',
            title.fontfamily = f1, title.size = 1.1, bg.color = "#fafafa")
            
tmap_save(p2, "22_p2.png")


#NY boundaries: Community Districts, Neighbourhood Tabluation Areas (NTAs), Park districts and zipcode

# source:https://data.cityofnewyork.us/City-Government/Community-Districts/yfnk-k7r4
community = read_sf("data/Community_Districts/geo_export_e9f6edab-f816-4b0a-8d57-1987794f937e.shp")
# source: https://data.cityofnewyork.us/City-Government/2010-Neighborhood-Tabulation-Areas-NTAs-/cpf4-rkhq
nta2010 = read_sf("data/2010_NTAs/geo_export_4b6c640a-3e0d-4572-b35b-8024b142508a.shp")
# source: https://data.cityofnewyork.us/City-Government/NYC-Parks-Districts-Map/w8cn-6mjd
park = read_sf("data/NYC_Parks_Districts/geo_export_c615e940-5700-4e44-9460-64ad8a02e8d9.shp")

f1 = community %>% select(geometry) %>% mutate(grp="Community Districts Boundaries")
f2 = nta2010 %>% select(geometry) %>% mutate(grp="Neighborhood Tabulation Area Boundaries")
f3 = park %>% select(geometry) %>% mutate(grp="Park Districts Boundaries")
f3 = st_make_valid(f3)
f4 = zipcode2 %>% select(geometry) %>% mutate(grp="Zipcode Boundaries")

combined = rbind(f1,f2,f3,f4)

tm_shape(combined) + 
  tm_borders(lwd=.5, col="#d5f2e3") +
  tm_layout(frame=FALSE, frame.lwd = NA, panel.label.bg.color = NA, 
            panel.label.fontfamily = f1, panel.label.color = "#d5f2e3",
            bg.color = "#143642") +
  tm_facets(by="grp")

