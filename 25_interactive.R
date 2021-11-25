#30DayMapChallenge 25 Interactive Map
# Vancouver bikeways, traffic signals and viewcones

# Data from City of Vancouver Open Data Portal 
#https://opendata.vancouver.ca/explore/dataset/bikeways/
#https://opendata.vancouver.ca/explore/dataset/traffic-signals/information/
#https://opendata.vancouver.ca/explore/dataset/view-cones/

# import shp
view_cones = read_sf("data/view-cones/view-cones.shp")
bikeways = read_sf("data/bikeways/bikeways.shp")
traffic_signals = read_sf("data/traffic-signals/traffic-signals.shp")

# interactive map
tmap_mode("view")
m = tm_basemap("Stamen.Toner") +
  tm_shape(bikeways) + tm_lines(col="#2ec4b6", lwd=1.2) +
  tm_shape(traffic_signals) + tm_dots(col="#F15C17") +
  tm_shape(view_cones) + tm_polygons(col="#e9c46a", alpha = .2, border.col = "#e9c46a") +
  tm_add_legend(title='Vancouver',type="fill",col=c("#2ec4b6","#F15C17","#e6bd59"), labels=c("Bikeways","Traffic Signals","View Cones"))

# save  
tmap_save(m, "interactive_map.html")

