#30DayMapChallenge 20 Movement 
#Bike data: https://ride.citibikenyc.com/system-data
#Map data: © OpenStreetMap contributors
#Method from @quite_grey https://twitter.com/quite_grey/status/1411785842520203266

# load libraries
library(sfnetworks)
library(tidygraph)
library(osmdata)
library(sf)
library(ggspatial)
library(tidyverse)
library(ggtext)

# load text
library(sysfonts)
library(showtext)
font_add_google("Lato")
showtext_auto()

# import bike data
bikesdf = read_csv("data/JC-202110-citibike-tripdata.csv")

# find most common start station
st = bikesdf %>% count(start_station_name, sort=T) %>% pull(start_station_name)

# Grove St Path station location
station <- st_as_sf(st_sfc(st_point(c(-74.04312, 40.71959))), crs = 4326)

# get unique trips from Grove St Path
grove = bikesdf %>% filter(start_station_name==st) %>%
  select(start_station_name, start_station_id, end_station_name,end_station_id,
         latitude= end_lat,longitude=end_lng) %>%
  drop_na() %>% distinct()
#there are 4112 trips originating from Grove St Path station with 86 unique trips

# boundary box
bbox <- c(min(grove$longitude)-0.01, 
          min(grove$latitude)-0.01, 
          max(grove$longitude)+0.01, 
          max(grove$latitude)+0.01)
bbox

# base map (without motorways)
queries <- list()
 
queries[["highway"]] <- opq(bbox = bbox) %>%
  add_osm_feature(key = "highway", value = c("secondary", "tertiary", "secondary_link", "tertiary_link","residential", "living_street","unclassified","service","footway")) %>% 
  osmdata_sf

lines <- queries[["highway"]] %>% 
  pluck("osm_lines") %>% 
  select(osm_id, geometry) 

# turn lines into sfnetwork  
net <- lines %>% 
  as_sfnetwork(., directed = FALSE) %>%
  activate("edges") %>%
  mutate(weight = edge_length()) %>% 
  filter(!edge_is_multiple()) %>%
  filter(!edge_is_loop()) %>% 
  convert(., to_spatial_subdivision)
  
# find shortest path
paths <- st_network_paths(net, from = station %>% st_geometry, to = bike_sf %>% st_geometry)

# create df from path list
df <- paths %>% 
  mutate(id = row_number(),
         route = map(edge_paths, ~ net %>% activate("edges") %>% slice(.x %>% unlist) %>% st_as_sf)) %>%
  select(id, route) %>% 
  unnest(route) %>% 
  st_as_sf %>% 
  mutate(from_to = str_c(from, to, sep = "_")) %>%
  group_by(from_to) %>% 
  mutate(count = n()) %>% 
  ungroup()
  
# plot
ggplot() +
  annotation_spatial(lines,
                     size = 0.1,
                     colour = "grey30") +
  annotation_spatial(df, aes(size = count),
                     colour = "grey90",
                     show.legend = FALSE) +
  layer_spatial(bike_sf, shape = 21,
                  colour = "grey30",
                  fill = "#e6d94f",
                  size = 2) +
  annotation_spatial(station,
                     shape = 22,
                     colour = "grey30",
                     fill = "#5298AF",
                     size = 3.7) +
  scale_size(range = c(0.25, 1)) +
  theme_void() +
  theme(panel.background = element_rect(fill = "#001219", colour = NA),
        #plot.margin = unit(c(1,1,1,1), "cm"),
        ) +
  annotate(geom="richtext", fill = NA, label.color = NA, 
           label.padding = grid::unit(rep(0, 4), "pt"), family="Lato",
           x=-74.085, y=40.753, hjust=0, vjust=1,color="white",size=3.5, lineheight=1.3,
           label="Jersey City Citi Bike trips <span style = 'color:#e6d94f;'>ending stations</span> from <br><span style = 'color:#5298AF;'>Grove St PATH station</span> in October 2021<br><span style = 'font-size:6pt;color:grey90'>#30DayMapChallenge Nov 2021 | Data: Citi Bike and OpenStreetMap")
          
# save plot
ggsave("bike.pdf", width=6.94, height=7, units="in", bg="#192233")

  


