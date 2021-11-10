#30DayMapChallenge 10 Monochrome 
#Kilauea, Hawaii 96754, USA
#USGS elevation data from the ArcGIS REST API and map data from openstreetmap.org

# load libaries 
library(raster)
library(rayshader)
library(osmdata)
library(sf)
library(tidyverse)

## Part 1: get elevation data
#reference: https://wcmbishop.github.io/rayshader-demo/

# bounding box of Kilauea, Hawaii 96754, USA (https://boundingbox.klokantech.com/)
bbox <- list(
  p1 = list(long = -159.422598, lat = 22.196185),
  p2 = list(long = -159.392682, lat = 22.22117)
)

# set image size 
# define_image_size function reference: https://github.com/wcmbishop/rayshader-demo/blob/master/R/image-size.R
image_size <- define_image_size(bbox, major_dim = 600)

# download elev data
# get_usgs_elevation_data function reference: https://github.com/wcmbishop/rayshader-demo/blob/master/R/elevation-api.R
elev_file <- file.path("data", "sf-elevation.tif")
get_usgs_elevation_data(bbox, size = image_size$size, file = elev_file,
                        sr_bbox = 4326, sr_image = 4326)

# load elevation data
elev_img <- raster(elev_file)
elev_matrix <- matrix(
  extract(elev_img, extent(elev_img), buffer = 1000), 
  nrow = ncol(elev_img), ncol = nrow(elev_img)
)

## Part 2: base map with road from osm 
# code adapted from https://www.javierorraca.com/posts/2021-09-07-rayshader/ and https://www.tylermw.com/adding-open-street-map-data-to-rayshader-maps-in-r/

# Open street map roads
osm_bbox = c(-159.422598,22.196185,-159.392682,22.22117)

highway_sf <- opq(osm_bbox) %>% 
  add_osm_feature("highway") %>% 
  osmdata_sf() 

# Transform lines into new projection
lines <- st_transform(highway_sf$osm_lines,crs = crs(elev_img))

big_st = lines %>% filter(highway %in% c("motorway", "primary", "motorway_link", "primary_link"))

small_st = lines %>% filter(highway %in% c("residential", "living_street",
                            "unclassified","service","footway"))
 
rcartocolor::carto_pal(n=7, name="OrYel")

# plot
elev_matrix %>% 
  height_shade(texture = (grDevices::colorRampPalette(c("#0d585f","#287274","#448c8a",
  "#63a6a0","#89c0b6","#b4d9cc","#e4f1e1")))(256)) %>%
  add_shadow(lamb_shade(elev_matrix),0.6) %>%
  add_shadow(ambient_shade(elev_matrix), 0) %>%
  add_shadow(texture_shade(elev_matrix,detail=8/10,contrast=9,brightness = 11), 0.1) %>%
  add_overlay(
    generate_line_overlay(
      big_st, extent = extent(elev_img),
      linewidth = 3, color = "white",
      heightmap = elev_matrix)) %>% 
  add_overlay(
    generate_line_overlay(
      small_st, extent = extent(elev_img),
      linewidth = 2, color = "white",
      heightmap = elev_matrix)) %>% 
  plot_map()