#30DayMapChallenge 02 Lines
#Water supply main pipelines from Melbourne Water Corporation. 
#Data source: https://discover.data.vic.gov.au/dataset/water-supply-main-pipelines (last updated: Nov 03, 2021)

# load libraries
library(sf)
library(tmap)

# read pipeline shp file
water_pipes_shp = read_sf('data/Water_Supply_Main_Pipelines-shp/Water_Supply_Main_Pipelines.shp')

# read region shp file
# source: https://www.planmelbourne.vic.gov.au/maps/spatial-data
mel = read_sf('data/Plan-Melbourne-Shape-Files/Administrative/State Regions_region.shp')

# plot
tm_shape(water_pipes_shp) + tm_lines(col="#ffff3f") + 
  tm_shape(mel) + tm_borders(col = "#577B77") + tm_polygons(col="#192a32") + 
  tm_shape(water_pipes_shp) + tm_lines(col="#ffb703") +
  tm_layout(title = "Water Supply Main Pipelines\nfrom Melbourne Water Corporation", title.size = 0.5, title.color = "#ffb703", title.position = c('right', 'top'))