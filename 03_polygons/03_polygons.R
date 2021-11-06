#30DayMapChallenge 03 Polygons
#Heritage Conservation Districts in Toronto
#Data source: https://data.melbourne.vic.gov.au/Environment/Tree-Canopies-2021-Urban-Forest-/krg7-hmyt (last updated: Aug 12, 2021)

# load libraries 
library(sf)
library(tmap)

# read Heritage Conservation Districts shp file
heritage = read_sf('data/heritagehcds20210421_webm/Heritage_Conservation_Districts_WGS84.shp')

# read centerline shp file
# source: https://ckan0.cf.opendata.inter.prod-toronto.ca/ne/dataset/toronto-centreline-tcl/resource/6b5415cc-864f-4702-9296-46520f479b0f
toronto_centerline = read_sf('data/centreline_wgs84_v2/CENTRELINE_WGS84.shp')

# plot
p03 = tm_shape(heritage) + tm_fill() + 
  #tm_layout(bg.color = "#d4dddd") +
  tm_shape(toronto_centerline) + tm_lines(col="#dee2e6") +
  tm_shape(heritage) + tm_fill(col="HCD_TYPE", title="",palette = c("#F2A413","#03A696","#1D3D59")) +
  tm_layout(legend.position = c("right","bottom"), legend.title.size = .4, legend.text.size = .4,
            title = "Heritage Conservation\nDistricts in Toronto 2021\nSource: open.toronto.ca", 
            title.size = .4,title.position = c('right', 'bottom'))

p03